;;; novice.el --- handling of disabled commands ("novice mode") for Emacs

;; Copyright (C) 1985, 1986, 1987, 1994, 2002 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal, help

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

;; This mode provides a hook which is, by default, attached to various
;; putatively dangerous commands in a (probably futile) attempt to
;; prevent lusers from shooting themselves in the feet.

;;; Code:

;; This function is called (by autoloading)
;; to handle any disabled command.
;; The command is found in this-command
;; and the keys are returned by (this-command-keys).

;;;###autoload
(defvar disabled-command-hook 'disabled-command-hook
  "Function to call to handle disabled commands.
If nil, the feature is disabled, i.e., all commands work normally.")

;;;###autoload
(defun disabled-command-hook (&rest ignore)
  (let (char)
    (save-window-excursion
     (with-output-to-temp-buffer "*Help*"
       (let ((keys (this-command-keys)))
	 (if (or (eq (aref keys 0)
		     (if (stringp keys)
			 (aref "\M-x" 0)
		       ?\M-x))
		 (and (>= (length keys) 2)
		      (eq (aref keys 0) meta-prefix-char)
		      (eq (aref keys 1) ?x)))
	     (princ (format "You have invoked the disabled command %s.\n"
			    (symbol-name this-command)))
	   (princ (format "You have typed %s, invoking disabled command %s.\n"
			  (key-description keys) (symbol-name this-command)))))
       ;; Print any special message saying why the command is disabled.
       (if (stringp (get this-command 'disabled))
	   (princ (get this-command 'disabled))
	 (princ "It is disabled because new users often find it confusing.\n")
	 (princ "Here's the first part of its description:\n\n")
	 ;; Keep only the first paragraph of the documentation.
	 (with-current-buffer "*Help*"
	   (goto-char (point-max))
	   (let ((start (point)))
	     (save-excursion
	       (princ (or (condition-case ()
			      (documentation this-command)
			    (error nil))
			  "<< not documented >>")))
	     (if (search-forward "\n\n" nil t)
		 (delete-region (match-beginning 0) (point-max)))
	     (goto-char (point-max))
	     (indent-rigidly start (point) 3))))
       (princ "\n\nDo you want to use this command anyway?\n\n")
       (princ "You can now type
y   to try it and enable it (no questions if you use it again).
n   to cancel--don't try the command, and it remains disabled.
SPC to try the command just this once, but leave it disabled.
!   to try it, and enable all disabled commands for this session only.")
       (save-excursion
	(set-buffer standard-output)
	(help-mode)))
     (message "Type y, n, ! or SPC (the space bar): ")
     (let ((cursor-in-echo-area t))
       (while (not (memq (setq char (downcase (read-char)))
			 '(?! ?  ?y ?n)))
	 (ding)
	 (message "Please type y, n, ! or SPC (the space bar): "))))
    (if (= char ?!)
	(setq disabled-command-hook nil))
    (if (= char ?y)
	(if (and user-init-file
		 (not (string= "" user-init-file))
		 (y-or-n-p "Enable command for future editing sessions also? "))
	    (enable-command this-command)
	  (put this-command 'disabled nil)))
    (if (/= char ?n)
	(call-interactively this-command))))

;;;###autoload
(defun enable-command (command)
  "Allow COMMAND to be executed without special confirmation from now on.
The user's .emacs file is altered so that this will apply
to future sessions."
  (interactive "CEnable command: ")
  (put command 'disabled nil)
  (let ((init-file user-init-file)
	(default-init-file
	  (if (eq system-type 'ms-dos) "~/_emacs" "~/.emacs")))
    (when (null init-file)
      (if (or (file-exists-p default-init-file)
	      (and (eq system-type 'windows-nt)
		   (file-exists-p "~/_emacs")))
	  ;; Started with -q, i.e. the file containing
	  ;; enabled/disabled commands hasn't been read.  Saving
	  ;; settings there would overwrite other settings.
	  (error "Saving settings from \"emacs -q\" would overwrite existing customizations"))
      (setq init-file default-init-file)
      (if (and (not (file-exists-p init-file))
	       (eq system-type 'windows-nt)
	       (file-exists-p "~/_emacs"))
	  (setq init-file "~/_emacs")))
    (save-excursion
      (set-buffer (find-file-noselect
		   (substitute-in-file-name init-file)))
      (goto-char (point-min))
      (if (search-forward (concat "(put '" (symbol-name command) " ") nil t)
	  (delete-region
	   (progn (beginning-of-line) (point))
	   (progn (forward-line 1) (point))))
      ;; Explicitly enable, in case this command is disabled by default
      ;; or in case the code we deleted was actually a comment.
      (goto-char (point-max))
      (insert "\n(put '" (symbol-name command) " 'disabled nil)\n")
      (save-buffer))))

;;;###autoload
(defun disable-command (command)
  "Require special confirmation to execute COMMAND from now on.
The user's .emacs file is altered so that this will apply
to future sessions."
  (interactive "CDisable command: ")
  (if (not (commandp command))
      (error "Invalid command name `%s'" command))
  (put command 'disabled t)
  (let ((init-file user-init-file)
	(default-init-file
	  (if (eq system-type 'ms-dos) "~/_emacs" "~/.emacs")))
    (when (null init-file)
      (if (or (file-exists-p default-init-file)
	      (and (eq system-type 'windows-nt)
		   (file-exists-p "~/_emacs")))
	  ;; Started with -q, i.e. the file containing
	  ;; enabled/disabled commands hasn't been read.  Saving
	  ;; settings there would overwrite other settings.
	  (error "Saving settings from \"emacs -q\" would overwrite existing customizations"))
      (setq init-file default-init-file)
      (if (and (not (file-exists-p init-file))
	       (eq system-type 'windows-nt)
	       (file-exists-p "~/_emacs"))
	  (setq init-file "~/_emacs")))
    (save-excursion
      (set-buffer (find-file-noselect
		   (substitute-in-file-name init-file)))
      (goto-char (point-min))
      (if (search-forward (concat "(put '" (symbol-name command) " ") nil t)
	  (delete-region
	   (progn (beginning-of-line) (point))
	   (progn (forward-line 1) (point))))
      (goto-char (point-max))
      (insert "\n(put '" (symbol-name command) " 'disabled t)\n")
      (save-buffer))))

(provide 'novice)

;;; novice.el ends here
