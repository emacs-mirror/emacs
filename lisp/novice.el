;;; novice.el --- handling of disabled commands ("novice mode") for Emacs.

;; Copyright (C) 1985, 1986, 1987, 1994 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

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
(setq disabled-command-hook 'disabled-command-hook)

;;;###autoload
(defun disabled-command-hook (&rest ignore)
  (let (char)
    (save-window-excursion
     (with-output-to-temp-buffer "*Help*"
       (if (eq (aref (this-command-keys) 0)
	       (if (stringp (this-command-keys))
		   (aref "\M-x" 0)
		 ?\M-x))
	   (princ "You have invoked the disabled command ")
	 (princ "You have typed ")
	 (princ (key-description (this-command-keys)))
	 (princ ", invoking disabled command "))
       (princ this-command)
       (princ ":\n")
       ;; Print any special message saying why the command is disabled.
       (if (stringp (get this-command 'disabled))
	   (princ (get this-command 'disabled)))
       (princ (or (condition-case ()
		      (documentation this-command)
		    (error nil))
		  "<< not documented >>"))
       ;; Keep only the first paragraph of the documentation.
       (save-excursion
	 (set-buffer "*Help*")
	 (goto-char (point-min))
	 (if (search-forward "\n\n" nil t)
	     (delete-region (1- (point)) (point-max))
	   (goto-char (point-max))))
       (princ "\n\n")
       (princ "You can now type
Space to try the command just this once,
      but leave it disabled,
Y to try it and enable it (no questions if you use it again),
N to do nothing (command remains disabled).")
       (save-excursion
	(set-buffer standard-output)
	(help-mode)))
     (message "Type y, n or Space: ")
     (let ((cursor-in-echo-area t))
       (while (not (memq (setq char (downcase (read-char)))
			 '(?  ?y ?n)))
	 (ding)
	 (message "Please type y, n or Space: "))))
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
  (save-excursion
   (set-buffer (find-file-noselect
		(substitute-in-file-name user-init-file)))
   (goto-char (point-min))
   (if (search-forward (concat "(put '" (symbol-name command) " ") nil t)
       (delete-region
	(progn (beginning-of-line) (point))
	(progn (forward-line 1) (point))))
   ;; Explicitly enable, in case this command is disabled by default
   ;; or in case the code we deleted was actually a comment.
   (goto-char (point-max))
   (insert "\n(put '" (symbol-name command) " 'disabled nil)\n")
   (save-buffer)))

;;;###autoload
(defun disable-command (command)
  "Require special confirmation to execute COMMAND from now on.
The user's .emacs file is altered so that this will apply
to future sessions."
  (interactive "CDisable command: ")
  (if (not (commandp command))
      (error "Invalid command name `%s'" command))
  (put command 'disabled t)
  (save-excursion
   (set-buffer (find-file-noselect
		(substitute-in-file-name user-init-file)))
   (goto-char (point-min))
   (if (search-forward (concat "(put '" (symbol-name command) " ") nil t)
       (delete-region
	(progn (beginning-of-line) (point))
	(progn (forward-line 1) (point))))
   (goto-char (point-max))
   (insert "\n(put '" (symbol-name command) " 'disabled t)\n")
   (save-buffer)))

;;; novice.el ends here
