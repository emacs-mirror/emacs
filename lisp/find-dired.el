;;; find-dired.el -- Run a `find' command and dired the result.
;;; Copyright (C) 1991 Roland McGrath

(defconst find-dired-version "$Id: find-dired.el,v 1.7 1991/06/20 08:50:20 sk RelBeta $")

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to roland@ai.mit.edu) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to roland@gnu.ai.mit.edu.

;;; To use this file, byte-compile it, install it somewhere
;;; in your load-path, and put:
;;;   (autoload 'find-dired "find-dired" nil t)
;;;   (autoload 'lookfor-dired "find-dired" nil t)
;;; in your .emacs, or site-init.el, etc.
;;; To bind it to a key, put, e.g.:
;;;   (global-set-key "\C-cf" 'find-dired)
;;;   (global-set-key "\C-cl" 'lookfor-dired)
;;; in your .emacs.

(require 'dired)

(defvar find-args nil
  "Last arguments given to `find' by \\[find-dired].")

(defvar find-ls-option (if (eq system-type 'berkeley-unix) "-ls"
			 "-exec ls -ldi {} \\;")
  "Option to `find' to produce an `ls -l'-type listing.")

;;;###autoload
(defun find-dired (dir args)
  "Run `find' and go into dired-mode on a buffer of the output.
The command run is \"find . \\( ARGS \\) -ls\" (after changing into DIR)."
  (interactive (list (read-file-name "Run find in directory: " nil "" t)
		     (if (featurep 'gmhist)
			 (read-with-history-in 'find-args-history
					       "Run find (with args): ")
		       (read-string "Run find (with args): " find-args))))
  (if (equal dir "")
      (setq dir default-directory))
  ;; Expand DIR, and make sure it has a trailing slash.
  (setq dir (file-name-as-directory (expand-file-name dir)))
  ;; Check that it's really a directory.
  (or (file-directory-p dir)
      (error "%s is not a directory!" dir))
  (switch-to-buffer (get-buffer-create "*Find*"))
  (widen)
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq default-directory dir
	find-args args
	args (concat "find . " (if (string= args "") ""
				 (concat "\\( " args " \\) ")) find-ls-option))
  (insert "  " args "\n"
	  "  " dir ":\n")
  (set-process-filter (start-process-shell-command "find"
						   (current-buffer) args)
		      'find-dired-filter)
  (set-process-sentinel (get-buffer-process (current-buffer))
			'find-dired-sentinel)
  (dired-mode)
  (setq mode-line-process '(": %s")))

;;;###autoload
(defun find-name-dired (dir pattern)
  "Search DIR recursively for files matching the globbing pattern PATTERN,
and run dired on those files."
  (interactive "DSearch directory: \nsSearch directory %s for: ")
  (find-dired dir (concat "-name '" pattern "'")))

(defun find-dired-filter (proc string)
  ;; Filter for \\[find-dired] processes.
  (let ((buf (process-buffer proc)))
    (if (buffer-name buf)		; not killed?
	(save-excursion
	  (set-buffer buf)
	  (save-restriction
	    (widen)
	    (save-excursion
	      (let ((buffer-read-only nil)
		    (end (point-max)))
		(goto-char end)
		(insert string)
		(goto-char end)
		(or (looking-at "^")
		    (forward-line 1))
		(while (looking-at "^")
		  (insert "  ")
		  (forward-line 1))))))
      ;; The buffer has been killed.
      (delete-process proc))))

(defun find-dired-sentinel (proc state)
  ;; Sentinel for \\[find-dired] processes.
  (let ((buf (process-buffer proc)))
    (if (buffer-name buf)
	(save-excursion
	  (set-buffer buf)
	  (setq mode-line-process nil)
	  (message "find-dired %s finished." (current-buffer))))))

(or (fboundp 'start-process-shell-command)
    ;; From version 19 subr.el.
    (defun start-process-shell-command (name buffer &rest args)
      "Start a program in a subprocess.  Return the process object for it.
Args are NAME BUFFER COMMAND &rest COMMAND-ARGS.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer or (buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is command name, the name of a shell command.
Remaining arguments are the arguments for the command.
Wildcards and redirection are handle as usual in the shell."
      (if (eq system-type 'vax-vms)
	  (apply 'start-process name buffer args)
	(start-process name buffer shell-file-name "-c"
		       (concat "exec " (mapconcat 'identity args " ")))))
    )

;; From: oblanc@watcgl.waterloo.edu (Olivier Blanc)
;; Subject: find-dired, lookfor-dired
;; Date: 10 May 91 17:50:00 GMT
;; Organization: University of Waterloo

;; I added a functiopn to the find-dired.el file:
;; The function is a lookfor-dired and is used to search a string
;; a subtree:

;;;###autoload
(defun lookfor-dired (dir args)
  "Find files in DIR containing a regexp ARG and go into dired-mode on the output.
The command run is

    \"find . -exec grep -l ARG {} \\\; -ls\"

\(after changing into DIR)."
  (interactive (list (read-file-name "Run find in directory: " nil "" t)
		     (read-string "Run find (with args): " find-args)))
  (if (equal dir "")
      (setq dir default-directory))
  ;; Expand DIR, and make sure it has a trailing slash.
  (setq dir (file-name-as-directory (expand-file-name dir)))
  ;; Check that it's really a directory.
  (or (file-directory-p dir)
      (error "%s is not a directory!" dir))
  (switch-to-buffer (get-buffer-create "*Find*"))
  (widen)
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq default-directory dir
	find-args args
	args (concat "find . -exec grep -l " args " {} \\\; -ls"))
  (insert "  " args "\n"
	  "  " dir ":\n")
  (set-process-filter (start-process-shell-command "find"
						   (current-buffer) args)
		      'find-dired-filter)
  (set-process-sentinel (get-buffer-process (current-buffer))
			'find-dired-sentinel)
  (dired-mode)
  (setq mode-line-process '(": %s")))

(provide 'find-dired)
