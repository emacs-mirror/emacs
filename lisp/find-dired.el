;;; find-dired.el -- Run a `find' command and dired the result.
;;; Copyright (C) 1991 Free Software Foundation, Inc.
;;; Written by Roland McGrath
;;;
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
      (if (buffer-name buf)
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
  (save-excursion
    (set-buffer (process-buffer proc))
    (setq mode-line-process nil)
    (message "find-dired %s finished." (current-buffer))))

(provide 'find-dired)
