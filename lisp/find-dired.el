;;; find-dired.el --- run a `find' command and dired the output

;;; Copyright (C) 1992, 1994, 1995 Free Software Foundation, Inc.

;; Author: Roland McGrath <roland@gnu.ai.mit.edu>,
;;	   Sebastian Kremer <sk@thp.uni-koeln.de>
;; Keywords: unix

(defconst find-dired-version (substring "$Revision: 1.20 $" 11 -2)
  "$Id: find-dired.el,v 1.20 1995/03/16 04:27:11 rms Exp kwzh $")

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
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

;;; Code:

(require 'dired)

;; find's -ls corresponds to these switches.
;; Note -b, at least GNU find quotes spaces etc. in filenames
;;;###autoload
(defvar find-ls-option (if (eq system-type 'berkeley-unix) '("-ls" . "-gilsb")
			 '("-exec ls -ld {} \\;" . "-ld"))
  "*Description of the option to `find' to produce an `ls -l'-type listing.
This is a cons of two strings (FIND-OPTION . LS-SWITCHES).  FIND-OPTION
gives the option (or options) to `find' that produce the desired output.
LS-SWITCHES is a list of `ls' switches to tell dired how to parse the output.")

;;;###autoload
(defvar find-grep-options (if (eq system-type 'berkeley-unix) "-s" "-q")
  "*Option to grep to be as silent as possible.
On Berkeley systems, this is `-s'; on Posix, and with GNU grep, `-q' does it.
On other systems, the closest you can come is to use `-l'.")

(defvar find-args nil
  "Last arguments given to `find' by \\[find-dired].")

;; History of find-args values entered in the minibuffer.
(defvar find-args-history nil)

;;;###autoload
(defun find-dired (dir args)
  "Run `find' and go into dired-mode on a buffer of the output.
The command run (after changing into DIR) is

    find . \\( ARGS \\) -ls"
  (interactive (list (read-file-name "Run find in directory: " nil "" t)
		     (read-string "Run find (with args): " find-args
				  '(find-args-history . 1))))
  ;; Expand DIR ("" means default-directory), and make sure it has a
  ;; trailing slash.
  (setq dir (file-name-as-directory (expand-file-name dir)))
  ;; Check that it's really a directory.
  (or (file-directory-p dir)
      (error "find-dired needs a directory: %s" dir))
  (switch-to-buffer (get-buffer-create "*Find*"))
  (widen)
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq default-directory dir
	find-args args			; save for next interactive call
	args (concat "find . "
		     (if (string= args "")
			 ""
		       (concat "\\( " args " \\) "))
		     (car find-ls-option)))
  ;; The next statement will bomb in classic dired (no optional arg allowed)
  (dired-mode dir (cdr find-ls-option))
  ;; Set subdir-alist so that Tree Dired will work:
  (dired-simple-subdir-alist)
  (setq buffer-read-only nil)
  ;; Subdir headlerline must come first because the first marker in
  ;; subdir-alist points there.
  (insert "  " dir ":\n")
  ;; Make second line a ``find'' line in analogy to the ``total'' or
  ;; ``wildcard'' line. 
  (insert "  " args "\n")
  ;; Start the find process.
  (let ((proc (start-process-shell-command "find" (current-buffer) args)))
    (set-process-filter proc (function find-dired-filter))
    (set-process-sentinel proc (function find-dired-sentinel))
    ;; Initialize the process marker; it is used by the filter.
    (move-marker (process-mark proc) 1 (current-buffer)))
  (setq mode-line-process '(":%s")))

;;;###autoload
(defun find-name-dired (dir pattern)
  "Search DIR recursively for files matching the globbing pattern PATTERN,
and run dired on those files.
PATTERN is a shell wildcard (not an Emacs regexp) and need not be quoted.
The command run (after changing into DIR) is

    find . -name 'PATTERN' -ls"
  (interactive
   "DFind-name (directory): \nsFind-name (filename wildcard): ")
  (find-dired dir (concat "-name '" pattern "'")))

;; This functionality suggested by
;; From: oblanc@watcgl.waterloo.edu (Olivier Blanc)
;; Subject: find-dired, lookfor-dired
;; Date: 10 May 91 17:50:00 GMT
;; Organization: University of Waterloo

(defalias 'lookfor-dired 'find-grep-dired)
;;;###autoload
(defun find-grep-dired (dir args)
  "Find files in DIR containing a regexp ARG and start Dired on output.
The command run (after changing into DIR) is

    find . -exec grep -s ARG {} \\\; -ls

Thus ARG can also contain additional grep options."
  (interactive "DFind-grep (directory): \nsFind-grep (grep regexp): ")
  ;; find -exec doesn't allow shell i/o redirections in the command,
  ;; or we could use `grep -l >/dev/null'
  (find-dired dir
	      (concat "! -type d -exec grep " find-grep-options " "
		      args " {} \\\; ")))

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
		  (forward-line 1))
		;; Convert ` ./FILE' to ` FILE'
		;; This would lose if the current chunk of output
		;; starts or ends within the ` ./', so back up a bit:
		(goto-char (- end 3))	; no error if < 0
		(while (search-forward " ./" nil t)
		  (delete-region (point) (- (point) 2)))
		;; Find all the complete lines in the unprocessed
		;; output and process it to add text properties.
		(goto-char end)
		(if (search-backward "\n" (process-mark proc) t)
		    (progn
		      (dired-insert-set-properties (process-mark proc)
						   (1+ (point)))
		      (move-marker (process-mark proc) (1+ (point)))))
		))))
      ;; The buffer has been killed.
      (delete-process proc))))

(defun find-dired-sentinel (proc state)
  ;; Sentinel for \\[find-dired] processes.
  (let ((buf (process-buffer proc)))
    (if (buffer-name buf)
	(save-excursion
	  (set-buffer buf)
	  (let ((buffer-read-only nil))
	    (save-excursion
	      (goto-char (point-max))
	      (insert "\nfind " state)
	      (forward-char -1)		;Back up before \n at end of STATE.
	      (insert " at " (substring (current-time-string) 0 19))
	      (forward-char 1)
	      (setq mode-line-process
		    (concat ":"
			    (symbol-name (process-status proc))))
	      ;; Since the buffer and mode line will show that the
	      ;; process is dead, we can delete it now.  Otherwise it
	      ;; will stay around until M-x list-processes.
	      (delete-process proc)
	      (force-mode-line-update)))
	  (message "find-dired %s finished." (current-buffer))))))

(provide 'find-dired)

;;; find-dired.el ends here
