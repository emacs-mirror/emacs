;;; desktop.el --- save partial status of Emacs when killed

;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Morten Welinder <terra@diku.dk>
;; Version: 2.02

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

;; Save the Desktop, i.e.,
;;	- some global variables
;; 	- the list of buffers with associated files.  For each buffer also
;;		- the major mode
;;		- the default directory
;;		- the point
;;		- the mark & mark-active
;;		- buffer-read-only
;;		- truncate-lines
;;		- case-fold-search
;;		- case-replace
;;		- fill-column

;; To use this, first put these three lines in the bottom of your .emacs
;; file (the later the better):
;;
;;	(load "desktop")
;;	(desktop-load-default)
;;	(desktop-read)
;;

;; Start Emacs in the root directory of your "project". The desktop saver
;; is inactive by default.  You activate it by M-x desktop-save RET.  When
;; you exit the next time the above data will be saved.  This ensures that
;; all the files you were editing will be reloaded the next time you start
;; Emacs from the same directory and that points will be set where you
;; left them.
;;
;; PLEASE NOTE: The kill ring can be saved as specified by the variable
;; `desktop-globals-to-save' (by default it isn't).  This may result in saving
;; things you did not mean to keep.  Use M-x desktop-clear RET.
;;
;; Thanks to  hetrick@phys.uva.nl (Jim Hetrick)   for useful ideas.
;;            avk@rtsg.mot.com (Andrew V. Klein)  for a dired tip.
;;            chris@tecc.co.uk (Chris Boucher)    for a mark tip.
;; ---------------------------------------------------------------------------
;; TODO:
;;
;; Save window configuration.
;; Recognize more minor modes.
;; Save mark rings.
;; Start-up with buffer-menu???

;;; Code:

;; USER OPTIONS -- settings you might want to play with.
;; ----------------------------------------------------------------------------
(defconst desktop-basefilename
  (if (equal system-type 'ms-dos)
      "emacs.dsk" ; Ms-Dos does not support multiple dots in file name
    ".emacs.desktop")
  "File for Emacs desktop.  A directory name will be prepended to this name.")

(defvar desktop-missing-file-warning t
  "*If non-nil then issue warning if a file no longer exists.
Otherwise simply ignore the file.")

(defvar desktop-globals-to-save
  (list 'desktop-missing-file-warning
	;; Feature: saving kill-ring implies saving kill-ring-yank-pointer
	;; 'kill-ring			
	'tags-file-name
	'tags-table-list
	;; 'desktop-globals-to-save	; Itself!
	) 
  "List of global variables to save when killing Emacs.")

;; We skip .log files because they are normally temporary.
;;         (ftp) files because they require passwords and whatsnot.
;;         TAGS files to save time (tags-file-name is saved instead).
(defvar desktop-buffers-not-to-save
 "\\(\\.log\\|(ftp)\\|^tags\\|^TAGS\\)$"
 "Regexp identifying buffers that are to be excluded from saving.")

(defvar desktop-buffer-handlers
  '(desktop-buffer-dired
    desktop-buffer-rmail
    desktop-buffer-info
    desktop-buffer-file)
  "*List of functions to call in order to create a buffer.  The functions are
called without explicit parameters but may access the the major mode as `mam',
the file name as `fn', the buffer name as `bn', the default directory as
`dd'.  If some function returns non-nil no further functions are called.
If the function returns t then the buffer is considered created.")
;; ----------------------------------------------------------------------------
(defvar desktop-dirname nil
  "The directory in which the current desktop file resides.")

(defconst desktop-header
";; --------------------------------------------------------------------------
;; Desktop File for Emacs
;; --------------------------------------------------------------------------
" "*Header to place in Desktop file.")
;; ----------------------------------------------------------------------------
(defconst postv18
  (string-lessp "19" emacs-version)
  "t if Emacs version 19 or later.")

(defun desktop-clear () "Empty the Desktop."
  (interactive)
  (setq kill-ring nil)
  (setq kill-ring-yank-pointer nil)
  (mapcar (function kill-buffer) (buffer-list)))
;; ----------------------------------------------------------------------------
;; This is a bit dirty for version 18 because that version of Emacs was not
;; toilet-trained considering hooks.
(if (not (boundp 'desktop-kill))
    (if postv18
	(add-hook 'kill-emacs-hook 'desktop-kill)
      (setq old-kill-emacs kill-emacs-hook)
      (setq kill-emacs-hook
	    (function (lambda ()
			(progn (desktop-kill)
			       (if (or (null old-kill-emacs)
				       (symbolp old-kill-emacs))
				   (run-hooks old-kill-emacs)
				 (funcall old-kill-emacs))))))))
;; ----------------------------------------------------------------------------
(defun desktop-kill ()
  (if desktop-dirname
      (progn
	(desktop-save desktop-dirname))))
;; ----------------------------------------------------------------------------
(defun desktop-outvar (VAR)
  "Output a setq statement for VAR to the desktop file."
  (if (boundp VAR)
      (progn
	(insert "(setq ")
	(prin1 VAR (current-buffer))
	(insert " '")
	(prin1 (symbol-value VAR) (current-buffer))
	(insert ")\n"))))
;; ----------------------------------------------------------------------------
(defun desktop-save-buffer-p (filename bufname mode)
  "Return t if should record a particular buffer for next startup.
FILENAME is the visited file name, BUFNAME is the buffer name, and
MODE is the major mode."

  (or (and filename
	   (not (string-match desktop-buffers-not-to-save bufname)))
      (and (null filename)
	   (memq mode '(Info-mode dired-mode rmail-mode)))))
;; ----------------------------------------------------------------------------
(defun desktop-save (dirname)
  "Save the Desktop file.  Parameter DIRNAME specifies where to save desktop."
  (interactive "DDirectory to save desktop file in: ")
  (save-excursion
    (let ((filename (expand-file-name
		     (concat dirname desktop-basefilename)))
	  (info (nreverse
		 (mapcar
		  (function (lambda (b)
			      (set-buffer b)
			      (list
			       (buffer-file-name)
			       (buffer-name)
			       (list 'quote major-mode)
			       (list 'quote
				     (list overwrite-mode
					   (not (null
						 (if postv18
						     auto-fill-function
						   auto-fill-hook)))))
			       (point)
			       (if postv18
				   (list 'quote (mark t) mark-active)
				 (mark))
			       buffer-read-only
			       truncate-lines
			       fill-column
			       case-fold-search
			       case-replace
			       (list
				'quote
				(cond ((equal major-mode 'Info-mode)
				       (list Info-current-file
					     Info-current-node))
				      ((equal major-mode 'dired-mode)
				       (if postv18
					   (nreverse
					    (mapcar
					     (function car)
					     dired-subdir-alist))
					 (list default-directory)))
				      ))
			       )))
		  (buffer-list))))
	  (buf (get-buffer-create "*desktop*")))
      (set-buffer buf)
      (erase-buffer)

      (insert desktop-header
	      ";; Created " (current-time-string) "\n"
	      ";; Emacs version " emacs-version "\n\n"
	      ";; Global section:\n")
      (mapcar (function desktop-outvar) desktop-globals-to-save)
      (if (memq 'kill-ring desktop-globals-to-save)
	  (insert "(setq kill-ring-yank-pointer (nthcdr "
		  (int-to-string
		   (- (length kill-ring) (length kill-ring-yank-pointer)))
		  " kill-ring))\n"))

      (insert "\n;; Buffer section:\n")
      (mapcar
       (function (lambda (l)
		   (if (desktop-save-buffer-p
			(car l)
			(nth 1 l)
			(nth 1 (nth 2 l)))
		       (progn
			 (insert "(desktop-buffer")
			 (mapcar
			  (function (lambda (e)
				      (insert "\n  ")
				      (prin1 e (current-buffer))))
			  l)
			 (insert ")\n\n")))))
       info)
      (setq default-directory dirname)
      (if (file-exists-p filename) (delete-file filename))
      (write-region (point-min) (point-max) filename nil 'nomessage)))
  (setq desktop-dirname dirname))
;; ----------------------------------------------------------------------------
(defun desktop-remove ()
  "Delete the Desktop file and inactivate the desktop system."
  (interactive)
  (if desktop-dirname
      (let ((filename (concat desktop-dirname desktop-basefilename)))
	(if (file-exists-p filename) (delete-file filename))
	(setq desktop-dirname nil))))
;; ----------------------------------------------------------------------------
(defun desktop-read ()
  "Read the Desktop file and the files it specifies."
  (interactive)
  (let ((filename))
    (if (file-exists-p (concat "./" desktop-basefilename))
	(setq desktop-dirname (expand-file-name "./"))
      (if (file-exists-p (concat "~/" desktop-basefilename))
	  (setq desktop-dirname (expand-file-name "~/"))
	(setq desktop-dirname nil)))
    (if desktop-dirname
	(progn
	  (load (concat desktop-dirname desktop-basefilename) t t t)
	  (message "Desktop loaded."))
      (desktop-clear))))
;; ----------------------------------------------------------------------------
(defun desktop-load-default ()
  "Load the `default' start-up library manually.  Also inhibit further loading
of it.  Call this from your `.emacs' file to provide correct modes for 
autoloaded files."
  (if (not inhibit-default-init)	; safety check
      (progn
	(load "default" t t)
	(setq inhibit-default-init t))))
;; ----------------------------------------------------------------------------
;; Note: the following functions use the dynamic variable binding in Lisp.
;;       The byte compiler may therefore complain of undeclared variables.
;;
(defun desktop-buffer-info () "Load an info file."
  (if (equal 'Info-mode mam)
      (progn
	(require 'info)
	(Info-find-node (nth 0 misc) (nth 1 misc))
	t)))
;; ----------------------------------------------------------------------------
(defun desktop-buffer-rmail () "Load a RMAIL file."
  (if (equal 'rmail-mode mam)
      (progn (rmail-input fn) t)))
;; ----------------------------------------------------------------------------
(defun desktop-buffer-dired () "Load a directory using dired."
  (if (equal 'dired-mode mam)
      (progn
	(dired (car misc))
	(mapcar (function dired-maybe-insert-subdir) (cdr misc))
	t)))
;; ----------------------------------------------------------------------------
(defun desktop-buffer-file () "Load a file."
  (if fn
      (if (or (file-exists-p fn)
	      (and desktop-missing-file-warning
		   (y-or-n-p (format
			      "File \"%s\" no longer exists. Re-create? "
			      fn))))
	  (progn (find-file fn) t)
	'ignored)))
;; ----------------------------------------------------------------------------
;; Create a buffer, load its file, set is mode, ...;  called from Desktop file
;; only.
(defun desktop-buffer (fn bn mam mim pt mk ro tl fc cfs cr misc)
  (let ((hlist desktop-buffer-handlers)
	(result)
	(handler))
    (while (and (not result) hlist)
      (setq handler (car hlist))
      (setq result (funcall handler))
      (setq hlist (cdr hlist)))
    (if (equal result t)
	(progn
	  (if (not (equal (buffer-name) bn))
	      (rename-buffer bn))
	  (if (nth 0 mim)
	      (overwrite-mode 1)
	    (overwrite-mode 0))
	  (if (nth 1 mim)
	      (auto-fill-mode 1)
	    (overwrite-mode 0))
	  (goto-char pt)
	  (if (consp mk)
	      (progn
		(set-mark (car mk))
		(setq mark-active (car (cdr mk))))
	    (set-mark mk))
	  ;; Never override file system if the file really is read-only marked.
	  (if ro (setq buffer-read-only ro))
	  (setq truncate-lines tl)
	  (setq fill-column fc)
	  (setq case-fold-search cfs)
	  (setq case-replace cr)
	  ))))
;; ----------------------------------------------------------------------------
(provide 'desktop)

;; desktop.el ends here.

