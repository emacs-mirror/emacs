;;; desktop.el --- save partial status of Emacs when killed

;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Morten Welinder <terra@diku.dk>
;; Version: 2.05
;; Keywords: customization
;; Favourite-brand-of-beer: None, I hate beer.

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
;;		- some local variables

;; To use this, first put these three lines in the bottom of your .emacs
;; file (the later the better):
;;
;;	(load "desktop")
;;	(desktop-load-default)
;;	(desktop-read)
;;
;; Between the second and the third line you may wish to add something that
;; updates the variables `desktop-globals-to-save' and/or 
;; `desktop-locals-to-save'.  If for instance you want to save the local
;; variable `foobar' for every buffer in which it is local, you could add
;; the line
;;
;;	(setq desktop-locals-to-save (cons 'foobar desktop-locals-to-save))
;;
;; To avoid saving excessive amounts of data you may also with to add 
;; something like the following
;;
;;	(add-hook 'kill-emacs-hook
;;		  '(lambda () 
;;		     (desktop-truncate search-ring 3)
;;		     (desktop-truncate regexp-search-ring 3)))
;;
;; which will make sure that no more than three search items are saved.  You
;; must place this line *after* the (load "desktop") line.

;; Start Emacs in the root directory of your "project". The desktop saver
;; is inactive by default.  You activate it by M-x desktop-save RET.  When
;; you exit the next time the above data will be saved.  This ensures that
;; all the files you were editing will be reloaded the next time you start
;; Emacs from the same directory and that points will be set where you
;; left them.  If you save a desktop file in your home directory it will
;; act as a default desktop when you start Emacs from a directory that 
;; doesn't have its own.  I never do this, but you may want to.

;; By the way: don't use desktop.el to customize Emacs -- the file .emacs
;; in your home directory is used for that.  Saving global default values
;; for buffers is an example of misuse.

;; PLEASE NOTE: The kill ring can be saved as specified by the variable
;; `desktop-globals-to-save' (by default it isn't).  This may result in saving
;; things you did not mean to keep.  Use M-x desktop-clear RET.

;; Thanks to  hetrick@phys.uva.nl (Jim Hetrick)     for useful ideas.
;;            avk@rtsg.mot.com (Andrew V. Klein)    for a dired tip.
;;            chris@tecc.co.uk (Chris Boucher)      for a mark tip.
;;            f89-kam@nada.kth.se (Klas Mellbourn)  for a mh-e tip.
;; ---------------------------------------------------------------------------
;; TODO:
;;
;; Save window configuration.
;; Recognize more minor modes.
;; Save mark rings.
;; Start-up with buffer-menu???

;;; Code:

;; Make the compilation more silent
(eval-when-compile
  ;; We use functions from these modules
  (mapcar 'require '(info mh-e dired))
  ;; We handle auto-fill-hook in a way that is ok.
  (put 'auto-fill-hook 'byte-obsolete-variable nil)
  ;; Some things are different in version 18.
  (setq postv18 (string-lessp "19" emacs-version)))
;; ----------------------------------------------------------------------------
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
	'search-ring
	'regexp-search-ring
	;; 'desktop-globals-to-save	; Itself!
	)
  "List of global variables to save when killing Emacs.")

(defvar desktop-locals-to-save
  (list 'desktop-locals-to-save		; Itself!  Think it over.
        'truncate-lines
	'case-fold-search
	'case-replace
	'fill-column
	'overwrite-mode
	'change-log-default-name
	)
  "List of local variables to save for each buffer.  The variables are saved
only when they really are local.")

;; We skip .log files because they are normally temporary.
;;         (ftp) files because they require passwords and whatsnot.
;;         TAGS files to save time (tags-file-name is saved instead).
(defvar desktop-buffers-not-to-save
 "\\(\\.log\\|(ftp)\\|^tags\\|^TAGS\\)$"
 "Regexp identifying buffers that are to be excluded from saving.")

(defvar desktop-buffer-handlers
  '(desktop-buffer-dired
    desktop-buffer-rmail
    desktop-buffer-mh
    desktop-buffer-info
    desktop-buffer-file)
  "*List of functions to call in order to create a buffer.  The functions are
called without explicit parameters but may access the the major mode as `mam',
the file name as `fn', the buffer name as `bn', the default directory as
`dd'.  If some function returns non-nil no further functions are called.
If the function returns t then the buffer is considered created.")

(defvar desktop-create-buffer-form "(desktop-create-buffer 205"
  "Opening of form for creation of new buffers.")
;; ----------------------------------------------------------------------------
(defvar desktop-dirname nil
  "The directory in which the current desktop file resides.")

(defconst desktop-header
";; --------------------------------------------------------------------------
;; Desktop File for Emacs
;; --------------------------------------------------------------------------
" "*Header to place in Desktop file.")
;; ----------------------------------------------------------------------------
(defun desktop-truncate (l n)
  "Truncate LIST to at most N elements destructively."
  (let ((here (nthcdr (1- n) l)))
    (if (consp here)
	(setcdr here nil))))		  
;; ----------------------------------------------------------------------------
(defun desktop-clear () "Empty the Desktop."
  (interactive)
  (setq kill-ring nil)
  (setq kill-ring-yank-pointer nil)
  (mapcar (function kill-buffer) (buffer-list))
  (delete-other-windows))
;; ----------------------------------------------------------------------------
;; This is a bit dirty for version 18 because that version of Emacs was not
;; toilet-trained considering hooks.
(defvar old-kill-emacs)

(if (eval-when-compile postv18)
    (add-hook 'kill-emacs-hook 'desktop-kill)
  (if (not (boundp 'desktop-kill))
      (setq old-kill-emacs kill-emacs-hook
	    kill-emacs-hook
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
(defun desktop-value-to-string (val)
  (let ((print-escape-newlines t))
    (concat
     ;; symbols are needed for cons cells and for symbols except
     ;; `t' and `nil'.
     (if (or (consp val)
	     (and (symbolp val) val (not (eq t val))))
	 "'"
       "")
     (prin1-to-string val))))
;; ----------------------------------------------------------------------------
(defun desktop-outvar (var)
  "Output a setq statement for VAR to the desktop file."
  (if (boundp var)
      (insert "(setq "
	      (symbol-name var)
	      " "
	      (desktop-value-to-string (symbol-value var))
	      ")\n")))
;; ----------------------------------------------------------------------------
(defun desktop-save-buffer-p (filename bufname mode &rest dummy)
  "Return t if the desktop should record a particular buffer for next startup.
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
			       major-mode
			       (list	; list explaining minor modes
				     (not (null
					   (if (eval-when-compile postv18)
					       auto-fill-function
					     auto-fill-hook))))
			       (point)
			       (if (eval-when-compile postv18)
				   (list (mark t) mark-active)
				 (mark))
			       buffer-read-only
			       (cond ((eq major-mode 'Info-mode)
				      (list Info-current-file
					    Info-current-node))
				     ((eq major-mode 'dired-mode)
				      (if (eval-when-compile postv18)
					  (nreverse
					   (mapcar
					    (function car)
					    dired-subdir-alist))
					(list default-directory)))
				     )
			       (let ((locals desktop-locals-to-save)
				     (loclist (buffer-local-variables))
				     (ll))
				 (while locals
				   (let ((here (assq (car locals) loclist)))
				     (if here
					 (setq ll (cons here ll))
				       (if (member (car locals) loclist)
					   (setq ll (cons (car locals) ll)))))
				   (setq locals (cdr locals)))
				 ll)
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
      (let ((print-escape-newlines t))
	(mapcar
	 (function (lambda (l)
		     (if (apply 'desktop-save-buffer-p l)
			 (progn
			   (insert desktop-create-buffer-form)
			   (mapcar
			    (function (lambda (e)
					(insert "\n  "
						(desktop-value-to-string e))))
			    l)
			   (insert ")\n\n")))))
	 info))
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
;;
(defun desktop-buffer-info () "Load an info file."
  (if (eq 'Info-mode mam)
      (progn
	(require 'info)
	(Info-find-node (nth 0 misc) (nth 1 misc))
	t)))
;; ----------------------------------------------------------------------------
(defun desktop-buffer-rmail () "Load an RMAIL file."
  (if (eq 'rmail-mode mam)
      (progn (rmail-input fn) t)))
;; ----------------------------------------------------------------------------
(defun desktop-buffer-mh () "Load a folder in the mh system."
  (if (eq 'mh-folder-mode mam)
      (progn
	(require 'mh-e)
	(mh-find-path)
	(mh-visit-folder bn)
	t)))
;; ----------------------------------------------------------------------------
(defun desktop-buffer-dired () "Load a directory using dired."
  (if (eq 'dired-mode mam)
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
(defun desktop-create-buffer (ver fn bn mam mim pt mk ro misc &optional locals)
  (let ((hlist desktop-buffer-handlers)
	(result)
	(handler))
    (while (and (not result) hlist)
      (setq handler (car hlist))
      (setq result (funcall handler))
      (setq hlist (cdr hlist)))
    (if (eq result t)
	(progn
	  (if (not (equal (buffer-name) bn))
	      (rename-buffer bn))
	  (auto-fill-mode (if (nth 0 mim) 1 0))
	  (goto-char pt)
	  (if (consp mk)
	      (progn
		(set-mark (car mk))
		(setq mark-active (car (cdr mk))))
	    (set-mark mk))
	  ;; Never override file system if the file really is read-only marked.
	  (if ro (setq buffer-read-only ro))
	  (while locals
	    (let ((this (car locals)))
	      (if (consp this)
		  ;; an entry of this form `(symbol . value)'
		  (progn
		    (make-local-variable (car this))
		    (set (car this) (cdr this)))
		;; an entry of the form `symbol'
		(make-local-variable this)
		(makunbound this)))
	    (setq locals (cdr locals)))
	  ))))

;; Backward compatibility -- update parameters to 205 standards.
(defun desktop-buffer (fn bn mam mim pt mk ro tl fc cfs cr misc)
  (desktop-create-buffer 205 fn bn mam (cdr mim) pt mk ro misc
			 (list (cons 'truncate-lines tl)
			       (cons 'fill-column fc)
			       (cons 'case-fold-search cfs)
			       (cons 'case-replace cr)
			       (cons 'overwrite-mode (car mim)))))
;; ----------------------------------------------------------------------------
(provide 'desktop)

;; desktop.el ends here.
