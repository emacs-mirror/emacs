;;; desktop.el --- save partial status of Emacs when killed

;; Copyright (C) 1993, 1994, 1995, 1997, 2000, 2001
;;   Free Software Foundation, Inc.

;; Author: Morten Welinder <terra@diku.dk>
;; Keywords: convenience
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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

;; To use this, first put these two lines in the bottom of your .emacs
;; file (the later the better):
;;
;;	(desktop-load-default)
;;	(desktop-read)
;;
;; Between these two lines you may wish to add something that updates the
;; variables `desktop-globals-to-save' and/or `desktop-locals-to-save'.  If
;; for instance you want to save the local variable `foobar' for every buffer
;; in which it is local, you could add the line
;;
;;	(setq desktop-locals-to-save (cons 'foobar desktop-locals-to-save))
;;
;; To avoid saving excessive amounts of data you may also wish to add
;; something like the following
;;
;;	(add-hook 'kill-emacs-hook
;;		  '(lambda ()
;;		     (desktop-truncate search-ring 3)
;;		     (desktop-truncate regexp-search-ring 3)))
;;
;; which will make sure that no more than three search items are saved.  You
;; must place this line *after* the `(desktop-load-default)' line.  See also
;; the variable `desktop-save-hook'.

;; Start Emacs in the root directory of your "project". The desktop saver
;; is inactive by default.  You activate it by M-x desktop-save RET.  When
;; you exit the next time the above data will be saved.  This ensures that
;; all the files you were editing will be reloaded the next time you start
;; Emacs from the same directory and that points will be set where you
;; left them.  If you save a desktop file in your home directory it will
;; act as a default desktop when you start Emacs from a directory that
;; doesn't have its own.  I never do this, but you may want to.

;; Some words on minor modes: Most minor modes are controlled by
;; buffer-local variables, which have a standard save / restore
;; mechanism.  To handle all minor modes, we take the following
;; approach: (1) check whether the variable name from
;; `minor-mode-alist' is also a function; and (2) use translation
;; table `desktop-minor-mode-table' in the case where the two names
;; are not the same.

;; By the way: don't use desktop.el to customize Emacs -- the file .emacs
;; in your home directory is used for that.  Saving global default values
;; for buffers is an example of misuse.

;; PLEASE NOTE: The kill ring can be saved as specified by the variable
;; `desktop-globals-to-save' (by default it isn't).  This may result in saving
;; things you did not mean to keep.  Use M-x desktop-clear RET.

;; Thanks to  hetrick@phys.uva.nl (Jim Hetrick)      for useful ideas.
;;            avk@rtsg.mot.com (Andrew V. Klein)     for a dired tip.
;;            chris@tecc.co.uk (Chris Boucher)       for a mark tip.
;;            f89-kam@nada.kth.se (Klas Mellbourn)   for a mh-e tip.
;;            kifer@sbkifer.cs.sunysb.edu (M. Kifer) for a bug hunt.
;;            treese@lcs.mit.edu (Win Treese)        for ange-ftp tips.
;;            pot@cnuce.cnr.it (Francesco Potorti`)  for misc. tips.
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
  ;; We can't (require 'mh-e) since that wants to load something.
  (mapcar 'require '(info dired reporter)))
;; ----------------------------------------------------------------------------
;; USER OPTIONS -- settings you might want to play with.
;; ----------------------------------------------------------------------------

(defgroup desktop nil
  "Save status of Emacs when you exit."
  :group 'frames)

(defcustom desktop-enable nil
  "*Non-nil enable Desktop to save the state of Emacs when you exit."
  :group 'desktop
  :type 'boolean
  :require 'desktop
  :initialize 'custom-initialize-default
  :version "20.3")

(defcustom desktop-basefilename
  (convert-standard-filename ".emacs.desktop")
  "File for Emacs desktop, not including the directory name."
  :type 'file
  :group 'desktop)

(defcustom desktop-missing-file-warning nil
  "*If non-nil then desktop warns when a file no longer exists.
Otherwise it simply ignores that file."
  :type 'boolean
  :group 'desktop)

(defvar desktop-globals-to-save
  (list 'desktop-missing-file-warning
	;; Feature: saving kill-ring implies saving kill-ring-yank-pointer
	;; 'kill-ring
	'tags-file-name
	'tags-table-list
	'search-ring
	'regexp-search-ring
	'register-alist
	;; 'desktop-globals-to-save	; Itself!
	)
  "List of global variables to save when killing Emacs.
An element may be variable name (a symbol)
or a cons cell of the form  (VAR . MAX-SIZE),
which means to truncate VAR's value to at most MAX-SIZE elements
\(if the value is a list) before saving the value.")

(defvar desktop-locals-to-save
  (list 'desktop-locals-to-save		; Itself!  Think it over.
        'truncate-lines
	'case-fold-search
	'case-replace
	'fill-column
	'overwrite-mode
	'change-log-default-name
	'line-number-mode
	)
  "List of local variables to save for each buffer.
The variables are saved only when they really are local.")
(make-variable-buffer-local 'desktop-locals-to-save)

;; We skip .log files because they are normally temporary.
;;         (ftp) files because they require passwords and whatnot.
;;         TAGS files to save time (tags-file-name is saved instead).
(defcustom desktop-buffers-not-to-save
 "\\(^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS\\)$"
 "Regexp identifying buffers that are to be excluded from saving."
 :type 'regexp
 :group 'desktop)

;; Skip ange-ftp files
(defcustom desktop-files-not-to-save
  "^/[^/:]*:"
  "Regexp identifying files whose buffers are to be excluded from saving."
  :type 'regexp
  :group 'desktop)

(defcustom desktop-buffer-modes-to-save
  '(Info-mode rmail-mode)
  "If a buffer is of one of these major modes, save the buffer name.
It is up to the functions in `desktop-buffer-handlers' to decide
whether the buffer should be recreated or not, and how."
  :type '(repeat symbol)
  :group 'desktop)

(defcustom desktop-modes-not-to-save nil
  "List of major modes whose buffers should not be saved."
  :type '(repeat symbol)
  :group 'desktop)

(defcustom desktop-buffer-major-mode nil
  "When desktop creates a buffer, this holds the desired Major mode."
  :type 'symbol
  :group 'desktop)

(defcustom desktop-buffer-file-name nil
  "When desktop creates a buffer, this holds the file name to visit."
  :type '(choice file (const nil))
  :group 'desktop)

(defcustom desktop-buffer-name nil
  "When desktop creates a buffer, this holds the desired buffer name."
  :type '(choice string (const nil))
  :group 'desktop)

(defvar desktop-buffer-misc nil
  "When desktop creates a buffer, this holds a list of misc info.
It is used by the `desktop-buffer-handlers' functions.")

(defcustom desktop-buffer-misc-functions
  '(desktop-buffer-info-misc-data
    desktop-buffer-dired-misc-data)
  "*Functions used to determine auxiliary information for a buffer.
These functions are called in order, with no arguments.  If a function
returns non-nil, its value is saved along with the desktop buffer for
which it was called; no further functions will be called.

Later, when desktop.el restores the buffers it has saved, each of the
`desktop-buffer-handlers' functions will have access to a buffer local
variable, named `desktop-buffer-misc', whose value is what the
\"misc\" function returned previously."
  :type '(repeat function)
  :group 'desktop)

(defcustom desktop-buffer-handlers
  '(desktop-buffer-dired
    desktop-buffer-rmail
    desktop-buffer-mh
    desktop-buffer-info
    desktop-buffer-file)
  "*List of functions to call in order to create a buffer.
The functions are called without explicit parameters but can use the
variables `desktop-buffer-major-mode', `desktop-buffer-file-name',
`desktop-buffer-name'.
If one function returns non-nil, no further functions are called.
If the function returns a buffer, then the saved mode settings
and variable values for that buffer are copied into it."
  :type '(repeat function)
  :group 'desktop)

(put 'desktop-buffer-handlers 'risky-local-variable t)

(defvar desktop-create-buffer-form "(desktop-create-buffer 205"
  "Opening of form for creation of new buffers.")

(defcustom desktop-save-hook nil
  "Hook run before desktop saves the state of Emacs.
This is useful for truncating history lists, for example."
  :type 'hook
  :group 'desktop)

(defcustom desktop-minor-mode-table
  '((auto-fill-function auto-fill-mode)
    (vc-mode nil))
  "Table mapping minor mode variables to minor mode functions.
Each entry has the form (NAME RESTORE-FUNCTION).
NAME is the name of the buffer-local variable indicating that the minor
mode is active.  RESTORE-FUNCTION is the function to activate the minor mode.
called.  RESTORE-FUNCTION nil means don't try to restore the minor mode.
Only minor modes for which the name of the buffer-local variable
and the name of the minor mode function are different have to added to
this table."
  :type 'sexp
  :group 'desktop)

;; ----------------------------------------------------------------------------
(defvar desktop-dirname nil
  "The directory in which the current desktop file resides.")

(defconst desktop-header
";; --------------------------------------------------------------------------
;; Desktop File for Emacs
;; --------------------------------------------------------------------------
" "*Header to place in Desktop file.")

(defvar desktop-delay-hook nil
  "Hooks run after all buffers are loaded; intended for internal use.")

;; ----------------------------------------------------------------------------
(defun desktop-truncate (l n)
  "Truncate LIST to at most N elements destructively."
  (let ((here (nthcdr (1- n) l)))
    (if (consp here)
	(setcdr here nil))))
;; ----------------------------------------------------------------------------
(defcustom desktop-clear-preserve-buffers
  '("*scratch*" "*Messages*")
  "*Buffer names that `desktop-clear' should not delete."
  :type '(repeat string)
  :group 'desktop)

(defun desktop-clear ()
  "Empty the Desktop.
This kills all buffers except for internal ones
and those listed in `desktop-clear-preserve-buffers'."
  (interactive)
  (setq kill-ring nil
	kill-ring-yank-pointer nil
	search-ring nil
	search-ring-yank-pointer nil
	regexp-search-ring nil
	regexp-search-ring-yank-pointer nil)
  (let ((buffers (buffer-list)))
    (while buffers
      (or (member (buffer-name (car buffers)) desktop-clear-preserve-buffers)
	  (null (buffer-name (car buffers)))
	  ;; Don't kill buffers made for internal purposes.
	  (and (not (equal (buffer-name (car buffers)) ""))
	       (eq (aref (buffer-name (car buffers)) 0) ?\ ))
	  (kill-buffer (car buffers)))
      (setq buffers (cdr buffers))))
  (delete-other-windows))
;; ----------------------------------------------------------------------------
(add-hook 'kill-emacs-hook 'desktop-kill)

(defun desktop-kill ()
  (if desktop-dirname
      (condition-case err
	  (desktop-save desktop-dirname)
	(file-error
	 (if (yes-or-no-p "Error while saving the desktop.  Quit anyway? ")
	     nil
	   (signal (car err) (cdr err)))))))
;; ----------------------------------------------------------------------------
(defun desktop-list* (&rest args)
  (if (null (cdr args))
      (car args)
    (setq args (nreverse args))
    (let ((value (cons (nth 1 args) (car args))))
      (setq args (cdr (cdr args)))
      (while args
	(setq value (cons (car args) value))
	(setq args (cdr args)))
      value)))

(defun desktop-internal-v2s (val)
  "Convert VALUE to a pair (QUOTE . TXT); (eval (read TXT)) gives VALUE.
TXT is a string that when read and evaluated yields value.
QUOTE may be `may' (value may be quoted),
`must' (values must be quoted), or nil (value may not be quoted)."
  (cond
   ((or (numberp val) (null val) (eq t val))
    (cons 'may (prin1-to-string val)))
   ((stringp val)
    (let ((copy (copy-sequence val)))
      (set-text-properties 0 (length copy) nil copy)
      ;; Get rid of text properties because we cannot read them
      (cons 'may (prin1-to-string copy))))
   ((symbolp val)
    (cons 'must (prin1-to-string val)))
   ((vectorp val)
    (let* ((special nil)
	   (pass1 (mapcar
		   (lambda (el)
		     (let ((res (desktop-internal-v2s el)))
		       (if (null (car res))
			   (setq special t))
		       res))
		   val)))
      (if special
	  (cons nil (concat "(vector "
			    (mapconcat (lambda (el)
					 (if (eq (car el) 'must)
					     (concat "'" (cdr el))
					   (cdr el)))
				       pass1
				       " ")
			    ")"))
	(cons 'may (concat "[" (mapconcat 'cdr pass1 " ") "]")))))
   ((consp val)
    (let ((p val)
	  newlist
	  use-list*
	  anynil)
      (while (consp p)
	(let ((q.txt (desktop-internal-v2s (car p))))
	  (or anynil (setq anynil (null (car q.txt))))
	  (setq newlist (cons q.txt newlist)))
	(setq p (cdr p)))
      (if p
	  (let ((last (desktop-internal-v2s p))
		(el (car newlist)))
	    (or anynil (setq anynil (null (car last))))
	    (or anynil
		(setq newlist (cons '(must . ".") newlist)))
	    (setq use-list* t)
	    (setq newlist (cons last newlist))))
      (setq newlist (nreverse newlist))
      (if anynil
	  (cons nil
		(concat (if use-list* "(desktop-list* "  "(list ")
			(mapconcat (lambda (el)
				     (if (eq (car el) 'must)
					 (concat "'" (cdr el))
				       (cdr el)))
				   newlist
				   " ")
			")"))
	(cons 'must
	      (concat "(" (mapconcat 'cdr newlist " ") ")")))))
   ((subrp val)
    (cons nil (concat "(symbol-function '"
		      (substring (prin1-to-string val) 7 -1)
		      ")")))
   ((markerp val)
    (let ((pos (prin1-to-string (marker-position val)))
	  (buf (prin1-to-string (buffer-name (marker-buffer val)))))
      (cons nil (concat "(let ((mk (make-marker)))"
			" (add-hook 'desktop-delay-hook"
			" (list 'lambda '() (list 'set-marker mk "
			pos " (get-buffer " buf ")))) mk)"))))
   (t					; save as text
    (cons 'may "\"Unprintable entity\""))))

(defun desktop-value-to-string (val)
  "Convert VALUE to a string that when read evaluates to the same value.
Not all types of values are supported."
  (let* ((print-escape-newlines t)
	 (float-output-format nil)
	 (quote.txt (desktop-internal-v2s val))
	 (quote (car quote.txt))
	 (txt (cdr quote.txt)))
    (if (eq quote 'must)
	(concat "'" txt)
      txt)))
;; ----------------------------------------------------------------------------
(defun desktop-outvar (varspec)
  "Output a setq statement for variable VAR to the desktop file.
The argument VARSPEC may be the variable name VAR (a symbol),
or a cons cell of the form  (VAR . MAX-SIZE),
which means to truncate VAR's value to at most MAX-SIZE elements
\(if the value is a list) before saving the value."
  (let (var size)
    (if (consp varspec)
	(setq var (car varspec) size (cdr varspec))
      (setq var varspec))
    (if (boundp var)
	(progn
	  (if (and (integerp size)
		   (> size 0)
		   (listp (eval var)))
	      (desktop-truncate (eval var) size))
	  (insert "(setq "
		  (symbol-name var)
		  " "
		  (desktop-value-to-string (symbol-value var))
		  ")\n")))))
;; ----------------------------------------------------------------------------
(defun desktop-save-buffer-p (filename bufname mode &rest dummy)
  "Return t if the desktop should record a particular buffer for next startup.
FILENAME is the visited file name, BUFNAME is the buffer name, and
MODE is the major mode."
  (let ((case-fold-search nil))
    (and (not (string-match desktop-buffers-not-to-save bufname))
	 (not (memq mode desktop-modes-not-to-save))
	 (or (and filename
		  (not (string-match desktop-files-not-to-save filename)))
	     (and (eq mode 'dired-mode)
		  (save-excursion
		    (set-buffer (get-buffer bufname))
		    (not (string-match desktop-files-not-to-save
				       default-directory))))
	     (and (null filename)
		  (memq mode desktop-buffer-modes-to-save))))))
;; ----------------------------------------------------------------------------
(defun desktop-save (dirname)
  "Save the Desktop file.  Parameter DIRNAME specifies where to save desktop."
  (interactive "DDirectory to save desktop file in: ")
  (run-hooks 'desktop-save-hook)
  (save-excursion
    (let ((filename (expand-file-name desktop-basefilename dirname))
	  (info (nreverse
		 (mapcar
		  (function
		   (lambda (b)
			      (set-buffer b)
			      (list
			       (buffer-file-name)
			       (buffer-name)
			       major-mode
			       ;; minor modes
			       (let (ret)
				 (mapcar
				  #'(lambda (mim)
				      (and (boundp mim)
					   (symbol-value mim)
					   (setq ret
						 (cons (let ((special (assq mim desktop-minor-mode-table)))
							(if special
							    (cadr special)
							  mim))
						      ret))))
				  (mapcar #'car minor-mode-alist))
				 ret)
			       (point)
			       (list (mark t) mark-active)
			       buffer-read-only
                               (run-hook-with-args-until-success
                                'desktop-buffer-misc-functions)
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

      (insert ";; -*- coding: emacs-mule; -*-\n"
	      desktop-header
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
		   (if (apply 'desktop-save-buffer-p l)
		       (progn
			 (insert desktop-create-buffer-form)
			 (mapcar
			  (function (lambda (e)
				      (insert "\n  "
					      (desktop-value-to-string e))))
			  l)
			 (insert ")\n\n")))))
       info)
      (setq default-directory dirname)
      (if (file-exists-p filename) (delete-file filename))
      (let ((coding-system-for-write 'emacs-mule))
	(write-region (point-min) (point-max) filename nil 'nomessage))))
  (setq desktop-dirname dirname))
;; ----------------------------------------------------------------------------
(defun desktop-remove ()
  "Delete the Desktop file and inactivate the desktop system."
  (interactive)
  (if desktop-dirname
      (let ((filename (concat desktop-dirname desktop-basefilename)))
	(setq desktop-dirname nil)
	(if (file-exists-p filename)
	    (delete-file filename)))))
;; ----------------------------------------------------------------------------
;;;###autoload
(defun desktop-read ()
  "Read the Desktop file and the files it specifies.
This is a no-op when Emacs is running in batch mode."
  (interactive)
  (if noninteractive
      nil
    (let ((dirs '("./" "~/")))
      (while (and dirs
		  (not (file-exists-p (expand-file-name
				       desktop-basefilename
				       (car dirs)))))
	(setq dirs (cdr dirs)))
      (setq desktop-dirname (and dirs (expand-file-name (car dirs))))
      (if desktop-dirname
	  (let ((desktop-last-buffer nil))
	    ;; `load-with-code-conversion' calls `eval-buffer' which
	    ;; contains a `save-excursion', so we end up with the same
	    ;; buffer before and after the load.  This is a problem
	    ;; when the desktop is read initially when Emacs starts up
	    ;; because, if we still are in *scratch* after running
	    ;; `after-init-hook', the splash screen will be displayed.
	    (load (expand-file-name desktop-basefilename desktop-dirname)
		  t t t)
	    (when desktop-last-buffer
	      (switch-to-buffer desktop-last-buffer))
	    (run-hooks 'desktop-delay-hook)
	    (setq desktop-delay-hook nil)
	    (message "Desktop loaded."))
	(desktop-clear)))))
;; ----------------------------------------------------------------------------
;;;###autoload
(defun desktop-load-default ()
  "Load the `default' start-up library manually.
Also inhibit further loading of it.  Call this from your `.emacs' file
to provide correct modes for autoloaded files."
  (if (not inhibit-default-init)	; safety check
      (progn
	(load "default" t t)
	(setq inhibit-default-init t))))
;; ----------------------------------------------------------------------------
;; Note: the following functions use the dynamic variable binding in Lisp.
;;
(defun desktop-buffer-info-misc-data ()
  (if (eq major-mode 'Info-mode)
      (list Info-current-file
            Info-current-node)))

(defun desktop-buffer-dired-misc-data ()
  (if (eq major-mode 'dired-mode)
      (cons
       (expand-file-name dired-directory)
       (cdr
        (nreverse
         (mapcar
          (function car)
          dired-subdir-alist))))))

(defun desktop-buffer-info () "Load an info file."
  (if (eq 'Info-mode desktop-buffer-major-mode)
      (progn
	(let ((first (nth 0 desktop-buffer-misc))
	      (second (nth 1 desktop-buffer-misc)))
	(when (and first second)
	  (require 'info)
	  (Info-find-node first second)
	  (current-buffer))))))
;; ----------------------------------------------------------------------------
(defun desktop-buffer-rmail () "Load an RMAIL file."
  (if (eq 'rmail-mode desktop-buffer-major-mode)
      (condition-case error
	  (progn (rmail-input desktop-buffer-file-name)
                         (if (eq major-mode 'rmail-mode)
                             (current-buffer)
                           rmail-buffer))
	(file-locked
	 (kill-buffer (current-buffer))
	 'ignored))))
;; ----------------------------------------------------------------------------
(defun desktop-buffer-mh () "Load a folder in the mh system."
  (if (eq 'mh-folder-mode desktop-buffer-major-mode)
      (progn
	(require 'mh-e)
	(mh-find-path)
        (mh-visit-folder desktop-buffer-name)
	(current-buffer))))
;; ----------------------------------------------------------------------------
(defun desktop-buffer-dired () "Load a directory using dired."
  (if (eq 'dired-mode desktop-buffer-major-mode)
      (if (file-directory-p (file-name-directory (car desktop-buffer-misc)))
	  (progn
            (dired (car desktop-buffer-misc))
	    (mapcar 'dired-maybe-insert-subdir (cdr desktop-buffer-misc))
	    (current-buffer))
	(message "Directory %s no longer exists." (car desktop-buffer-misc))
	(sit-for 1)
	'ignored)))
;; ----------------------------------------------------------------------------
(defun desktop-buffer-file () "Load a file."
  (if desktop-buffer-file-name
      (if (or (file-exists-p desktop-buffer-file-name)
	      (and desktop-missing-file-warning
		   (y-or-n-p (format
			      "File \"%s\" no longer exists. Re-create? "
			      desktop-buffer-file-name))))
	  (let ((buf (find-file-noselect desktop-buffer-file-name)))
	    (condition-case nil
		(switch-to-buffer buf)
	      (error (pop-to-buffer buf)))
	    buf)
	'ignored)))
;; ----------------------------------------------------------------------------
;; Create a buffer, load its file, set is mode, ...;  called from Desktop file
;; only.

(defvar desktop-last-buffer nil
  "Last buffer read.  Dynamically bound in `desktop-read'.")

(defun desktop-create-buffer (ver desktop-buffer-file-name desktop-buffer-name
				  desktop-buffer-major-mode
				  mim pt mk ro desktop-buffer-misc
				  &optional locals)
  (let ((hlist desktop-buffer-handlers)
	(result)
	(handler))
    (while (and (not result) hlist)
      (setq handler (car hlist))
      (setq result (funcall handler))
      (setq hlist (cdr hlist)))
    (when (bufferp result)
      (setq desktop-last-buffer result)
      (set-buffer result)
      (if (not (equal (buffer-name) desktop-buffer-name))
	  (rename-buffer desktop-buffer-name))
      ;; minor modes
      (cond ((equal '(t) mim)   (auto-fill-mode 1))	; backwards compatible
	    ((equal '(nil) mim) (auto-fill-mode 0))
	    (t (mapcar #'(lambda (minor-mode)
			   (when (functionp minor-mode)
			     (funcall minor-mode 1)))
		       mim)))
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
	(setq locals (cdr locals))))))

;; Backward compatibility -- update parameters to 205 standards.
(defun desktop-buffer (desktop-buffer-file-name desktop-buffer-name
		       desktop-buffer-major-mode
		       mim pt mk ro tl fc cfs cr desktop-buffer-misc)
  (desktop-create-buffer 205 desktop-buffer-file-name desktop-buffer-name
			 desktop-buffer-major-mode (cdr mim) pt mk ro
			 desktop-buffer-misc
			 (list (cons 'truncate-lines tl)
			       (cons 'fill-column fc)
			       (cons 'case-fold-search cfs)
			       (cons 'case-replace cr)
			       (cons 'overwrite-mode (car mim)))))
;; ----------------------------------------------------------------------------

;; If the user set desktop-enable to t with Custom,
;; do the rest of what it takes to use desktop,
;; but do it after finishing loading the init file.
(add-hook 'after-init-hook
	  '(lambda ()
	     (when desktop-enable
	       (desktop-load-default)
	       (desktop-read))))

(provide 'desktop)

;;; desktop.el ends here
