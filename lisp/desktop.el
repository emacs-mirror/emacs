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

;;; Code:

;; Make the compilation more silent
(eval-when-compile
  ;; We use functions from these modules
  ;; We can't (require 'mh-e) since that wants to load something.
  (mapcar 'require '(info dired reporter)))

(defvar desktop-file-version "206"
  "Verion number of desktop file format.
Written into the desktop file and used at desktop read to provide
backward compatibility.")

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

(defcustom desktop-save 'ask-if-new
  "*When the user changes desktop or quits emacs, should the desktop be saved?
\(in the current desktop directory)
   t             -- always save.
   ask           -- always ask.
   ask-if-new    -- ask if no desktop file exists, otherwise just save.
   ask-if-exists -- ask if desktop file exists, otherwise don't save.
   if-exists     -- save if desktop file exists, otherwise don't save.
   nil           -- never save.
The desktop is never saved when `desktop-enable' is nil."
  :type '(choice
    (const :tag "Always save" t)
    (const :tag "Always ask" ask)
    (const :tag "Ask if desktop file is new, else do save" ask-if-new)
    (const :tag "Ask if desktop file exists, else don't save" ask-if-exists)
    (const :tag "Save if desktop file exists, else don't" if-exists)
    (const :tag "Never save" nil))
  :group 'desktop)

(defcustom desktop-base-file-name
  (convert-standard-filename ".emacs.desktop")
  "File for Emacs desktop, not including the directory name."
  :type 'file
  :group 'desktop)
(defvaralias 'desktop-basefilename 'desktop-base-file-name)

(defcustom desktop-path '("." "~")
  "List of directories to search for the desktop file.
The base name of the file is specified in `desktop-base-file-name'."
  :type '(repeat directory)
  :group 'desktop)

(defcustom desktop-missing-file-warning nil
  "*If non-nil then desktop warns when a file no longer exists.
Otherwise it simply ignores that file."
  :type 'boolean
  :group 'desktop)

(defcustom desktop-no-desktop-file-hook nil
  "Normal hook run after fail of `desktop-read' due to missing desktop file.
May e.g. be used to show a dired buffer."
  :type 'hook
  :group 'desktop)

(defcustom desktop-after-read-hook nil
  "Normal hook run after a sucessful `desktop-read'.
May e.g. be used to show a buffer list."
  :type 'hook
  :group 'desktop)

(defcustom desktop-save-hook nil
  "Hook run before desktop saves the state of Emacs.
This is useful for truncating history lists, for example."
  :type 'hook
  :group 'desktop)

(defcustom desktop-globals-to-save '(
  desktop-missing-file-warning
  tags-file-name
  tags-table-list
  search-ring
  regexp-search-ring
  register-alist)
  "List of global variables to save when killing Emacs.
An element may be variable name (a symbol)
or a cons cell of the form  (VAR . MAX-SIZE),
which means to truncate VAR's value to at most MAX-SIZE elements
\(if the value is a list) before saving the value.
Feature: Saving `kill-ring' implies saving `kill-ring-yank-pointer'."
  :type '(repeat (restricted-sexp :match-alternatives (symbolp consp)))
  :group 'desktop)

(defcustom desktop-globals-to-clear '(
  kill-ring
  kill-ring-yank-pointer
  search-ring
  search-ring-yank-pointer
  regexp-search-ring
  regexp-search-ring-yank-pointer)
  "List of global variables set to clear by `desktop-clear'.
An element may be variable name (a symbol) or a cons cell of the form
\(VAR . FORM). Symbols are set to nil and for cons cells VAR is set
to the value obtained by evaluateing FORM."
  :type '(repeat (restricted-sexp :match-alternatives (symbolp consp)))
  :group 'desktop)

(defcustom desktop-clear-preserve-buffers-regexp
  "^\\*tramp/.+\\*$"
  "Regexp identifying buffers that `desktop-clear' should not delete."
  :type 'regexp
  :group 'desktop)

;; Maintained for backward compatibility
(defcustom desktop-clear-preserve-buffers
  '("*scratch*" "*Messages*")
  "*List of buffer names that `desktop-clear' should not delete."
  :type '(repeat string)
  :group 'desktop)

(defvar desktop-locals-to-save '(
  desktop-locals-to-save  ; Itself!  Think it over.
  truncate-lines
  case-fold-search
  case-replace
  fill-column
  overwrite-mode
  change-log-default-name
  line-number-mode)
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

(defcustom desktop-file-name-format 'absolute
  "*Format in which desktop file names should be saved.
Possible values are:
   absolute -- Absolute file name.
   tilde    -- Relative to ~.
   local    -- Relative to directory of desktop file."
  :type '(choice (const absolute) (const tilde) (const local))
  :group 'desktop)

(defcustom desktop-buffer-misc-functions
  '(desktop-buffer-info-misc-data
    desktop-buffer-dired-misc-data)
  "*Functions used to determine auxiliary information for a buffer.
These functions are called in order, with no arguments.  If a function
returns non-nil, its value is saved along with the desktop buffer for
which it was called; no further functions will be called.

File names should formatted using the call
\"(desktop-file-name FILE-NAME dirname)\".

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
following variables:

   desktop-file-version
   desktop-buffer-file-name
   desktop-buffer-name
   desktop-buffer-major-mode
   desktop-buffer-minor-modes
   desktop-buffer-point
   desktop-buffer-mark
   desktop-buffer-read-only
   desktop-buffer-misc
   desktop-buffer-locals

If one function returns non-nil, no further functions are called.
If the function returns a buffer, then the saved mode settings
and variable values for that buffer are copied into it."
  :type '(repeat function)
  :group 'desktop)

(put 'desktop-buffer-handlers 'risky-local-variable t)

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
(defun desktop-clear ()
  "Empty the Desktop.
This kills all buffers except for internal ones and those listed
in `desktop-clear-preserve-buffers'.  Furthermore, it clears the
variables listed in `desktop-globals-to-clear'."
  (interactive)
  (dolist (var desktop-globals-to-clear)
    (if (symbolp var)
      (eval `(setq-default ,var nil))
      (eval `(setq-default ,(car var) ,(cdr var)))))
  (let ((buffers (buffer-list)))
    (while buffers
      (let ((bufname (buffer-name (car buffers))))
         (or
           (null bufname)
           (string-match desktop-clear-preserve-buffers-regexp bufname)
           (member bufname desktop-clear-preserve-buffers)
           ;; Don't kill buffers made for internal purposes.
           (and (not (equal bufname "")) (eq (aref bufname 0) ?\ ))
           (kill-buffer (car buffers))))
      (setq buffers (cdr buffers))))
  (delete-other-windows))

;; ----------------------------------------------------------------------------
(add-hook 'kill-emacs-hook 'desktop-kill)

(defun desktop-kill ()
  "If `desktop-enable' is non-nil, do what `desktop-save' says to do.
If the desktop should be saved and `desktop-dirname'
is nil, ask the user where to save the desktop."
  (when
    (and
      desktop-enable
      (let ((exists (file-exists-p (concat desktop-dirname desktop-base-file-name))))
        (or
          (eq desktop-save 't)
          (and exists (memq desktop-save '(ask-if-new if-exists)))
          (and
            (or
              (memq desktop-save '(ask ask-if-new))
              (and exists (eq desktop-save 'ask-if-exists)))
            (y-or-n-p "Save desktop? ")))))
    (unless desktop-dirname
      (setq desktop-dirname
        (expand-file-name
          (call-interactively
            (lambda (dir) (interactive "DDirectory for desktop file: ") dir)))))
    (condition-case err
      (desktop-save desktop-dirname)
      (file-error
        (unless (yes-or-no-p "Error while saving the desktop. Ignore? ")
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

;; ----------------------------------------------------------------------------
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

;; ----------------------------------------------------------------------------
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
(defun desktop-file-name (filename dirname)
  "Convert FILENAME to format specified in `desktop-file-name-format'.
DIRNAME must be the directory in which the desktop file will be saved."
  (cond
    ((not filename) nil)
    ((eq desktop-file-name-format 'tilde)
     (let ((relative-name (file-relative-name (expand-file-name filename) "~")))
       (cond
         ((file-name-absolute-p relative-name) relative-name)
         ((string= "./" relative-name) "~/")
         ((string= "." relative-name) "~")
         (t (concat "~/" relative-name)))))
    ((eq desktop-file-name-format 'local) (file-relative-name filename dirname))
    (t (expand-file-name filename))))

;; ----------------------------------------------------------------------------
(defun desktop-save (dirname)
  "Save the Desktop file. Parameter DIRNAME specifies where to save desktop."
  (interactive "DDirectory to save desktop file in: ")
  (run-hooks 'desktop-save-hook)
  (setq dirname (file-name-as-directory (expand-file-name dirname)))
  (save-excursion
    (let ((filename (concat dirname desktop-base-file-name))
          (info
            (mapcar
              (function
                (lambda (b)
                  (set-buffer b)
                  (list
                    (desktop-file-name (buffer-file-name) dirname)
                    (buffer-name)
                    major-mode
                    ;; minor modes
                    (let (ret)
                      (mapcar
                        #'(lambda (mim)
                          (and
                            (boundp mim)
                            (symbol-value mim)
                            (setq ret
                              (cons
                                (let ((special (assq mim desktop-minor-mode-table)))
                                  (if special (cadr special) mim))
                                ret))))
                        (mapcar #'car minor-mode-alist))
                      ret)
                    (point)
                    (list (mark t) mark-active)
                    buffer-read-only
                    (run-hook-with-args-until-success 'desktop-buffer-misc-functions)
                    (let ((locals desktop-locals-to-save)
                          (loclist (buffer-local-variables))
                          (ll))
                      (while locals
                        (let ((here (assq (car locals) loclist)))
                          (if here
                            (setq ll (cons here ll))
                            (when (member (car locals) loclist)
                              (setq ll (cons (car locals) ll)))))
                        (setq locals (cdr locals)))
                      ll))))
              (buffer-list)))
          (buf (get-buffer-create "*desktop*")))
      (set-buffer buf)
      (erase-buffer)

      (insert
        ";; -*- coding: emacs-mule; -*-\n"
        desktop-header
        ";; Created " (current-time-string) "\n"
        ";; Desktop file format version " desktop-file-version "\n"
        ";; Emacs version " emacs-version "\n\n"
        ";; Global section:\n")
      (mapcar (function desktop-outvar) desktop-globals-to-save)
      (if (memq 'kill-ring desktop-globals-to-save)
        (insert
          "(setq kill-ring-yank-pointer (nthcdr "
          (int-to-string (- (length kill-ring) (length kill-ring-yank-pointer)))
          " kill-ring))\n"))

      (insert "\n;; Buffer section -- buffers listed in same order as in buffer list:\n")
      (mapcar
        (function
          (lambda (l)
            (if (apply 'desktop-save-buffer-p l)
              (progn
                (insert "(desktop-create-buffer " desktop-file-version)
                (mapcar
                  (function
                    (lambda (e)
                      (insert "\n  " (desktop-value-to-string e))))
                  l)
                (insert ")\n\n")))))
        info)
      (setq default-directory dirname)
      (when (file-exists-p filename) (delete-file filename))
      (let ((coding-system-for-write 'emacs-mule))
        (write-region (point-min) (point-max) filename nil 'nomessage))))
  (setq desktop-dirname dirname))

;; ----------------------------------------------------------------------------
(defun desktop-remove ()
  "Delete the Desktop file and inactivate the desktop system."
  (interactive)
  (if desktop-dirname
      (let ((filename (concat desktop-dirname desktop-base-file-name)))
	(setq desktop-dirname nil)
	(if (file-exists-p filename)
	    (delete-file filename)))))

;; ----------------------------------------------------------------------------
;;;###autoload
(defun desktop-read ()
  "Read the Desktop file and the files it specifies.
This is a no-op when Emacs is running in batch mode.
Look for the desktop file according to the variables `desktop-base-file-name'
and `desktop-path'.  If no desktop file is found, clear the desktop.
Returns t if it has read a desktop file, nil otherwise."
  (interactive)
  (unless noninteractive
    (let ((dirs desktop-path))
      (while
        (and
          dirs
          (not
            (file-exists-p (expand-file-name desktop-base-file-name (car dirs)))))
        (setq dirs (cdr dirs)))
      (setq desktop-dirname (and dirs (expand-file-name (car dirs))))
      (if desktop-dirname
        (let ((desktop-first-buffer nil))
          ;; Evaluate desktop buffer.
          (load (expand-file-name desktop-base-file-name desktop-dirname) t t t)
          ;; `desktop-create-buffer' puts buffers at end of the buffer list.
          ;; We want buffers existing prior to evaluating the desktop (and not reused)
          ;; to be placed at the end of the buffer list, so we move them here.
          (mapcar 'bury-buffer
                  (nreverse (cdr (memq desktop-first-buffer (nreverse (buffer-list))))))
          (switch-to-buffer (car (buffer-list)))
          (run-hooks 'desktop-delay-hook)
          (setq desktop-delay-hook nil)
          (run-hooks 'desktop-after-read-hook)
          (message "Desktop loaded.")
          t)
        (desktop-clear)
        (run-hooks 'desktop-no-desktop-file-hook)
        (message "No desktop file.")
        nil))))

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
;;;###autoload
(defun desktop-change-dir (dir)
  "Save and clear the desktop, then load the desktop from directory DIR.
However, if `desktop-enable' was nil at call, don't save the old desktop.
This function always sets `desktop-enable' to t."
  (interactive "DNew directory: ")
  (desktop-kill)
  (desktop-clear)
  (cd dir)
  (setq desktop-enable t)
  (let ((desktop-path '(".")))
    (desktop-read)
    ;; Set `desktop-dirname' even in no desktop file was found
    (setq desktop-dirname (expand-file-name dir))))

;; ----------------------------------------------------------------------------
;;;###autoload
(defun desktop-save-in-load-dir ()
  "Save desktop in directory from which it was loaded."
  (interactive)
  (if desktop-dirname
    (desktop-save desktop-dirname)
    (call-interactively 'desktop-save))
  (message "Desktop saved in %s" desktop-dirname))

;; ----------------------------------------------------------------------------
;;;###autoload
(defun desktop-revert ()
  "Revert to the last loaded desktop."
  (interactive)
  (unless desktop-dirname (error "No desktop has been loaded"))
  (setq desktop-enable nil)
  (desktop-change-dir desktop-dirname))

;; ----------------------------------------------------------------------------
;; Note: the following functions use the dynamic variable binding in Lisp.
;;

(eval-when-compile ; Just to silence the byte compiler
  (defvar desktop-file-version)
  (defvar desktop-buffer-file-name)
  (defvar desktop-buffer-name)
  (defvar desktop-buffer-major-mode)
  (defvar desktop-buffer-minor-modes)
  (defvar desktop-buffer-point)
  (defvar desktop-buffer-mark)
  (defvar desktop-buffer-read-only)
  (defvar desktop-buffer-misc)
  (defvar desktop-buffer-locals)
)

(defun desktop-buffer-info-misc-data ()
  (if (eq major-mode 'Info-mode)
      (list Info-current-file
            Info-current-node)))

;; ----------------------------------------------------------------------------
(defun desktop-buffer-dired-misc-data ()
  (when (eq major-mode 'dired-mode)
    (eval-when-compile (defvar dirname))
    (cons
      ;; dired directory in portable form
      (file-name-as-directory (desktop-file-name dired-directory dirname))
      (cdr (nreverse (mapcar (function car) dired-subdir-alist))))))

;; ----------------------------------------------------------------------------
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
(eval-when-compile (defvar rmail-buffer)) ; Just to silence the byte compiler.
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
(defun desktop-buffer-file ()
  "Load a file."
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
	    (and (not (eq major-mode desktop-buffer-major-mode))
		 (functionp desktop-buffer-major-mode)
		 (funcall desktop-buffer-major-mode))
	    buf)
	'ignored)))

;; ----------------------------------------------------------------------------
;; Create a buffer, load its file, set is mode, ...;  called from Desktop file
;; only.

(eval-when-compile ; Just to silence the byte compiler
   (defvar desktop-first-buffer) ;; Dynamically bound in `desktop-read'
)

(defun desktop-create-buffer (
  desktop-file-version
  desktop-buffer-file-name
  desktop-buffer-name
  desktop-buffer-major-mode
  desktop-buffer-minor-modes
  desktop-buffer-point
  desktop-buffer-mark
  desktop-buffer-read-only
  desktop-buffer-misc
  &optional
  desktop-buffer-locals)
  ;; To make desktop files with relative file names possible, we cannot
  ;; allow `default-directory' to change. Therefore we save current buffer.
  (save-current-buffer
    (let (
      (buffer-list (buffer-list))
      (hlist desktop-buffer-handlers)
      (result)
      (handler)
    )
      ;; Call desktop-buffer-handlers to create buffer.
      (while (and (not result) hlist)
        (setq handler (car hlist))
        (setq result (funcall handler))
        (setq hlist (cdr hlist)))
      (unless (bufferp result) (setq result nil))
      ;; Restore buffer list order with new buffer at end. Don't change
      ;; the order for old desktop files (old desktop module behaviour).
      (unless (< desktop-file-version 206)
        (mapcar 'bury-buffer buffer-list)
        (when result (bury-buffer result)))
      (when result
        (unless (or desktop-first-buffer (< desktop-file-version 206))
          (setq desktop-first-buffer result))
        (set-buffer result)
        (unless (equal (buffer-name) desktop-buffer-name)
          (rename-buffer desktop-buffer-name))
        ;; minor modes
        (cond (
          ;; backwards compatible
          (equal '(t) desktop-buffer-minor-modes)
          (auto-fill-mode 1))(
          (equal '(nil) desktop-buffer-minor-modes)
          (auto-fill-mode 0))(
          t
          (mapcar
            #'(lambda (minor-mode)
              (when (functionp minor-mode) (funcall minor-mode 1)))
            desktop-buffer-minor-modes)))
        ;; Even though point and mark are non-nil when written by `desktop-save'
        ;; they may be modified by mandlers wanting to set point or mark themselves.
        (when desktop-buffer-point (goto-char desktop-buffer-point))
        (when desktop-buffer-mark
          (if (consp desktop-buffer-mark)
            (progn
              (set-mark (car desktop-buffer-mark))
              (setq mark-active (car (cdr desktop-buffer-mark))))
            (set-mark desktop-buffer-mark)))
        ;; Never override file system if the file really is read-only marked.
        (if desktop-buffer-read-only (setq buffer-read-only desktop-buffer-read-only))
        (while desktop-buffer-locals
          (let ((this (car desktop-buffer-locals)))
            (if (consp this)
              ;; an entry of this form `(symbol . value)'
              (progn
                (make-local-variable (car this))
                (set (car this) (cdr this)))
              ;; an entry of the form `symbol'
              (make-local-variable this)
              (makunbound this)))
          (setq desktop-buffer-locals (cdr desktop-buffer-locals)))))))

;; ----------------------------------------------------------------------------
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
;; When `desktop-enable' is non-nil and "--no-desktop" is not specified on the
;; command line, we do the rest of what it takes to use desktop, but do it
;; after finishing loading the init file.
;; We cannot use `command-switch-alist' to process "--no-desktop" because these
;; functions are processed after `after-init-hook'.
(add-hook
  'after-init-hook
  '(lambda ()
    (let ((key "--no-desktop"))
      (if (member key command-line-args)
        (delete key command-line-args)
        (when desktop-enable
          (desktop-load-default)
          (desktop-read))))))

(provide 'desktop)

;;; desktop.el ends here
