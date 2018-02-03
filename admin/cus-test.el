;;; cus-test.el --- tests for custom types and load problems

;; Copyright (C) 1998, 2000, 2002-2018 Free Software Foundation, Inc.

;; Author: Markus Rost <markus.rost@mathematik.uni-regensburg.de>
;; Maintainer: Markus Rost <rost@math.ohio-state.edu>
;; Created: 13 Sep 1998
;; Keywords: maint

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides simple tests to detect custom options with
;; incorrect customization types and load problems for custom and
;; autoload dependencies.
;;
;; The basic tests can be run in batch mode.  Invoke them with
;;
;;   src/emacs -batch -l admin/cus-test.el -f cus-test-opts [all]
;;
;;   src/emacs -batch -l admin/cus-test.el -f cus-test-deps
;;
;;   src/emacs -batch -l admin/cus-test.el -f cus-test-libs [all]
;;
;;   src/emacs -batch -l admin/cus-test.el -f cus-test-noloads
;;
;; in the emacs source directory.
;;
;; For interactive use: Load this file.  Then
;;
;;    M-x cus-test-apropos REGEXP RET
;;
;; checks the options matching REGEXP.	In particular
;;
;;    M-x cus-test-apropos RET
;;
;; checks all options.  The detected options are stored in the
;; variable `cus-test-errors'.
;;
;; Only those options are checked which have been already loaded.
;; Therefore `cus-test-apropos' is more efficient after loading many
;; libraries.
;;
;;    M-x cus-test-load-custom-loads
;;
;; loads all (!) custom dependencies and
;;
;;    M-x cus-test-load-libs
;;
;; loads all (!) libraries with autoloads.
;;
;; Options with a custom-get property, usually defined by a :get
;; declaration, are stored in the variable
;;
;; `cus-test-vars-with-custom-get'
;;
;; Options with a state of 'changed ("changed outside the customize
;; buffer") are stored in the variable
;;
;; `cus-test-vars-with-changed-state'
;;
;; These lists are prepared just in case one wants to investigate
;; those options further.
;;
;; The command `cus-test-opts' tests many (all?) custom options.
;;
;; The command `cus-test-deps' is like `cus-test-load-custom-loads'
;; but reports about load errors.
;;
;; The command `cus-test-libs' runs for all libraries with autoloads
;; separate emacs processes of the form "emacs -batch -l LIB".
;;
;; The command `cus-test-noloads' returns a list of variables which
;; are somewhere declared as custom options, but not loaded by
;; `custom-load-symbol'.


;;; Code:

;;; Workarounds.  For a smooth run and to avoid some side effects.

(defvar cus-test-after-load-libs-hook nil
  "Used to switch off undesired side effects of loading libraries.")

(defvar cus-test-skip-list nil
  "List of variables to disregard by `cus-test-apropos'.")

(defvar cus-test-libs-noloads
  ;; Loading dunnet in batch mode leads to a Dead end.
  ;; blessmail writes a file.
  ;; characters cannot be loaded twice ("Category `a' is already defined").
  '("play/dunnet.el" "emulation/edt-mapper.el"
    "loadup.el" "mail/blessmail.el" "international/characters.el"
    "cedet/ede/loaddefs.el" "cedet/semantic/loaddefs.el"
    "net/tramp-loaddefs.el")
  "List of files not to load by `cus-test-load-libs'.
Names should be as they appear in loaddefs.el.")

;; This avoids a hang of `cus-test-apropos' in 21.2.
;; (add-to-list 'cus-test-skip-list 'sh-alias-alist)

(or noninteractive
    ;; Never Viperize.
    (setq viper-mode nil))

;; Don't create a file `save-place-file'.
(eval-after-load "saveplace"
  '(remove-hook 'kill-emacs-hook 'save-place-kill-emacs-hook))

;; Don't create a file `abbrev-file-name'.
(setq save-abbrevs nil)

;; Avoid compile logs from adviced functions.
(eval-after-load "bytecomp"
  '(setq ad-default-compilation-action 'never))


;;; Main code:

;; We want to log all messages.
(setq message-log-max t)

(require 'cus-edit)
(require 'cus-load)

(defvar cus-test-errors nil
  "List of problematic variables found by `cus-test-apropos'.")

(defvar cus-test-tested-variables nil
  "List of options tested by last call of `cus-test-apropos'.")

;; I haven't understood this :get stuff.  The symbols with a
;; custom-get property are stored here.
(defvar cus-test-vars-with-custom-get nil
  "Set by `cus-test-apropos' to a list of options with :get property.")

(defvar cus-test-vars-with-changed-state nil
  "Set by `cus-test-apropos' to a list of options with state 'changed.")

(defvar cus-test-deps-errors nil
  "List of require/load problems found by `cus-test-deps'.")

(defvar cus-test-deps-required nil
  "List of dependencies required by `cus-test-deps'.
Only unloaded features will be require'd.")

(defvar cus-test-deps-loaded nil
  "List of dependencies loaded by `cus-test-deps'.")

(defvar cus-test-libs-errors nil
  "List of load problems found by `cus-test-load-libs' or `cus-test-libs'.")

(defvar cus-test-libs-loaded nil
  "List of files loaded by `cus-test-load-libs' or `cus-test-libs'.")

(defvar cus-test-vars-not-cus-loaded nil
  "A list of options not loaded by `custom-load-symbol'.
Set by `cus-test-noloads'.")

;; (defvar cus-test-vars-cus-loaded nil
;;   "A list of options loaded by `custom-load-symbol'.")

(defun cus-test-apropos (regexp)
  "Check the options matching REGEXP.
The detected problematic options are stored in `cus-test-errors'."
  (interactive "sVariable regexp: ")
  (setq cus-test-errors nil)
  (setq cus-test-tested-variables nil)
  (mapc
   (lambda (symbol)
     (push symbol cus-test-tested-variables)
     ;; Be verbose in case we hang.
     (message "Cus Test running...%s %s"
	      (length cus-test-tested-variables) symbol)
     (condition-case alpha
	 ;; FIXME This defaults to 'sexp if no type was specified.
	 ;; Always report such instances as a type mismatch.
	 ;; Currently abusing cusver-scan to do that.
	 (let* ((type (custom-variable-type symbol))
		(conv (widget-convert type))
		(get (or (get symbol 'custom-get) 'default-value))
		values
		mismatch)
	   (when (default-boundp symbol)
	     (push (funcall get symbol) values)
	     (push (eval (car (get symbol 'standard-value))) values))
	   (if (boundp symbol)
	       (push (symbol-value symbol) values))
	   ;; That does not work.
	   ;; (push (widget-get conv :value) values)

	   ;; Check the values
	   (mapc (lambda (value)
		   ;; TODO for booleans, check for values that can be
		   ;; evaluated and are not t or nil.  Usually a bug.
		   (unless (widget-apply conv :match value)
		     (setq mismatch 'mismatch)))
		 values)

	   ;; Store symbols with a custom-get property.
	   (when (get symbol 'custom-get)
	     (add-to-list 'cus-test-vars-with-custom-get symbol))

	   ;; Changed outside the customize buffer?
	   ;; This routine is not very much tested.
	   (let ((c-value
		  (or (get symbol 'customized-value)
		      (get symbol 'saved-value)
		      (get symbol 'standard-value))))
	     (and (consp c-value)
		  (boundp symbol)
		  (not (equal (eval (car c-value)) (symbol-value symbol)))
		  (add-to-list 'cus-test-vars-with-changed-state symbol)))

	   (if mismatch
	       (push symbol cus-test-errors)))

       (error
	(push symbol cus-test-errors)
	(message "Error for %s: %s" symbol alpha))))
   (cus-test-get-options regexp))
  (message "%s options tested"
	   (length cus-test-tested-variables))
  (cus-test-errors-display))

(defun cus-test-cus-load-groups (&optional cus-load)
  "Return a list of current custom groups.
If CUS-LOAD is non-nil, include groups from cus-load.el."
  (append (mapcar 'cdr custom-current-group-alist)
	  (if cus-load
	      (with-temp-buffer
		(insert-file-contents (locate-library "cus-load.el"))
		(search-forward "(put '")
		(beginning-of-line)
		(let (res)
		  (while (and (looking-at "^(put '\\(\\S-+\\)")
			      (zerop (forward-line 1)))
		    (push (intern (match-string 1)) res))
		  res)))))

(defun cus-test-get-options (regexp &optional group)
  "Return a list of custom options matching REGEXP.
If GROUP is non-nil, return groups rather than options.
If GROUP is `cus-load', include groups listed in cus-loads as well as
currently defined groups."
  (let ((groups (if group (cus-test-cus-load-groups (eq group 'cus-load))))
	found)
    (mapatoms
     (lambda (symbol)
       (and
	(if group
	    (memq symbol groups)
	  (or
	   ;; (user-variable-p symbol)
	   (get symbol 'standard-value)
	   ;; (get symbol 'saved-value)
	   (get symbol 'custom-type)))
	(string-match regexp (symbol-name symbol))
	(not (member symbol cus-test-skip-list))
	(push symbol found))))
    found))

(defun cus-test-errors-display ()
  "Report about the errors found by cus-test."
  (with-output-to-temp-buffer "*cus-test-errors*"
    (set-buffer standard-output)
    (insert (format "Cus Test tested %s variables.\
  See `cus-test-tested-variables'.\n\n"
		    (length cus-test-tested-variables)))
    (if (not cus-test-errors)
	(insert "No errors found by cus-test.")
      (insert "The following variables seem to have problems:\n\n")
      (dolist (e cus-test-errors)
	(insert (symbol-name e) "\n")))))

(defun cus-test-load-custom-loads ()
  "Call `custom-load-symbol' on all atoms."
  (interactive)
  (if noninteractive (let (noninteractive) (require 'dunnet)))
  (mapatoms 'custom-load-symbol)
  (run-hooks 'cus-test-after-load-libs-hook))

(defmacro cus-test-load-1 (&rest body)
  `(progn
     (setq cus-test-libs-errors nil
	   cus-test-libs-loaded nil)
     ,@body
     (message "%s libraries loaded successfully"
	      (length cus-test-libs-loaded))
     (if (not cus-test-libs-errors)
	 (message "No load problems encountered")
       (message "The following load problems appeared:")
       (cus-test-message cus-test-libs-errors))
     (run-hooks 'cus-test-after-load-libs-hook)))

;; This is just cus-test-libs, but loading in the current Emacs process.
(defun cus-test-load-libs (&optional more)
  "Load the libraries with autoloads.
Don't load libraries in `cus-test-libs-noloads'.
If optional argument MORE is \"defcustom\", load all files with defcustoms.
If it is \"all\", load all Lisp files."
  (interactive)
  (cus-test-load-1
   (let ((lispdir (file-name-directory (locate-library "loaddefs"))))
     (mapc
      (lambda (file)
	(condition-case alpha
	    (unless (member file cus-test-libs-noloads)
	      (load (file-name-sans-extension (expand-file-name file lispdir)))
	      (push file cus-test-libs-loaded))
	  (error
	   (push (cons file alpha) cus-test-libs-errors)
	   (message "Error for %s: %s" file alpha))))
      (if more
	  (cus-test-get-lisp-files (equal more "all"))
	(cus-test-get-autoload-deps))))))

(defun cus-test-get-autoload-deps ()
  "Return the list of files with autoloads."
  (with-temp-buffer
    (insert-file-contents (locate-library "loaddefs"))
    (let (files)
      (while (search-forward "\n;;; Generated autoloads from " nil t)
	(push (buffer-substring (match-end 0) (line-end-position)) files))
      files)))

(defun cus-test-get-lisp-files (&optional all)
  "Return list of all Lisp files with defcustoms.
Optional argument ALL non-nil means list all (non-obsolete) Lisp files."
  (let ((default-directory (expand-file-name "lisp/" source-directory))
	(msg "Finding files..."))
    (message "%s" msg)
    (prog1
	;; Hack to remove leading "./".
	(mapcar (lambda (e) (substring e 2))
		(apply 'process-lines find-program
		       "-name" "obsolete" "-prune" "-o"
		       "-name" "[^.]*.el" ; ignore .dir-locals.el
		       (if all
			   '("-print")
			 (list "-exec" grep-program
			       "-l" "^[ \t]*(defcustom" "{}" "+"))))
      (message "%sdone" msg))))

(defun cus-test-message (list)
  "Print the members of LIST line by line."
  (dolist (m list) (message "%s" m)))


;;; The routines for batch mode:

(defun cus-test-opts (&optional all)
  "Test custom options.
This function is suitable for batch mode.  E.g., invoke

  src/emacs -batch -l admin/cus-test.el -f cus-test-opts

in the Emacs source directory.
Normally only tests options belonging to files in loaddefs.el.
If optional argument ALL is non-nil, test all files with defcustoms."
  (interactive)
  (and noninteractive
       command-line-args-left
       (setq all (pop command-line-args-left)))
  (message "Running %s" 'cus-test-load-libs)
  (cus-test-load-libs (if all "defcustom"))
  (message "Running %s" 'cus-test-load-custom-loads)
  (cus-test-load-custom-loads)
  (message "Running %s" 'cus-test-apropos)
  (cus-test-apropos "")
  (if (not cus-test-errors)
      (message "No problems found")
    (message "The following options might have problems:")
    (cus-test-message cus-test-errors)))

(defun cus-test-deps ()
  "Run a verbose version of `custom-load-symbol' on all atoms.
This function is suitable for batch mode.  E.g., invoke

  src/emacs -batch -l admin/cus-test.el -f cus-test-deps

in the Emacs source directory."
  (interactive)
  (setq cus-test-deps-errors nil)
  (setq cus-test-deps-required nil)
  (setq cus-test-deps-loaded nil)
  (mapatoms
   ;; This code is mainly from `custom-load-symbol'.
   (lambda (symbol)
     (let ((custom-load-recursion t))
       (dolist (load (get symbol 'custom-loads))
	 (cond
	  ((symbolp load)
	   ;; (condition-case nil (require load) (error nil))
	   (condition-case alpha
	       (unless (or (featurep load)
			   (and noninteractive (eq load 'dunnet)))
		 (require load)
		 (push (list symbol load) cus-test-deps-required))
	     (error
	      (push (list symbol load alpha) cus-test-deps-errors)
	      (message "Require problem: %s %s %s" symbol load alpha))))
	  ((equal load "loaddefs")
	   (push
	    (message "Symbol %s has loaddefs as custom dependency" symbol)
	    cus-test-deps-errors))
	  ;; This is subsumed by the test below, but it's much
	  ;; faster.
	  ((assoc load load-history))
	  ;; This was just
	  ;; (assoc (locate-library load) load-history)
	  ;; but has been optimized not to load locate-library
	  ;; if not necessary.
	  ((let ((regexp (concat "\\(\\`\\|/\\)" (regexp-quote load)
				 "\\(\\'\\|\\.\\)"))
		 (found nil))
	     (dolist (loaded load-history)
	       (and (stringp (car loaded))
		    (string-match regexp (car loaded))
		    (setq found t)))
	     found))
	  ;; Without this, we would load cus-edit recursively.
	  ;; We are still loading it when we call this,
	  ;; and it is not in load-history yet.
	  ((equal load "cus-edit"))
	  ;; This would ignore load problems with files in
	  ;; lisp/term/
	  ;; ((locate-library (concat term-file-prefix load)))
	  (t
	   ;; (condition-case nil (load load) (error nil))
	   (condition-case alpha
	       (progn
		 (load load)
		 (push (list symbol load) cus-test-deps-loaded))
	     (error
	      (push (list symbol load alpha) cus-test-deps-errors)
	      (message "Load Problem: %s %s %s" symbol load alpha))))
	  )))))
  (message "%s features required"
	   (length cus-test-deps-required))
  (message "%s files loaded"
	   (length cus-test-deps-loaded))
  (if (not cus-test-deps-errors)
      (message "No load problems encountered")
    (message "The following load problems appeared:")
    (cus-test-message cus-test-deps-errors))
  (run-hooks 'cus-test-after-load-libs-hook))

(defun cus-test-libs (&optional more)
  "Load the libraries with autoloads in separate processes.
This function is useful to detect load problems of libraries.
It is suitable for batch mode.  E.g., invoke

  ./src/emacs -batch -l admin/cus-test.el -f cus-test-libs

in the Emacs source directory.

If optional argument MORE is \"defcustom\", load all files with defcustoms.
If it is \"all\", load all Lisp files."
  (interactive)
  (and noninteractive
       command-line-args-left
       (setq more (pop command-line-args-left)))
  (cus-test-load-1
   (let* ((default-directory source-directory)
	  (emacs (expand-file-name "src/emacs"))
	  skipped)
     (or (file-executable-p emacs)
	 (error "No such executable `%s'" emacs))
     (mapc
      (lambda (file)
	(if (member file cus-test-libs-noloads)
	    (push file skipped)
	  (condition-case alpha
	      (let* ((fn (expand-file-name file "lisp/"))
		     (elc (concat fn "c"))
		     status)
		(if (file-readable-p elc) ; load compiled if present (faster)
		    (setq fn elc)
		  (or (file-readable-p fn)
		      (error "Library %s not found" file)))
		(if (equal 0 (setq status (call-process emacs nil nil nil
							"-batch" "-l" fn)))
		    (message "%s" file)
		  (error "%s" status))
		(push file cus-test-libs-loaded))
	    (error
	     (push (cons file alpha) cus-test-libs-errors)
	     (message "Error for %s: %s" file alpha)))))
      (if more
	  (cus-test-get-lisp-files (equal more "all"))
	(cus-test-get-autoload-deps)))
     (message "Default directory: %s" default-directory)
     (when skipped
       (message "The following libraries were skipped:")
       (cus-test-message skipped)))))

(defun cus-test-noloads ()
  "Find custom options not loaded by `custom-load-symbol'.
Calling this function after `cus-test-load-libs' is not meaningful.
It is suitable for batch mode.  E.g., invoke

  src/emacs -batch -l admin/cus-test.el -f cus-test-noloads

in the Emacs source directory."
  (interactive)
  (let ((groups-loaded (cus-test-get-options "" 'cus-load))
	cus-loaded groups-not-loaded)

    (message "Running %s" 'cus-test-load-custom-loads)
    (cus-test-load-custom-loads)
    (setq cus-loaded (cus-test-get-options ""))

    (message "Running %s" 'cus-test-load-libs)
    (cus-test-load-libs "all")
    (setq cus-test-vars-not-cus-loaded (cus-test-get-options "")
	  groups-not-loaded (cus-test-get-options "" t))

    (dolist (o cus-loaded)
      (setq cus-test-vars-not-cus-loaded
	    (delete o cus-test-vars-not-cus-loaded)))

    (if (not cus-test-vars-not-cus-loaded)
	(message "No options not loaded by custom-load-symbol found")
      (message "The following options were not loaded by custom-load-symbol:")
      (cus-test-message
       (sort cus-test-vars-not-cus-loaded 'string<)))

    (dolist (o groups-loaded)
      (setq groups-not-loaded (delete o groups-not-loaded)))

    (if (not groups-not-loaded)
	(message "No groups not in cus-load.el found")
      (message "The following groups are not in cus-load.el:")
      (cus-test-message (sort groups-not-loaded 'string<)))))

(provide 'cus-test)

;;; cus-test.el ends here
