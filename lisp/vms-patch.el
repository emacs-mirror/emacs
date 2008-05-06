;; -*- no-byte-compile: t -*-
;;; vms-patch.el --- override parts of files.el for VMS

;; Copyright (C) 1986, 1992, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: vms

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defvar print-region-function)

(setq auto-mode-alist (cons '(("\\.com\\'" . dcl-mode)) auto-mode-alist))

;;; Functions that need redefinition

;;; VMS file names are upper case, but buffer names are more
;;; convenient in lower case.

(defun create-file-buffer (filename)
  "Create a suitably named buffer for visiting FILENAME, and return it.
FILENAME (sans directory) is used unchanged if that name is free;
otherwise a string <2> or <3> or ... is appended to get an unused name."
  (generate-new-buffer (downcase (file-name-nondirectory filename))))

;;; Given a string FN, return a similar name which is a valid VMS filename.
;;; This is used to avoid invalid auto save file names.
(defun make-valid-file-name (fn)
  (setq fn (copy-sequence fn))
  (let ((dot nil) (indx 0) (len (length fn)) chr)
    (while (< indx len)
      (setq chr (aref fn indx))
      (cond
       ((eq chr ?.) (if dot (aset fn indx ?_) (setq dot t)))
       ((not (or (and (>= chr ?a) (<= chr ?z)) (and (>= chr ?A) (<= chr ?Z))
		 (and (>= chr ?0) (<= chr ?9))
		 (eq chr ?$) (eq chr ?_) (and (eq chr ?-) (> indx 0))))
	(aset fn indx ?_)))
      (setq indx (1+ indx))))
  fn)

(define-obsolete-function-alias 'make-legal-file-name 'make-valid-file-name "23.1")

;;; Auto save filesnames start with _$ and end with $.

(defun make-auto-save-file-name ()
  "Return file name to use for auto-saves of current buffer.
This function does not consider `auto-save-visited-file-name';
the caller should check that before calling this function.
This is a separate function so that your `.emacs' file or the site's
`site-init.el' can redefine it.
See also `auto-save-file-name-p'."
  (if buffer-file-name
      (concat (file-name-directory buffer-file-name)
	      "_$"
	      (file-name-nondirectory buffer-file-name)
	      "$")
    (expand-file-name (concat "_$_" (make-valid-file-name (buffer-name)) "$"))))

(defun auto-save-file-name-p (filename)
  "Return t if FILENAME can be yielded by `make-auto-save-file-name'.
FILENAME should lack slashes.
This is a separate function so that your `.emacs' file or the site's
`site-init.el' can redefine it."
  (string-match "^_\\$.*\\$" filename))

;;;
;;; This goes along with kepteditor.com which defines these logicals
;;; If EMACS_COMMAND_ARGS is defined, it supersedes EMACS_FILE_NAME,
;;;   which is probably set up incorrectly anyway.
;;; The function command-line-again is a kludge, but it does the job.
;;;
(defun vms-suspend-resume-hook ()
  "When resuming suspended Emacs, check for file to be found.
If the logical name `EMACS_FILE_NAME' is defined, `find-file' that file."
  (let ((file (vms-system-info "LOGICAL" "EMACS_FILE_NAME"))
	(args (vms-system-info "LOGICAL" "EMACS_COMMAND_ARGS"))
	(line (vms-system-info "LOGICAL" "EMACS_FILE_LINE")))
    (if (not args)
	(if file
	    (progn (find-file file)
		   (if line (goto-line (string-to-number line)))))
      (cd (file-name-directory file))
      (vms-command-line-again))))

(setq suspend-resume-hook 'vms-suspend-resume-hook)

(defun vms-suspend-hook ()
  "Don't allow suspending if logical name `DONT_SUSPEND_EMACS' is defined."
  (if (vms-system-info "LOGICAL" "DONT_SUSPEND_EMACS")
      (error "Can't suspend this emacs"))
  nil)

(setq suspend-hook 'vms-suspend-hook)

;;;
;;; A kludge that allows reprocessing of the command line.  This is mostly
;;;   to allow a spawned VMS mail process to do something reasonable when
;;;   used in conjunction with the modifications to sysdep.c that allow
;;;   Emacs to attach to a "foster" parent.
;;;
(defun vms-command-line-again ()
  "Reprocess command line arguments.  VMS specific.
Command line arguments are initialized from the logical EMACS_COMMAND_ARGS
which is defined by kepteditor.com.  On VMS this allows attaching to a
spawned Emacs and doing things like \"emacs -l myfile.el -f doit\""
  (let* ((args (downcase (vms-system-info "LOGICAL" "EMACS_COMMAND_ARGS")))
	 (command-line-args (list "emacs"))
	 (beg 0)
	 (end 0)
	 (len (length args))
	 this-char)
    (if args
	(progn
;;; replace non-printable stuff with spaces
	  (while (< beg (length args))
	    (if (or (> 33 (setq this-char (aref args beg)))
		    (< 127 this-char))
		(aset args beg 32))
	    (setq beg (1+ beg)))
	  (setq beg (1- (length args)))
	  (while (= 32 (aref args beg)) (setq beg (1- beg)))
	  (setq args (substring args 0 (1+ beg)))
	  (setq beg 0)
;;; now start parsing args
	  (while (< beg (length args))
	    (while (and (< beg (length args))
			(or (> 33 (setq this-char (aref args beg)))
			    (< 127 this-char))
			(setq beg (1+ beg))))
	    (setq end (1+ beg))
	    (while (and (< end (length args))
			(< 32 (setq this-char (aref args end)))
			(> 127 this-char))
	      (setq end (1+ end)))
	    (setq command-line-args (append
				     command-line-args
				     (list (substring args beg end))))
	    (setq beg (1+ end)))
	  (command-line)))))

(defun vms-read-directory (dirname switches buffer)
  (save-excursion
    (set-buffer buffer)
    (subprocess-command-to-buffer
     (concat "DIRECTORY " switches " " dirname)
     buffer)
    (goto-char (point-min))
    ;; Remove all the trailing blanks.
    (while (search-forward " \n")
      (forward-char -1)
      (delete-horizontal-space))
    (goto-char (point-min))))

(setq dired-listing-switches
      "/SIZE/DATE/OWNER/WIDTH=(FILENAME=32,SIZE=5)")

(setq print-region-function
      (lambda (start end command ign1 ign2 ign3 &rest switches)
	 (write-region start end "sys$login:delete-me.txt")
	 (send-command-to-subprocess
	  1
	  (concat command
		  " sys$login:delete-me.txt/name=\"GNUprintbuffer\" "
		  (mapconcat 'identity switches " "))
	  nil nil nil)))

;;;
;;; Fuctions for using Emacs as a VMS Mail editor
;;;
(autoload 'vms-pmail-setup "vms-pmail"
  "Set up file assuming use by VMS Mail utility.
The buffer is put into text-mode, auto-save is turned off and the
following bindings are established.

\\[vms-pmail-save-and-exit]	vms-pmail-save-and-exit
\\[vms-pmail-abort]	vms-pmail-abort

All other Emacs commands are still available."
  t)

;;;
;;; Filename handling in the minibuffer
;;;
(defun vms-magic-right-square-brace ()
  "\
Insert a right square brace, but do other things first depending on context.
During filename completion, when point is at the end of the line and the
character before is not a right square brace, do one of three things before
inserting the brace:
 - If there are already two left square braces preceding, do nothing special.
 - If there is a previous right-square-brace, convert it to dot.
 - If the character before is dot, delete it.
Additionally, if the preceding chars are right-square-brace followed by
either \"-\" or \"..\", strip one level of directory hierarchy."
  (interactive)
  (when (and minibuffer-completing-file-name
	     (= (point) (point-max))
	     (not (= 93 (char-before))))
    (cond
     ;; Avoid clobbering: user:[one.path][another.path
     ((search-backward "[" (field-beginning) t 2))
     ((search-backward "]" (field-beginning) t)
      (delete-char 1)
      (insert ".")
      (goto-char (point-max)))
     ((= ?. (char-before))
      (delete-char -1)))
    (goto-char (point-max))
    (let ((specs '(".." "-"))
	  (pmax (point-max)))
      (while specs
	(let* ((up (car specs))
	       (len (length up))
	       (cut (- (point) len)))
	  (when (and (< (1+ len) pmax)
		     (= ?. (char-before cut))
		     (string= up (buffer-substring cut (point))))
	    (delete-char (- (1+ len)))
	    (while (not (let ((c (char-before)))
			  (or (= ?. c) (= 91 c))))
	      (delete-char -1))
	    (when (= ?. (char-before)) (delete-char -1))
	    (setq specs nil)))
	(setq specs (cdr specs)))))
  (insert "]"))

(defun vms-magic-colon ()
  "\
Insert a colon, but do other things first depending on context.
During filename completion, when point is at the end of the line
and the line contains a right square brace, remove all characters
from the beginning of the line up to and including such brace.
This enables one to type a new filespec without having to delete
the old one."
  (interactive)
  (when (and minibuffer-completing-file-name
	     (= (point) (point-max))
             (search-backward "]" (field-beginning) t))
    (delete-region (field-beginning) (1+ (point)))
    (goto-char (point-max)))
  (insert ":"))

(let ((m minibuffer-local-completion-map))
  (define-key m "]" 'vms-magic-right-square-brace)
  (define-key m "/" 'vms-magic-right-square-brace)
  (define-key m ":" 'vms-magic-colon))

;; arch-tag: c178494e-2c37-4d02-99b7-e47e615656cf
;;; vms-patch.el ends here
