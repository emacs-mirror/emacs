;;; ede/auto.el --- Autoload features for EDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2022 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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
;;
;; EDE Autoloads are a way to refer to different project types without
;; loading those projects into Emacs.
;;
;; These routines are used to detect a project in a filesystem before
;; handing over control to the usual EDE project system.

;;; Code:

(require 'eieio)
(require 'cl-generic)
(require 'eieio-base)

(declare-function ede-directory-safe-p "ede")
(declare-function ede-add-project-to-global-list "ede")

(defclass ede-project-autoload-dirmatch ()
  ((fromconfig :initarg :fromconfig
	       :initform nil
	       :documentation
	       "A config file within which the match pattern lives.")
   (configregex :initarg :configregex
		:initform nil
		:documentation
		"A regexp to identify the dirmatch pattern.")
   (configregexidx :initarg :configregexidx
		   :initform nil
		   :documentation
		   "An index into the match-data of `configregex'.")
   (subdir-only :initarg :subdir-only
		:initform t
		:documentation
		"Non-nil means an exact match to the found directory is a non-match.
This implies projects exist only in subdirectories of the configuration path.
If `:subdir-only' is nil, then the directory from the configuration file is the project.")
   (configdatastash :documentation
		    "Save discovered match string.")
   )
  "Support complex matches for projects that live in named directories.
For most cases, a simple string is sufficient.  If, however, a project
location is varied dependent on other complex criteria, this class
can be used to define that match without loading the specific project
into memory.")

(cl-defmethod ede-calc-fromconfig ((dirmatch ede-project-autoload-dirmatch))
  "Calculate the value of :fromconfig from DIRMATCH."
  (let* ((fc (oref dirmatch fromconfig))
	 (found (cond ((stringp fc) fc)
		      ((functionp fc) (funcall fc))
                      (t (error "Unknown dirmatch object match style")))))
    (expand-file-name found)
    ))

(cl-defmethod ede-dirmatch-installed ((dirmatch ede-project-autoload-dirmatch))
  "Return non-nil if the tool DIRMATCH might match is installed on the system."
  (file-exists-p (ede-calc-fromconfig dirmatch)))

(cl-defmethod ede-do-dirmatch ((dirmatch ede-project-autoload-dirmatch) file)
  "Does DIRMATCH match the filename FILE."
  (let ((fc (ede-calc-fromconfig dirmatch)))

    (cond
     ;; If the thing to match is stored in a config file.
     ((stringp fc)
      (when (file-exists-p fc)
	(let ((matchstring
	       (if (slot-boundp dirmatch 'configdatastash)
		   (oref dirmatch configdatastash)
		 nil)))
	  (when (and (not matchstring) (not (slot-boundp dirmatch 'configdatastash)))
	    (save-current-buffer
	      (let* ((buff (get-file-buffer fc))
		     (readbuff
		      (let ((find-file-hook nil)) ;; Disable ede from recursing
			(find-file-noselect fc))))
		(set-buffer readbuff)
		(save-excursion
		  (goto-char (point-min))
		  (when (re-search-forward (oref dirmatch configregex) nil t)
		    (setq matchstring
			  (match-string (or (oref dirmatch configregexidx) 0)))))
		(if (not buff) (kill-buffer readbuff))))
	    (when matchstring
	      ;; If this dirmatch only finds subdirs of matchstring, then
	      ;; force matchstring to be a directory.
	      (when (oref dirmatch subdir-only)
		(setq matchstring (file-name-as-directory matchstring)))
	      ;; Convert matchstring to a regexp
	      (setq matchstring (concat "^" (regexp-quote matchstring)))
	      ;; Stash it for later.
	      (oset dirmatch configdatastash matchstring))
	    ;; Debug
	    ;;(message "Stashing config data for dirmatch %S as %S" (eieio-object-name dirmatch) matchstring)
	    )
	  ;;(message "dirmatch %s against %s" matchstring (expand-file-name file))
	  ;; Match against our discovered string
	  (setq file (file-name-as-directory (expand-file-name file)))
	  (and matchstring (string-match matchstring (expand-file-name file))
	       (or (not (oref dirmatch subdir-only))
		   (not (= (match-end 0) (length file))))
	       )
	  )))

     ;; Add new matches here
     ;; ((stringp somenewslot ...)
     ;;   )

     ;; Error if none others known
     (t
      (error "Unknown dirmatch object match style")))
    ))

(declare-function ede-directory-safe-p "ede")
(declare-function ede-add-project-to-global-list "ede")

(defclass ede-project-autoload (eieio-named)
  ((name :initarg :name
	 :documentation "Name of this project type")
   (file :initarg :file
	 :documentation "The lisp file belonging to this class.")
   (proj-file :initarg :proj-file
	      :documentation "Name of a project file of this type.")
   (root-only :initarg :root-only
	      :initform t ;; Default - majority case.
	      :documentation
	      "Non-nil if project detection only finds proj-file @ project root.")
   (proj-root-dirmatch :initarg :proj-root-dirmatch
		       :initform nil
		       :type (or null string ede-project-autoload-dirmatch)
		       :documentation
		       "To avoid loading a project, check if the directory matches this.
Specifying this matcher object will allow EDE to perform a complex
check without loading the project.

NOTE: If you use dirmatch, you may need to set :root-only to nil.
While it may be a root based project, all subdirs will happen to return
true for the dirmatch, so for scanning purposes, set it to nil.")
   (proj-root :initarg :proj-root
	      :type function
	      :documentation "A function symbol to call for the project root.
This function takes no arguments, and returns the current directories
root, if available.  Leave blank to use the EDE directory walking
routine instead.")
   (initializers :initarg :initializers
		 :initform nil
		 :documentation
		 "Initializers passed to the project object.
These are used so there can be multiple types of projects
associated with a single object class, based on the initializers used.")
   (load-type :initarg :load-type
	      :documentation "Fn symbol used to load this project file.")
   (class-sym :initarg :class-sym
	      :documentation "Symbol representing the project class to use.")
   (generic-p :initform nil
	      :documentation
	      "Generic projects are added to the project list at the end.
The add routine will set this to non-nil so that future non-generic placement will
be successful.")
   (new-p :initarg :new-p
	  :initform t
	  :documentation
	  "Non-nil if this is an option when a user creates a project.")
   (safe-p :initarg :safe-p
	   :initform t
	   :documentation
	   "Non-nil if the project load files are \"safe\".
An unsafe project is one that loads project variables via Emacs
Lisp code.  A safe project is one that loads project variables by
scanning files without loading Lisp code from them.")
   )
  "Class representing minimal knowledge set to run preliminary EDE functions.
When more advanced functionality is needed from a project type, that projects
type is required and the load function used.")

(defvar ede-project-class-files
  (list
   (ede-project-autoload :name "Make" :file 'ede/proj
			 :proj-file "Project.ede"
			 :root-only nil
			 :load-type 'ede-proj-load
			 :class-sym 'ede-proj-project
			 :safe-p nil)
   (ede-project-autoload :name "Automake" :file 'ede/proj
			 :proj-file "Project.ede"
			 :root-only nil
			 :initializers '(:makefile-type Makefile.am)
			 :load-type 'ede-proj-load
			 :class-sym 'ede-proj-project
			 :safe-p nil)
   (ede-project-autoload :name "automake" :file 'ede/project-am
			 :proj-file "Makefile.am"
			 :root-only nil
			 :load-type 'project-am-load
			 :class-sym 'project-am-makefile
			 :new-p nil
			 :safe-p t)
   )
  "List of vectors defining how to determine what type of projects exist.")

(put 'ede-project-class-files 'risky-local-variable t)

(defun ede-show-supported-projects ()
  "Display all the project types registered with EDE."
  (interactive)
  (let ((b (get-buffer-create "*EDE Autodetect Projects*")))
    (set-buffer b)
    (setq buffer-read-only nil)
    (erase-buffer)
    (dolist (prj ede-project-class-files)
      (insert (oref prj name))
      (newline))
    (display-buffer b)
 ))

(defun ede-add-project-autoload (projauto &optional flag)
  "Add PROJAUTO, an EDE autoload definition to `ede-project-class-files'.
Optional argument FLAG indicates how this autoload should be
added.  Possible values are:
  `generic' - A generic project type.  Keep this at the very end.
  `unique' - A unique project type for a specific project.  Keep at the very
             front of the list so more generic projects don't get priority."
  ;; First, can we identify PROJAUTO as already in the list?  If so, replace.
  (let ((projlist ede-project-class-files)
	(projname (oref projauto name)))
    (while (and projlist (not (string= (oref (car projlist) name) projname)))
      (setq projlist (cdr projlist)))

    (if projlist
	;; Stick the new one into the old slot.
	(setcar projlist projauto)

      ;; Else, see where to insert it.
      (cond ((and flag (eq flag 'unique))
	     ;; Unique items get stuck right onto the front.
	     (setq ede-project-class-files
		   (cons projauto ede-project-class-files)))

	    ;; Generic Projects go at the very end of the list.
	    ((and flag (eq flag 'generic))
	     (oset projauto generic-p t)
	     (setq ede-project-class-files
		   (append ede-project-class-files
			   (list projauto))))

	    ;; Normal projects go at the end of the list, but
	    ;; before the generic projects.
	    (t
	     (let ((prev nil)
		   (next ede-project-class-files))
	       (while (and next (not (oref (car next) generic-p)))
		 (setq prev next
		       next (cdr next)))
	       (when (not prev)
		 (error "ede-project-class-files not initialized"))
	       ;; Splice into the list.
	       (setcdr prev (cons projauto next))))))))

;;; Project Autoload Methods
;;

;; New method using detect.el
(cl-defmethod ede-auto-detect-in-dir ((this ede-project-autoload) dir)
  "Return non-nil if THIS project autoload is found in DIR."
  (let* ((d (file-name-as-directory dir))
	 (pf (oref this proj-file))
	 (f (when (stringp pf) (expand-file-name pf d))))
    (if f
	(and f (file-exists-p f))
      (let ((dirmatch (oref this proj-root-dirmatch)))
	(cond
	 ((stringp dirmatch)
	  nil) ; <- do something here - maybe obsolete the option?
	 ((ede-project-autoload-dirmatch-p dirmatch)
	  (if (and dirmatch (ede-dirmatch-installed dirmatch))
	      (ede-do-dirmatch dirmatch dir)
	    ;(message "Dirmatch %S not installed." dirmatch)
	    )))))))

(cl-defmethod ede-auto-load-project ((this ede-project-autoload) dir)
  "Load in the project associated with THIS project autoload description.
THIS project description should be valid for DIR, where the project will
be loaded.

NOTE: Do not call this - it should only be called from `ede-load-project-file'."
  ;; Last line of defense: don't load unsafe projects.
  (when (not (or (oref this safe-p)
		 (ede-directory-safe-p dir)))
    (error "Attempt to load an unsafe project (bug elsewhere in EDE)"))
  ;; Things are good - so load the project.
  (let ((o (funcall (oref this load-type) dir)))
    (when (not o)
      (error "Project type error: :load-type failed to create a project"))
    (ede-add-project-to-global-list o)
    ;; @TODO - Add to hash over at `ede-inode-directory-hash'.
    ))






;;; -------- Old Methods
;; See if we can do without them.

;; @FIXME - delete from loaddefs to remove this.
(cl-defmethod ede-project-root ((_this ede-project-autoload))
  "If a project knows its root, return it here.
Allows for one-project-object-for-a-tree type systems."
  nil)

;; @FIXME - delete from loaddefs to remove this.
(cl-defmethod ede-project-root-directory ((_this ede-project-autoload) &optional _file)
  "" nil)

(provide 'ede/auto)

;;; ede/auto.el ends here
