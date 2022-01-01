;;; ede/emacs.el --- Special project for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2022 Free Software Foundation, Inc.

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
;; Provide a special project type just for Emacs, cause Emacs is special.
;;
;; Identifies an Emacs project automatically.
;; Speedy ede-expand-filename based on extension.
;; Pre-populates the preprocessor map from lisp.h
;;
;; ToDo :
;; * Add "build" options.
;; * Add texinfo lookup options.
;; * Add website

(require 'ede)
(require 'semantic/db)

;;; Code:

;; @TODO - get rid of this.  Stuck in loaddefs right now.

(defun ede-emacs-project-root (&optional _dir)
  "Get the root directory for DIR."
  nil)

(defun ede-emacs-version (dir)
  "Find the Emacs version for the Emacs src in DIR.
Return a tuple of ( EMACSNAME . VERSION )."
  (let ((buff (get-buffer-create " *emacs-query*"))
	(configure_ac "configure.ac")
	(emacs "Emacs")
	(ver ""))
    (with-current-buffer buff
      (erase-buffer)
      (setq default-directory (file-name-as-directory dir))
      (cond
       ;; Vaguely recent version of GNU Emacs?
       ((or (file-exists-p configure_ac)
	    (file-exists-p (setq configure_ac "configure.in")))
	(insert-file-contents configure_ac)
	(goto-char (point-min))
	(re-search-forward "AC_INIT(\\(?:GNU \\)?[eE]macs,\\s-*\\([0-9.]+\\)\\s-*[,)]")
	(setq ver (match-string 1))
	)
       )
      ;; Return a tuple
      (cons emacs ver))))

(defclass ede-emacs-project (ede-project)
  (
   )
  "Project Type for the Emacs source code."
  :method-invocation-order :depth-first)

(defun ede-emacs-load (dir &optional _rootproj)
  "Return an Emacs Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  ;; Doesn't already exist, so let's make one.
  (let* ((vertuple (ede-emacs-version dir)))
    (ede-emacs-project
     (car vertuple)
     :name (car vertuple)
     :version (cdr vertuple)
     :directory (file-name-as-directory dir)
     :file (expand-file-name "src/emacs.c"
			     dir))))

;;;###autoload
(ede-add-project-autoload
 (make-instance 'ede-project-autoload
                :name "EMACS ROOT"
                :file 'ede/emacs
                :proj-file "src/emacs.c"
                :load-type 'ede-emacs-load
                :class-sym 'ede-emacs-project
                :new-p nil
                :safe-p t)
 'unique)

(defclass ede-emacs-target-c (ede-target)
  ()
  "EDE Emacs Project target for C code.
All directories need at least one target.")

(defclass ede-emacs-target-el (ede-target)
  ()
  "EDE Emacs Project target for Emacs Lisp code.
All directories need at least one target.")

(defclass ede-emacs-target-misc (ede-target)
  ()
  "EDE Emacs Project target for Misc files.
All directories need at least one target.")

(cl-defmethod initialize-instance ((this ede-emacs-project)
                                   &rest _fields)
  "Make sure the targets slot is bound."
  (cl-call-next-method)
  (unless (slot-boundp this 'targets)
    (oset this :targets nil)))

;;; File Stuff
;;
(cl-defmethod ede-project-root-directory ((this ede-emacs-project)
                                          &optional _file)
  "Return the root for THIS Emacs project with file."
  (ede-up-directory (file-name-directory (oref this file))))

(cl-defmethod ede-project-root ((this ede-emacs-project))
  "Return my root."
  this)

(cl-defmethod ede-find-subproject-for-directory ((proj ede-emacs-project)
                                                 _dir)
  "Return PROJ, for handling all subdirs below DIR."
  proj)

;;; TARGET MANAGEMENT
;;
(defun ede-emacs-find-matching-target (class dir targets)
  "Find a target that is a CLASS and is in DIR in the list of TARGETS."
  (let ((match nil))
    (dolist (T targets)
      (when (and (object-of-class-p T class)
		 (string= (oref T path) dir))
	(setq match T)
      ))
    match))

(cl-defmethod ede-find-target ((proj ede-emacs-project) buffer)
  "Find an EDE target in PROJ for BUFFER.
If one doesn't exist, create a new one for this directory."
  (let* ((ext (file-name-extension (buffer-file-name buffer)))
	 (cls (cond ((not ext)
		     'ede-emacs-target-misc)
		    ((string-match "c\\|h" ext)
		     'ede-emacs-target-c)
		    ((string-match "elc?" ext)
		     'ede-emacs-target-el)
		    (t 'ede-emacs-target-misc)))
	 (targets (oref proj targets))
	 (dir default-directory)
	 (ans (ede-emacs-find-matching-target cls dir targets))
	 )
    (when (not ans)
      (setq ans (make-instance
		 cls
		 :name (file-name-nondirectory
			(directory-file-name dir))
		 :path dir
		 :source nil))
      (object-add-to-list proj :targets ans)
      )
    ans))

;;; UTILITIES SUPPORT.
;;
(cl-defmethod ede-preprocessor-map ((this ede-emacs-target-c))
  "Get the pre-processor map for Emacs C code.
All files need the macros from lisp.h!"
  (require 'semantic/db)
  (let* ((proj (ede-target-parent this))
	 (root (ede-project-root proj))
	 (table (semanticdb-file-table-object
		 (ede-expand-filename root "lisp.h")))
	 (config (semanticdb-file-table-object
		  (ede-expand-filename root "config.h")))
	 filemap
	 )
    (when table
      (when (semanticdb-needs-refresh-p table)
	(semanticdb-refresh-table table))
      (setq filemap (append filemap (oref table lexical-table)))
      )
    (when config
      (when (semanticdb-needs-refresh-p config)
	(semanticdb-refresh-table config))
      (setq filemap (append filemap (oref config lexical-table)))
      )
    filemap
    ))

(defun ede-emacs-find-in-directories (name base dirs)
  "Find NAME is BASE directory sublist of DIRS."
  (let ((ans nil))
    (while (and dirs (not ans))
      (let* ((D (car dirs))
	     (ed (expand-file-name D base))
	     (ef (expand-file-name name ed)))
	(when (file-exists-p ed)
          (if (file-exists-p ef)
	      (setq ans ef)
	    ;; Not in this dir?  How about subdirs?
	    (let ((dirfile (directory-files ed t))
		  (moredirs nil))
	      ;; Get all the subdirs.
	      (dolist (DF dirfile)
	        (when (and (file-directory-p DF)
			   (not (string-match "\\.$" DF)))
		  (push DF moredirs)))
	      ;; Try again.
	      (setq ans (ede-emacs-find-in-directories name ed moredirs)))))
	(setq dirs (cdr dirs))))
    ans))

(cl-defmethod ede-expand-filename-impl ((proj ede-emacs-project) name)
  "Within this project PROJ, find the file NAME.
Knows about how the Emacs source tree is organized."
  (let* ((ext (file-name-extension name))
	 (root (ede-project-root proj))
	 (dir (ede-project-root-directory root))
	 (dirs (cond
		((not ext) nil)
		((string-match "h\\|c" ext)
		 '("src" "lib-src" "lwlib"))
		((string-match "elc?" ext)
		 '("lisp"))
		((string-match "texi" ext)
		 '("doc"))
		(t nil)))
	 )
    (if (not dirs) (cl-call-next-method)
      (ede-emacs-find-in-directories name dir dirs))
    ))

;;; Command Support
;;
(cl-defmethod project-rescan ((this ede-emacs-project))
  "Rescan this Emacs project from the sources."
  (let ((ver (ede-emacs-version (ede-project-root-directory this))))
    (oset this name (car ver))
    (oset this version (cdr ver))
    ))

(provide 'ede/emacs)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/emacs"
;; End:

;;; ede/emacs.el ends here
