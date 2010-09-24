;;; ede/auto.el --- Autoload features for EDE

;; Copyright (C) 2010  Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; EDE Autoloads are a way to refer to different project types without
;; loading those projects into Emacs.
;;
;; These routines are used to detect a project in a filesystem before
;; handing over control to the usual EDE project system.

;;; Code:

(require 'eieio)

(defclass ede-project-autoload ()
  ((name :initarg :name
	 :documentation "Name of this project type")
   (file :initarg :file
	 :documentation "The lisp file belonging to this class.")
   (proj-file :initarg :proj-file
	      :documentation "Name of a project file of this type.")
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
associated with a single object class, based on the initilizeres used.")
   (load-type :initarg :load-type
	      :documentation "Fn symbol used to load this project file.")
   (class-sym :initarg :class-sym
	      :documentation "Symbol representing the project class to use.")
   (new-p :initarg :new-p
	  :initform t
	  :documentation
	  "Non-nil if this is an option when a user creates a project.")
   )
  "Class representing minimal knowledge set to run preliminary EDE functions.
When more advanced functionality is needed from a project type, that projects
type is required and the load function used.")

(defvar ede-project-class-files
  (list
   (ede-project-autoload "edeproject-makefile"
			 :name "Make" :file 'ede/proj
			 :proj-file "Project.ede"
			 :load-type 'ede-proj-load
			 :class-sym 'ede-proj-project)
   (ede-project-autoload "edeproject-automake"
			 :name "Automake" :file 'ede/proj
			 :proj-file "Project.ede"
			 :initializers '(:makefile-type Makefile.am)
			 :load-type 'ede-proj-load
			 :class-sym 'ede-proj-project)
   (ede-project-autoload "automake"
			 :name "automake" :file 'ede/project-am
			 :proj-file "Makefile.am"
			 :load-type 'project-am-load
			 :class-sym 'project-am-makefile
			 :new-p nil))
  "List of vectors defining how to determine what type of projects exist.")

;;; EDE project-autoload methods
;;
(defmethod ede-project-root ((this ede-project-autoload))
  "If a project knows its root, return it here.
Allows for one-project-object-for-a-tree type systems."
  nil)

(defmethod ede-project-root-directory ((this ede-project-autoload)
				       &optional file)
  "If a project knows its root, return it here.
Allows for one-project-object-for-a-tree type systems.
Optional FILE is the file to test.  If there is no FILE, use
the current buffer."
  (when (not file)
    (setq file default-directory))
  (when (slot-boundp this :proj-root)
    (let ((rootfcn (oref this proj-root)))
      (when rootfcn
	(condition-case nil
	    (funcall rootfcn file)
	  (error
	   (funcall rootfcn)))
	))))

(defmethod ede-dir-to-projectfile ((this ede-project-autoload) dir)
  "Return a full file name of project THIS found in DIR.
Return nil if the project file does not exist."
  (let* ((d (file-name-as-directory dir))
	 (root (ede-project-root-directory this d))
	 (pf (oref this proj-file))
	 (f (cond ((stringp pf)
		   (expand-file-name pf (or root d)))
		  ((and (symbolp pf) (fboundp pf))
		   (funcall pf (or root d)))))
	 )
    (when (and f (file-exists-p f))
      f)))


(provide 'ede/auto)

;;; ede/auto.el ends here
