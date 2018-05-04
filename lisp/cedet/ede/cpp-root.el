;;; ede/cpp-root.el --- A simple way to wrap a C++ project with a single root

;; Copyright (C) 2007-2018 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;; NOTE: ede/cpp-root.el has been commented so as to also make it
;;       useful for learning how to make similar project types.
;;
;; Not everyone can use automake, or an EDE project type.  For
;; pre-existing code, it is often helpful jut to be able to wrap the
;; whole thing up in as simple a way as possible.
;;
;; The cpp-root project type will allow you to create a single object
;; with no save-file in your .emacs file that will be recognized, and
;; provide a way to easily allow EDE to provide Semantic with the
;; ability to find header files, and other various source files
;; quickly.
;;
;; The cpp-root class knows a few things about C++ projects, such as
;; the prevalence of "include" directories, and typical file-layout
;; stuff.  If this isn't sufficient, you can subclass
;; `ede-cpp-root-project' and add your own tweaks in just a few lines.
;; See the end of this file for an example.
;;
;;; EXAMPLE
;;
;; Add this to your .emacs file, modifying appropriate bits as needed.
;;
;; (ede-cpp-root-project "SOMENAME" :file "/dir/to/some/file")
;;
;; Replace SOMENAME with whatever name you want, and the filename to
;; an actual file at the root of your project.  It might be a
;; Makefile, a README file.  Whatever.  It doesn't matter.  It's just
;; a key to hang the rest of EDE off of.
;;
;; The most likely reason to create this project, is to help make
;; finding files within the project faster.  In conjunction with
;; Semantic completion, having a short include path is key.  You can
;; override the include path like this:
;;
;; (ede-cpp-root-project "NAME" :file "FILENAME"
;;     :include-path '( "/include" "../include" "/c/include" )
;;     :system-include-path '( "/usr/include/c++/3.2.2/" )
;;     :spp-table '( ("MOOSE" . "")
;;                   ("CONST" . "const") )
;;     :spp-files '( "include/config.h" )
;;     )
;;
;;  In this case each item in the include path list is searched.  If
;;  the directory starts with "/", then that expands to the project
;;  root directory.  If a directory does not start with "/", then it
;;  is relative to the default-directory of the current buffer when
;;  the file name is expanded.
;;
;;  The include path only affects C/C++ header files.  Use the slot
;;  :header-match-regexp to change it.
;;
;;  The :system-include-path allows you to specify full directory
;;  names to include directories where system header files can be
;;  found.  These will be applied to files in this project only.
;;
;;  The :spp-table provides a list of project specific #define style
;;  macros that are unique to this project, passed in to the compiler
;;  on the command line, or are in special headers.
;;
;;  The :spp-files option is like :spp-table, except you can provide a
;;  file name for a header in your project where most of your CPP
;;  macros reside.  Doing this can be easier than listing everything in
;;  the :spp-table option.  The files listed in :spp-files should not
;;  start with a /, and are relative to something in :include-path.
;;
;; If you want to override the file-finding tool with your own
;; function you can do this:
;;
;; (ede-cpp-root-project "NAME" :file "FILENAME" :locate-fcn 'MYFCN)
;;
;; Where FILENAME is a file in the root directory of the project.
;; Where MYFCN is a symbol for a function.  See:
;;
;; M-x describe-function RET ede-cpp-root-project RET
;;
;; for documentation about the locate-fcn extension.
;;
;;; ADVANCED EXAMPLE
;;
;; If the cpp-root project style is right for you, but you want a
;; dynamic loader, instead of hard-coding values in your .emacs, you
;; can do that too, but you will need to write some lisp code.
;;
;; To do that, you need to add an entry to the
;; `ede-project-class-files' list, and also provide two functions to
;; teach EDE how to load your project pattern
;;
;; It would look like this:
;;
;; (defun MY-FILE-FOR-DIR (&optional dir)
;;   "Return a full file name to the project file stored in DIR."
;;   <write your code here, or return nil>
;;   )
;;
;; (defun MY-LOAD (dir)
;;   "Load a project of type `cpp-root' for the directory DIR.
;; Return nil if there isn't one."
;;   (ede-cpp-root-project "NAME" :file (expand-file-name "FILE" dir)
;;                                :locate-fcn 'MYFCN)
;;   )
;;
;; (ede-add-project-autoload
;;  (ede-project-autoload "cpp-root"
;; 	      :name "CPP ROOT"
;; 	      :file 'ede/cpp-root
;; 	      :proj-file 'MY-FILE-FOR-DIR
;; 	      :load-type 'MY-LOAD
;; 	      :class-sym 'ede-cpp-root-project
;;	      :safe-p t))
;;
;;; TODO
;;
;; Need a way to reconfigure a project, and have it affect all open buffers.
;; From Tobias Gerdin:
;;
;;   >>3) Is there any way to refresh an ede-cpp-root-project dynamically? I have
;;   >>some file open part of the project, fiddle with the include paths and would
;;   >>like the open buffer to notice this when I re-evaluate the
;;   >>ede-cpp-root-project constructor.
;;   >
;;   > Another good idea.  The easy way is to "revert-buffer" as needed.  The
;;   > ede "project local variables" does this already, so it should be easy
;;   > to adapt something.
;;
;;   I actually tried reverting the buffer but Semantic did not seem to pick
;;   up the differences (the "include summary" reported the same include paths).

(require 'ede)

(defvar semantic-lex-spp-project-macro-symbol-obarray)
(declare-function semantic-lex-make-spp-table "semantic/lex-spp")
(declare-function semanticdb-file-table-object "semantic/db")
(declare-function semanticdb-needs-refresh-p "semantic/db")
(declare-function semanticdb-refresh-table "semantic/db")

;;; Code:

;;; PROJECT CACHE:
;;
;; cpp-root projects are created in a .emacs or other config file.  We
;; need to cache them so if the user re-loads a lisp file with the
;; config in it, we can flush out the old one and replace it.
;;
(defvar ede-cpp-root-project-list nil
  "List of projects created by option `ede-cpp-root-project'.")


;;; CLASSES
;;
;; EDE sets up projects with two kinds of objects.
;;
;; The PROJECT is a class that represents everything under a directory
;; hierarchy.  A TARGET represents a subset of files within a project.
;; A project can have multiple targets, and multiple sub-projects.
;; Sub projects should map to sub-directories.
;;
;; The CPP-ROOT project maps any file in C or C++ mode to a target for
;; C files.
;;
;; When creating a custom project the project developer an opportunity
;; to run code to setup various tools whenever an associated buffer is
;; loaded.  The CPP-ROOT project spends most of its time setting up C
;; level include paths, and PreProcessor macro tables.

(defclass ede-cpp-root-target (ede-target)
  ((project :initform nil
	    :initarg :project))
  "EDE cpp-root project target.
All directories need at least one target.")

;;;###autoload
(defclass ede-cpp-root-project (ede-project eieio-instance-tracker)
  ((tracking-symbol :initform 'ede-cpp-root-project-list)
   (include-path :initarg :include-path
		 :initform '( "/include" "../include/" )
		 :type list
		 :documentation
		 "The default locate function expands filenames within a project.
If a header file (.h, .hh, etc) name is expanded, and
the :locate-fcn slot is nil, then the include path is checked
first, and other directories are ignored.  For very large
projects, this optimization can save a lot of time.

Directory names in the path can be relative to the current
buffer's `default-directory' (not starting with a /).  Directories
that are relative to the project's root should start with a /, such
as  \"/include\", meaning the directory `include' off the project root
directory.")
   (system-include-path :initarg :system-include-path
			:initform nil
			:type list
			:documentation
			"The system include path for files in this project.
C files initialized in an ede-cpp-root-project have their semantic
system include path set to this value.  If this is nil, then the
semantic path is not modified.")
   (spp-table :initarg :spp-table
	      :initform nil
	      :type list
	      :documentation
	      "C Preprocessor macros for your files.
Preprocessor symbols will be used while parsing your files.
These macros might be passed in through the command line compiler, or
are critical symbols derived from header files.  Providing header files
macro values through this slot improves accuracy and performance.
Use `:spp-files' to use these files directly.")
   (spp-files :initarg :spp-files
	      :initform nil
	      :type list
	      :documentation
	      "C header file with Preprocessor macros for your files.
The PreProcessor symbols appearing in these files will be used while
parsing files in this project.
See `semantic-lex-c-preprocessor-symbol-map' for more on how this works.")
   (header-match-regexp :initarg :header-match-regexp
			:initform
			"\\.\\(h\\(h\\|xx\\|pp\\|\\+\\+\\)?\\|H\\)$\\|\\<\\w+$"
			:type string
			:documentation
			"Regexp used to identify C/C++ header files.")
   (locate-fcn :initarg :locate-fcn
	       :initform nil
	       :type (or null function)
	       :documentation
	       "The locate function can be used in place of
`ede-expand-filename' so you can quickly customize your custom target
to use specialized local routines instead of the EDE routines.
The function symbol must take two arguments:
  NAME - The name of the file to find.
  DIR - The directory root for this cpp-root project.

It should return the fully qualified file name passed in from NAME.  If that file does not
exist, it should return nil."
	       )
   (compile-command :initarg :compile-command
		    :initform nil
		    :type (or null string function)
		    :documentation
		    "Compilation command that will be used for this project.
It could be string or function that will accept proj argument and should return string.
The string will be passed to `compile' function that will be issued in root
directory of project."
		    )
   )
  "EDE cpp-root project class.
Each directory needs a project file to control it.")

;;; INIT
;;
;; Most projects use `initialize-instance' to do special setup
;; on the object when it is created.  In this case, EDE-CPP-ROOT can
;; find previous copies of this project, and make sure that one of the
;; objects is deleted.

(cl-defmethod initialize-instance ((this ede-cpp-root-project)
				&rest fields)
  "Make sure the :file is fully expanded."
  ;; Add ourselves to the master list
  (cl-call-next-method)
  (let ((f (expand-file-name (oref this :file))))
    ;; Remove any previous entries from the main list.
    (let ((old (eieio-instance-tracker-find (file-name-directory f)
					    :directory 'ede-cpp-root-project-list)))
      ;; This is safe, because :directory isn't filled in till later.
      (when (and old (not (eq old this)))
	(ede-delete-project-from-global-list old)
	(delete-instance old)))
    ;; Basic initialization.
    (when (or (not (file-exists-p f))
	      (file-directory-p f))
      (delete-instance this)
      (error ":file for ede-cpp-root-project must be a file"))
    (oset this :file f)
    (oset this :directory (file-name-directory f))
    (ede-project-directory-remove-hash (file-name-directory f))
    ;; NOTE: We must add to global list here because these classes are not
    ;;       created via the typical loader, but instead via calls from a .emacs
    ;;       file.
    (ede-add-project-to-global-list this)

    (unless (slot-boundp this 'targets)
      (oset this :targets nil))
    ))

;;; SUBPROJ Management.
;;
;; This is a way to allow a subdirectory to point back to the root
;; project, simplifying authoring new single-point projects.

(cl-defmethod ede-find-subproject-for-directory ((proj ede-cpp-root-project)
					      dir)
  "Return PROJ, for handling all subdirs below DIR."
  proj)

;;; TARGET MANAGEMENT
;;
;; Creating new targets on a per directory basis is a good way to keep
;; files organized.  See ede-emacs for an example with multiple file
;; types.
(cl-defmethod ede-find-target ((proj ede-cpp-root-project) buffer)
  "Find an EDE target in PROJ for BUFFER.
If one doesn't exist, create a new one for this directory."
  (let* ((targets (oref proj targets))
	 (dir default-directory)
	 (ans (object-assoc dir :path targets))
	 )
    (when (not ans)
      (setq ans (ede-cpp-root-target dir
                 :name (file-name-nondirectory
			(directory-file-name dir))
		 :path dir
		 :source nil
		 :project proj))
      (object-add-to-list proj :targets ans)
      )
    ans))

;;; FILE NAMES
;;
;; One of the more important jobs of EDE is to find files in a
;; directory structure.  cpp-root has tricks it knows about how most C
;; projects are set up with include paths.
;;
;; This tools also uses the ede-locate setup for augmented file name
;; lookup using external tools.
(cl-defmethod ede-expand-filename-impl ((proj ede-cpp-root-project) name)
  "Within this project PROJ, find the file NAME.
This knows details about or source tree."
  ;; The slow part of the original is looping over subprojects.
  ;; This version has no subprojects, so this will handle some
  ;; basic cases.
  (let ((ans (cl-call-next-method)))
    (unless ans
      (let* ((lf (oref proj locate-fcn))
	     (dir (file-name-directory (oref proj file))))
	(if lf
	    (setq ans (funcall lf name dir))
	  (if (ede-cpp-root-header-file-p proj name)
	      ;; Else, use our little hack.
	      (let ((ip (oref proj include-path))
		    (tmp nil))
		(while ip
		  ;; Translate
		  (setq tmp (ede-cpp-root-translate-file proj (car ip)))
		  ;; Test this name.
		  (setq tmp (expand-file-name name tmp))
		  (if (file-exists-p tmp)
		      (setq ans tmp))
		  (setq ip (cdr ip)) ))
	    ;; Else, do the usual.
	    (setq ans (cl-call-next-method)))
	  )))
    ;; TODO - does this call-next-method happen twice.  Is that bad??  Why is it here?
    (or ans (cl-call-next-method))))

(cl-defmethod ede-project-root ((this ede-cpp-root-project))
  "Return my root."
  this)

(cl-defmethod ede-project-root-directory ((this ede-cpp-root-project))
  "Return my root."
  (oref this directory))

;;; C/CPP SPECIFIC CODE
;;
;; The following code is specific to setting up header files,
;; include lists, and Preprocessor symbol tables.

(cl-defmethod ede-cpp-root-header-file-p ((proj ede-cpp-root-project) name)
  "Non nil if in PROJ the filename NAME is a header."
  (save-match-data
    (string-match (oref proj header-match-regexp) name)))

(cl-defmethod ede-cpp-root-translate-file ((proj ede-cpp-root-project) filename)
  "For PROJ, translate a user specified FILENAME.
This is for project include paths and spp source files."
  ;; Step one: Root of this project.
  (let ((dir (file-name-directory (oref proj file))))

    ;; Step two: Analyze first char, and rehost
    (if (and (not (string= filename "")) (= (aref filename 0) ?/))
	;; Check relative to root of project
	(setq filename (expand-file-name (substring filename 1)
					 dir))
      ;; Relative to current directory.
      (setq filename (expand-file-name filename)))

    filename))

(cl-defmethod ede-system-include-path ((this ede-cpp-root-project))
  "Get the system include path used by project THIS."
  (oref this system-include-path))

(cl-defmethod ede-preprocessor-map ((this ede-cpp-root-project))
  "Get the pre-processor map for project THIS."
  (require 'semantic/db)
  (let ((spp (oref this spp-table))
	(root (ede-project-root this))
	)
    (mapc
     (lambda (F)
       (let* ((expfile (ede-expand-filename root F))
	      (table (when expfile
		       ;; Disable EDE init on preprocessor file load
		       ;; otherwise we recurse, cause errs, etc.
		       (let ((ede-constructing t))
			 (semanticdb-file-table-object expfile))))
	      )
	 (cond
	  ((not (file-exists-p expfile))
	   (message "Cannot find file %s in project." F))
	  ((string= expfile (buffer-file-name))
	   ;; Don't include this file in it's own spp table.
	   )
	  ((not table)
	   (message "No db table available for %s." expfile))
	  (t
	   (when (semanticdb-needs-refresh-p table)
	     (semanticdb-refresh-table table))
	   (setq spp (append spp (oref table lexical-table)))))))
     (oref this spp-files))
    spp))

(cl-defmethod ede-system-include-path ((this ede-cpp-root-target))
  "Get the system include path used by target THIS."
  (ede-system-include-path (ede-target-parent this)))

(cl-defmethod ede-preprocessor-map ((this ede-cpp-root-target))
  "Get the pre-processor map for project THIS."
  (ede-preprocessor-map  (ede-target-parent this)))

(cl-defmethod project-compile-project ((proj ede-cpp-root-project) &optional command)
  "Compile the entire current project PROJ.
Argument COMMAND is the command to use when compiling."
  ;; we need to be in the proj root dir for this to work
  (let* ((cmd (oref proj :compile-command))
	 (ov (oref proj :local-variables))
	 (lcmd (when ov (cdr (assoc 'compile-command ov))))
	 (cmd-str (cond
		   ((stringp cmd) cmd)
		   ((functionp cmd) (funcall cmd proj))
		   ((stringp lcmd) lcmd)
		   ((functionp lcmd) (funcall lcmd proj)))))
    (when cmd-str
	(let ((default-directory (ede-project-root-directory proj)))
	(compile cmd-str)))))

(cl-defmethod project-compile-target ((obj ede-cpp-root-target) &optional command)
  "Compile the current target OBJ.
Argument COMMAND is the command to use for compiling the target."
  (when (oref obj :project)
    (project-compile-project (oref obj :project) command)))


(cl-defmethod project-rescan ((this ede-cpp-root-project))
  "Don't rescan this project from the sources."
  (message "cpp-root has nothing to rescan."))

;;; Quick Hack
(defun ede-create-lots-of-projects-under-dir (dir projfile &rest attributes)
  "Create a bunch of projects under directory DIR.
PROJFILE is a file name sans directory that indicates a subdirectory
is a project directory.
Generic ATTRIBUTES, such as :include-path can be added.
Note: This needs some work."
  (let ((files (directory-files dir t)))
    (dolist (F files)
      (if (file-exists-p (expand-file-name projfile F))
	  `(ede-cpp-root-project (file-name-nondirectory F)
				 :name (file-name-nondirectory F)
				 :file (expand-file-name projfile F)
				 attributes)))))

(provide 'ede/cpp-root)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/cpp-root"
;; End:

;;; ede/cpp-root.el ends here
