;;; ede/config.el --- Configuration Handler baseclass

;; Copyright (C) 2014-2018 Free Software Foundation, Inc.

;; Author: Eric Ludlam <eric@siege-engine.com>

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
;; Some auto-detecting projects (such as the 'generic' project type)
;; can be enhanced by also saving a configuration file that is EDE
;; specific.  EDE will be able to load that configuration from the save
;; file as a way of augmenting what is normally already detected.
;;
;; How To Use:
;;
;; Subclass `ede-extra-config', and add the features you want to use.
;; Several mixins are available for adding in C++ or Java support.  Bring
;; in the pieces you need.
;;
;; Your project and targets should all have a common baseclass from
;; `ede-project-with-config' or `ede-target-with-config'.  When
;; subclassing the project, be sure to override the class allocated
;; slots for the `config-class'.  This will tie your new project to
;; the new configuration type.
;;
;; You can also override the file name used to save the configuration
;; object in.
;;
;; If you need to take special action in `project-rescan' be sure to also
;; call `call-next-method' to also get the configuration rescanned.
;;
;; Note on config file safety:
;;
;; Normally an EDE project that loads a save file should have it's
;; autoload slot :safe-p set to nil.  Projects who save data via
;; config.el can mark their project as :safe-p t.  The config system will
;; do the queries needed to protect the user.  This allows a generic
;; project to become active in cases where no save file exists, nor is
;; needed.

;;; Code:
(require 'ede)

;;; CONFIG
;;
;; This is the base of a configuration class supported by the
;; `ede-project-with-config' baseclass.
;;
(defclass ede-extra-config (eieio-persistent)
  ((extension :initform ".ede")
   (file-header-line :initform ";; EDE Project Configuration")
   (project :type ede-project-with-config-child
	    :documentation
	    "The project this config is bound to.")
   (ignored-file :initform nil
		 :type (or null symbol)
		 :documentation
		 "Set to non-nil if this was created and an on-disk file
was ignored.  Use this to warn the user that they might want to load in
an on-disk version.")
   )
  "Baseclass for auxiliary configuration files for EDE.
This should be subclassed by projects that auto detect a project
and also want to save some extra level of configuration.")

;;; PROJECT BASECLASS
;;
;; Subclass this baseclass if you want your EDE project to also
;; support saving an extra configuration file of unique data
;; needed for this project.
;;
(defclass ede-project-with-config (ede-project)
  ((menu :initform nil)
   (config-file-basename
    :initform "Config.ede"
    :allocation :class
    :type string
    :documentation
    "The filename to use for saving the configuration.
This filename excludes the directory name and is used to
initialize the :file slot of the persistent baseclass.")
   (config-class
    :initform ede-extra-config
    :allocation :class
    :type class
    :documentation
    "The class of the configuration used by this project.")
   (config :initform nil
	   :type (or null ede-extra-config-child)
	   :documentation
	   "The configuration object for this project.")
   )
  "Baseclass for projects that save a configuration.")

(defclass ede-target-with-config (ede-target)
  ()
  "Baseclass for targets of classes that use a config object.")

;;; Rescanning

(cl-defmethod project-rescan ((this ede-project-with-config))
  "Rescan this generic project from the sources."
  ;; Force the config to be rescanned.
  (oset this config nil)
  ;; Ask if it is safe to load the config from disk.
  (ede-config-get-configuration this t)
  )

;;; Project Methods for configuration

(cl-defmethod ede-config-get-configuration ((proj ede-project-with-config) &optional loadask)
  "Return the configuration for the project PROJ.
If optional LOADASK is non-nil, then if a project file exists, and if
the directory isn't on the `safe' list, ask to add it to the safe list."
  (let ((config (oref proj config)))

    ;; If the request is coming at a time when we want to ask the user,
    ;; and there already is a configuration, AND the last time we ignored
    ;; the on-file version we did so automatically (without asking) then
    ;; in theory there are NO mods to this config, and we should re-ask,
    ;; and possibly re-load.
    (when (and loadask config (eq (oref config ignored-file) 'auto))
      (setq config nil))

    (when (not config)
      (let* ((top (oref proj :directory))
	     (fname (expand-file-name (oref proj config-file-basename) top))
	     (class (oref proj config-class))
	     (ignore-type nil))
	(if (and (file-exists-p fname)
		 (or (ede-directory-safe-p top)
		     ;; Only force the load if someone asked.
		     (and loadask (ede-check-project-directory top))))
	    ;; Load in the configuration
	    (setq config (eieio-persistent-read fname class))
	  ;; If someone said not to load stuff from here then
	  ;; pop up a warning.
	  (when (file-exists-p fname)
	    (message "Ignoring EDE config file for now and creating a new one.  Use C-c . g to load it.")
	    ;; Set how it was ignored.
	    (if loadask
		(setq ignore-type 'manual)
	      (setq ignore-type 'auto))
	    )
	  ;; Create a new one.
	  (setq config (make-instance class
				      "Configuration"
				      :file fname))
	  (oset config ignored-file ignore-type)

	  ;; Set initial values based on project.
	  (ede-config-setup-configuration proj config))
	;; Link things together.
	(oset proj config config)
	(oset config project proj)))
    config))

(cl-defmethod ede-config-setup-configuration ((proj ede-project-with-config) config)
  "Default configuration setup method."
  nil)

(cl-defmethod ede-commit-project ((proj ede-project-with-config))
  "Commit any change to PROJ to its file."
  (let ((config (ede-config-get-configuration proj)))
    (ede-commit config)))

;;; Customization
;;
(cl-defmethod ede-customize ((proj ede-project-with-config))
  "Customize the EDE project PROJ by actually configuring the config object."
  (let ((config (ede-config-get-configuration proj t)))
    (eieio-customize-object config)))

(cl-defmethod ede-customize ((target ede-target-with-config))
  "Customize the EDE TARGET by actually configuring the config object."
  ;; Nothing unique for the targets, use the project.
  (ede-customize-project))

(cl-defmethod eieio-done-customizing ((config ede-extra-config))
  "Called when EIEIO is done customizing the configuration object.
We need to go back through the old buffers, and update them with
the new configuration."
  (ede-commit config)
  ;; Loop over all the open buffers, and re-apply.
  (ede-map-targets
   (oref config project)
   (lambda (target)
     (ede-map-target-buffers
      target
      (lambda (b)
	(with-current-buffer b
	  (ede-apply-target-options)))))))

(cl-defmethod ede-commit ((config ede-extra-config))
  "Commit all changes to the configuration to disk."
  ;; So long as the user is trying to safe this config, make sure they can
  ;; get at it again later.
  (let ((dir (file-name-directory (oref config file))))
    (ede-check-project-directory dir))

  (eieio-persistent-save config))

;;; PROJECT MIXINS
;;
;; These are project part mixins.  Use multiple inheritance for each
;; piece of these configuration options you would like to have as part
;; of your project.

;;; PROGRAM
;; If there is a program that can be run or debugged that is unknown
;; and needs to be configured.
(defclass ede-extra-config-program ()
  ((debug-command :initarg :debug-command
		  :initform "gdb "
		  :type string
		  :group commands
		  :custom string
		  :group (default build)
		  :documentation
		  "Command used for debugging this project.")
   (run-command :initarg :run-command
		:initform ""
		:type string
		:group commands
		:custom string
		:group (default build)
		:documentation
		"Command used to run something related to this project."))
  "Class to mix into a configuration for debug/run of programs.")

(defclass ede-project-with-config-program ()
  ()
  "Class to mix into a project with configuration for programs.")

(defclass ede-target-with-config-program ()
  ()
  "Class to mix into a project with configuration for programs.
This class brings in method overloads for running and debugging
programs from a project.")

(cl-defmethod project-debug-target ((target ede-target-with-config-program))
  "Run the current project derived from TARGET in a debugger."
  (let* ((proj (ede-target-parent target))
	 (config (ede-config-get-configuration proj t))
	 (debug (oref config :debug-command))
	 (cmd (read-from-minibuffer
	       "Debug Command: "
	       debug))
	 (cmdsplit (split-string cmd " " t))
	 ;; @TODO - this depends on the user always typing in something good
	 ;;  like "gdb" or "dbx" which also exists as a useful Emacs command.
	 ;;  Is there a better way?
	 (cmdsym (intern-soft (car cmdsplit))))
    (call-interactively cmdsym t)))

(declare-function ede-shell-run-something "ede/shell")

(cl-defmethod project-run-target ((target ede-target-with-config-program))
  "Run the current project derived from TARGET."
  (let* ((proj (ede-target-parent target))
	 (config (ede-config-get-configuration proj t))
	 (run (concat "./" (oref config :run-command)))
	 (cmd (read-from-minibuffer "Run (like this): " run)))
    (ede-shell-run-something target cmd)))

;;; BUILD
;; If the build style is unknown and needs to be configured.
(defclass ede-extra-config-build ()
  ((build-command :initarg :build-command
		  :initform "make -k"
		  :type string
		  :group commands
		  :custom string
		  :group (default build)
		  :documentation
		  "Command used for building this project."))
  "Class to mix into a configuration for compilation.")

(defclass ede-project-with-config-build ()
  ()
  "Class to mix into a project with configuration for builds.
This class brings in method overloads for building.")

(defclass ede-target-with-config-build ()
  ()
  "Class to mix into a project with configuration for builds.
This class brings in method overloads for for building.")

(cl-defmethod project-compile-project ((proj ede-project-with-config-build) &optional command)
  "Compile the entire current project PROJ.
Argument COMMAND is the command to use when compiling."
  (let* ((config (ede-config-get-configuration proj t))
	 (comp (oref config :build-command)))
    (compile comp)))

(cl-defmethod project-compile-target ((obj ede-target-with-config-build) &optional command)
  "Compile the current target OBJ.
Argument COMMAND is the command to use for compiling the target."
  (project-compile-project (ede-current-project) command))

;;; C / C++
;; Configure includes and preprocessor symbols for C/C++ needed by
;; Semantic.
(defclass ede-extra-config-c ()
  ((c-include-path :initarg :c-include-path
		   :initform nil
		   :type list
		   :custom (repeat (string :tag "Path"))
		   :group c
		   :documentation
		   "The include path used by C/C++ projects.
The include path is used when searching for symbols.")
   (c-preprocessor-table :initarg :c-preprocessor-table
			 :initform nil
			 :type list
			 :custom (repeat (cons (string :tag "Macro")
					       (string :tag "Value")))
			 :group c
			 :documentation
			 "Preprocessor Symbols for this project.
When files within this project are parsed by CEDET, these symbols will be
used to resolve macro occurrences in source files.
If you modify this slot, you will need to force your source files to be
parsed again.")
   (c-preprocessor-files :initarg :c-preprocessor-files
			 :initform nil
			 :type list
			 :group c
			 :custom (repeat (string :tag "Include File"))
			 :documentation
			 "Files parsed and used to populate preprocessor tables.
When files within this project are parsed by CEDET, these symbols will be used to
resolve macro occurrences in source files.
If you modify this slot, you will need to force your source files to be
parsed again."))
  "Class to mix into a configuration for compilation.")

(defclass ede-project-with-config-c ()
  ()
  "Class to mix into a project for C/C++ support.")

(defclass ede-target-with-config-c ()
  ()
  "Class to mix into a project for C/C++ support.
This target brings in methods used by Semantic to query
the preprocessor map, and include paths.")

(declare-function semanticdb-file-table-object "semantic/db"
		  (file &optional dontload))
(declare-function semanticdb-needs-refresh-p "semantic/db" (arg &rest args))
(declare-function semanticdb-refresh-table "semantic/db" (arg &rest args))

(cl-defmethod ede-preprocessor-map ((this ede-target-with-config-c))
  "Get the pre-processor map for some generic C code."
  (require 'semantic/sb)
  (let* ((proj (ede-target-parent this))
	 (root (ede-project-root proj))
	 (config (ede-config-get-configuration proj))
	 filemap
	 )
    ;; Preprocessor files
    (dolist (G (oref config :c-preprocessor-files))
      (let ((table (semanticdb-file-table-object
		    (ede-expand-filename root G))))
	(when table
	  (when (semanticdb-needs-refresh-p table)
	    (semanticdb-refresh-table table))
	  (setq filemap (append filemap (oref table lexical-table)))
	  )))
    ;; The core table
    (setq filemap (append filemap (oref config :c-preprocessor-table)))

    filemap
    ))

(cl-defmethod ede-system-include-path ((this ede-target-with-config-c))
  "Get the system include path used by project THIS."
  (let* ((proj (ede-target-parent this))
	(config (ede-config-get-configuration proj)))
    (oref config c-include-path)))

;;; Java
;; Configuration needed for programming with Java.
(defclass ede-extra-config-java ()
  ()
  "Class to mix into a configuration for compilation.")

(defclass ede-project-with-config-java ()
  ()
  "Class to mix into a project to support java.
This brings in methods to support Semantic querying the
java class path.")

(defclass ede-target-with-config-java ()
  ()
  "Class to mix into a project to support java.")

(cl-defmethod ede-java-classpath ((proj ede-project-with-config-java))
  "Return the classpath for this project."
  (oref (ede-config-get-configuration proj) :classpath))

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/config"
;; End:

(provide 'ede/config)

;;; ede/config.el ends here
