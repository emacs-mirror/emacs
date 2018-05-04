;;; ede/linux.el --- Special project for Linux

;; Copyright (C) 2008-2018 Free Software Foundation, Inc.

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
;; Provide a special project type just for Linux, cause Linux is special.
;;
;; Identifies a Linux project automatically.
;; Speedy ede-expand-filename based on extension.
;; Pre-populates the preprocessor map from lisp.h
;;
;; ToDo :
;; * Add "build" options.
;; * Add texinfo lookup options.
;; * Add website

(require 'ede)
(require 'ede/make)
(eval-when-compile (require 'cl-lib))

(declare-function semanticdb-file-table-object "semantic/db")
(declare-function semanticdb-needs-refresh-p "semantic/db")
(declare-function semanticdb-refresh-table "semantic/db")

;;; Code:
(defgroup project-linux nil
  "File and tag browser frame."
  :group 'tools
  :group 'ede
  :version "24.3")

(defcustom project-linux-build-directory-default 'ask
  "Build directory."
  :version "24.4"
  :group 'project-linux
  :type '(choice (const :tag "Same as source directory" same)
                 (const :tag "Ask the user" ask)))

(defcustom project-linux-architecture-default 'ask
  "Target architecture to assume when not auto-detected."
  :version "24.4"
  :group 'project-linux
  :type '(choice (string :tag "Architecture name")
                 (const :tag "Ask the user" ask)))


(defcustom project-linux-compile-target-command (concat ede-make-command " -k -C %s SUBDIRS=%s")
  "Default command used to compile a target."
  :group 'project-linux
  :type 'string)

(defcustom project-linux-compile-project-command (concat ede-make-command " -k -C %s")
  "Default command used to compile a project."
  :group 'project-linux
  :type 'string)

(defun ede-linux-version (dir)
  "Find the Linux version for the Linux src in DIR."
  (let ((buff (get-buffer-create " *linux-query*")))
    (with-current-buffer buff
      (erase-buffer)
      (setq default-directory (file-name-as-directory dir))
      (insert-file-contents "Makefile" nil 0 512)
      (goto-char (point-min))
      (let (major minor sub)
	(re-search-forward "^VERSION *= *\\([0-9.]+\\)")
	(setq major (match-string 1))
	(re-search-forward "^PATCHLEVEL *= *\\([0-9.]+\\)")
	(setq minor (match-string 1))
	(re-search-forward "^SUBLEVEL *= *\\([0-9.]+\\)")
	(setq sub (match-string 1))
	(prog1
	    (concat major "." minor "." sub)
	  (kill-buffer buff)
	  )))))

(defclass ede-linux-project (ede-project)
  ((build-directory :initarg :build-directory
                    :type string
                    :documentation "Build directory.")
   (architecture :initarg :architecture
                 :type string
                 :documentation "Target architecture.")
   (include-path :initarg :include-path
                 :type list
                 :documentation "Include directories.
Contains both common and target architecture-specific directories."))
  "Project Type for the Linux source code."
  :method-invocation-order :depth-first)


(defun ede-linux--get-build-directory (dir)
  "Detect build directory for sources in DIR.
If DIR has not been used as a build directory, fall back to
`project-linux-build-directory-default'."
  (or
   ;; detected build on source directory
   (and (file-exists-p (expand-file-name ".config" dir)) dir)
   ;; use configuration
   (cl-case project-linux-build-directory-default
     (same dir)
     (ask (read-directory-name "Select Linux' build directory: " dir)))))


(defun ede-linux--get-archs (dir)
  "Returns a list of architecture names found in DIR."
  (let ((archs-dir (expand-file-name "arch" dir))
        archs)
    (when (file-directory-p archs-dir)
      (mapc (lambda (elem)
              (when (and
                     (not (string= elem "."))
                     (not (string= elem ".."))
                     (not (string= elem "x86_64")) ; has no separate sources
                     (file-directory-p
                      (expand-file-name elem archs-dir)))
                (add-to-list 'archs elem t)))
            (directory-files archs-dir)))
    archs))


(defun ede-linux--detect-architecture (dir)
  "Try to auto-detect the architecture as configured in DIR.
DIR is Linux' build directory. If it cannot be auto-detected,
returns `project-linux-architecture-default'."
  (let ((archs-dir (expand-file-name "arch" dir))
        (archs (ede-linux--get-archs dir))
        arch found)
    (or (and
         archs
         ;; Look for /arch/<arch>/include/generated
         (progn
           (while (and archs (not found))
             (setq arch (car archs))
             (when (file-directory-p
                    (expand-file-name (concat arch "/include/generated")
                                      archs-dir))
               (setq found arch))
             (setq archs (cdr archs)))
           found))
       project-linux-architecture-default)))

(defun ede-linux--get-architecture (dir bdir)
  "Try to auto-detect the architecture as configured in BDIR.
Uses `ede-linux--detect-architecture' for the auto-detection. If
the result is `ask', let the user choose from architectures found
in DIR."
  (let ((arch (ede-linux--detect-architecture bdir)))
    (cl-case arch
      (ask
       (completing-read "Select target architecture: "
                        (ede-linux--get-archs dir)))
      (t arch))))


(defun ede-linux--include-path (dir bdir arch)
  "Returns a list with include directories.
Returned directories might not exist, since they are not created
until Linux is built for the first time."
  (cl-map 'list
       (lambda (elem) (format (concat (car elem) "/" (cdr elem)) arch))
       ;; XXX: taken from the output of "make V=1"
       (list (cons  dir "arch/%s/include")
             (cons bdir "arch/%s/include/generated")
             (cons  dir "include")
             (cons bdir "include")
             (cons  dir "arch/%s/include/uapi")
             (cons bdir "arch/%s/include/generated/uapi")
             (cons  dir "include/uapi")
             (cons bdir "include/generated/uapi"))))

;;;###autoload
(defun ede-linux-load (dir &optional _rootproj)
  "Return an Linux Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  ;; Doesn't already exist, so let's make one.
  (let* ((bdir (ede-linux--get-build-directory dir))
	 (arch (ede-linux--get-architecture dir bdir))
	 (include-path (ede-linux--include-path dir bdir arch)))
    (make-instance 'ede-linux-project
     :name "Linux"
     :version (ede-linux-version dir)
     :directory (file-name-as-directory dir)
     :file (expand-file-name "scripts/ver_linux"
			     dir)
     :build-directory bdir
     :architecture arch
     :include-path include-path)))

;;;###autoload
(ede-add-project-autoload
 (make-instance 'ede-project-autoload
                :name "LINUX ROOT"
                :file 'ede/linux
                :proj-file "scripts/ver_linux"
                :load-type 'ede-linux-load
                :class-sym 'ede-linux-project
                :new-p nil
                :safe-p t)
 'unique)

(defclass ede-linux-target-c (ede-target)
  ()
  "EDE Linux Project target for C code.
All directories need at least one target.")

(defclass ede-linux-target-misc (ede-target)
  ()
  "EDE Linux Project target for Misc files.
All directories need at least one target.")

(cl-defmethod initialize-instance ((this ede-linux-project)
                                   &rest _fields)
  "Make sure the targets slot is bound."
  (cl-call-next-method)
  (unless (slot-boundp this 'targets)
    (oset this :targets nil)))

;;; File Stuff
;;
(cl-defmethod ede-project-root-directory ((this ede-linux-project)
                                          &optional _file)
  "Return the root for THIS Linux project with file."
  (ede-up-directory (file-name-directory (oref this file))))

(cl-defmethod ede-project-root ((this ede-linux-project))
  "Return my root."
  this)

(cl-defmethod ede-find-subproject-for-directory ((proj ede-linux-project)
                                                 _dir)
  "Return PROJ, for handling all subdirs below DIR."
  proj)

;;; TARGET MANAGEMENT
;;
(defun ede-linux-find-matching-target (class dir targets)
  "Find a target that is a CLASS and is in DIR in the list of TARGETS."
  (let ((match nil))
    (dolist (T targets)
      (when (and (object-of-class-p T class)
		 (string= (oref T path) dir))
	(setq match T)
      ))
    match))

(cl-defmethod ede-find-target ((proj ede-linux-project) buffer)
  "Find an EDE target in PROJ for BUFFER.
If one doesn't exist, create a new one for this directory."
  (let* ((ext (file-name-extension (buffer-file-name buffer)))
	 (cls (cond ((not ext)
		     'ede-linux-target-misc)
		    ((string-match "c\\|h" ext)
		     'ede-linux-target-c)
		    (t 'ede-linux-target-misc)))
	 (targets (oref proj targets))
	 (dir default-directory)
	 (ans (ede-linux-find-matching-target cls dir targets))
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
(cl-defmethod ede-preprocessor-map ((this ede-linux-target-c))
  "Get the pre-processor map for Linux C code.
All files need the macros from lisp.h!"
  (require 'semantic/db)
  (let* ((proj (ede-target-parent this))
	 (root (ede-project-root proj))
	 (versionfile (ede-expand-filename root "include/linux/version.h"))
	 (table (when (and versionfile (file-exists-p versionfile))
		  (semanticdb-file-table-object versionfile)))
	 (filemap '( ("__KERNEL__" . "")
		     ))
	 )
    (when table
      (when (semanticdb-needs-refresh-p table)
	(semanticdb-refresh-table table))
      (setq filemap (append filemap (oref table lexical-table)))
      )
    filemap
    ))

(defun ede-linux-file-exists-name (name root subdir)
  "Return a file name if NAME exists under ROOT with SUBDIR in between."
  (let ((F (expand-file-name name (expand-file-name subdir root))))
    (when (file-exists-p F) F)))

(cl-defmethod ede-expand-filename-impl ((proj ede-linux-project) name)
  "Within this project PROJ, find the file NAME.
Knows about how the Linux source tree is organized."
  (let* ((ext (file-name-extension name))
         (root (ede-project-root proj))
         (dir (ede-project-root-directory root))
         (bdir (oref proj build-directory))
         (F (cond
             ((not ext) nil)
             ((string-match "h" ext)
              (let ((dirs (oref proj include-path))
                    found)
                (while (and dirs (not found))
                  (setq found
                        (or (ede-linux-file-exists-name name bdir (car dirs))
                           (ede-linux-file-exists-name name dir (car dirs))))
                  (setq dirs (cdr dirs)))
                found))
             ((string-match "txt" ext)
              (ede-linux-file-exists-name name dir "Documentation"))
             (t nil))))
    (or F (cl-call-next-method))))

;;; Command Support
;;
(cl-defmethod project-compile-project ((proj ede-linux-project)
				    &optional command)
  "Compile the entire current project.
Argument COMMAND is the command to use when compiling."
  (let* ((dir (ede-project-root-directory proj)))

    (require 'compile)
    (if (not project-linux-compile-project-command)
	(setq project-linux-compile-project-command compile-command))
    (if (not command)
	(setq command
	      (format
	       project-linux-compile-project-command
	       dir)))

    (compile command)))

(cl-defmethod project-compile-target ((obj ede-linux-target-c) &optional command)
  "Compile the current target.
Argument COMMAND is the command to use for compiling the target."
  (let* ((proj (ede-target-parent obj))
	 (root (ede-project-root proj))
	 (dir (ede-project-root-directory root))
	 (subdir (oref obj path)))

    (require 'compile)
    (if (not project-linux-compile-project-command)
	(setq project-linux-compile-project-command compile-command))
    (if (not command)
	(setq command
	      (format
	       project-linux-compile-target-command
	       dir subdir)))

    (compile command)))

(cl-defmethod project-rescan ((this ede-linux-project))
  "Rescan this Linux project from the sources."
  (let* ((dir (ede-project-root-directory this))
	 (bdir (ede-linux--get-build-directory dir))
	 (arch (ede-linux--get-architecture dir bdir))
	 (inc (ede-linux--include-path dir bdir arch))
	 (ver (ede-linux-version dir)))
    (oset this version ver)
    (oset this :build-directory bdir)
    (oset this :architecture arch)
    (oset this :include-path inc)
    ))

(provide 'ede/linux)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/linux"
;; End:

;;; ede/linux.el ends here
