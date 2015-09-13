;;; project.el --- Operations on the current project  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

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

;; This file contains generic infrastructure for dealing with
;; projects, and a number of public functions: finding the current
;; root, related project directories, search path, etc.
;;
;; The goal is to make it easy for Lisp programs to operate on the
;; current project, without having to know which package handles
;; detection of that project type, parsing its config files, etc.

;;; Code:

(require 'cl-generic)

(defvar project-find-functions (list #'project-try-vc
                                     #'project-ask-user)
  "Special hook to find the project containing a given directory.
Each functions on this hook is called in turn with one
argument (the directory) and should return either nil to mean
that it is not applicable, or a project instance.")

(declare-function etags-search-path "etags" ())

(defvar project-search-path-function #'etags-search-path
  "Function that returns a list of source root directories.

The directories in which we can recursively look for the
declarations or other references to the symbols used in the
current buffer.  Depending on the language, it should include the
headers search path, load path, class path, or so on.

The directory names should be absolute.  This variable is
normally set by the major mode.  Used in the default
implementation of `project-search-path'.")

;;;###autoload
(defun project-current (&optional dir)
  "Return the project instance in DIR or `default-directory'."
  (unless dir (setq dir default-directory))
  (run-hook-with-args-until-success 'project-find-functions dir))

;; FIXME: Add MODE argument, like in `ede-source-paths'?
(cl-defgeneric project-search-path (project)
  "Return the list of source root directories.
Any directory roots where source (or header, etc) files used by
the current project may be found, inside or outside of the
current project tree(s).  The directory names should be absolute.

Unless it really knows better, a specialized implementation
should take into account the value returned by
`project-search-path-function' and call
`project-prune-directories' on the result."
  (project-prune-directories
   (append
    ;; We don't know the project layout, like where the sources are,
    ;; so we simply include the roots.
    (project-roots project)
    (funcall project-search-path-function))))

(cl-defgeneric project-roots (project)
  "Return the list of directory roots related to the current project.
It should include the current project root, as well as the roots
of any other currently open projects, if they're meant to be
edited together.  The directory names should be absolute.")

(cl-defgeneric project-ignores (_project _dir)
  "Return the list of glob patterns to ignore inside DIR.
Patterns can match both regular files and directories.
To root an entry, start it with `./'.  To match directories only,
end it with `/'.  DIR must be either one of `project-roots', or
an element of `project-search-path'."
  (require 'grep)
  (defvar grep-find-ignored-files)
  (nconc
   (mapcar
    (lambda (dir)
      (concat dir "/"))
    vc-directory-exclusion-list)
   grep-find-ignored-files))

(defgroup project-vc nil
  "Project implementation using the VC package."
  :group 'tools)

(defcustom project-vc-search-path nil
  "List ot directories to include in `project-search-path'.
The file names can be absolute, or relative to the project root."
  :type '(repeat file)
  :safe 'listp)

(defcustom project-vc-ignores nil
  "List ot patterns to include in `project-ignores'."
  :type '(repeat string)
  :safe 'listp)

(defun project-try-vc (dir)
  (let* ((backend (ignore-errors (vc-responsible-backend dir)))
         (root (and backend (ignore-errors
                              (vc-call-backend backend 'root dir)))))
    (and root (cons 'vc root))))

(cl-defmethod project-roots ((project (head vc)))
  (list (cdr project)))

(cl-defmethod project-search-path ((project (head vc)))
  (append
   (let ((root (cdr project)))
     (mapcar
      (lambda (dir) (expand-file-name dir root))
      (project--value-in-dir 'project-vc-search-path root)))
   (cl-call-next-method)))

(cl-defmethod project-ignores ((project (head vc)) dir)
  (let* ((root (cdr project))
          backend)
    (append
     (when (file-equal-p dir root)
       (setq backend (vc-responsible-backend root))
       (mapcar
        (lambda (entry)
          (if (string-match "\\`/" entry)
              (replace-match "./" t t entry)
            entry))
        (vc-call-backend backend 'ignore-completion-table root)))
     (project--value-in-dir 'project-vc-ignores root)
     (cl-call-next-method))))

(defun project-ask-user (dir)
  (cons 'user (read-directory-name "Project root: " dir nil t)))

(cl-defmethod project-roots ((project (head user)))
  (list (cdr project)))

(defun project-prune-directories (dirs)
  "Returns a copy of DIRS sorted, without subdirectories or non-existing ones."
  (let* ((dirs (sort
                (mapcar
                 (lambda (dir)
                   (file-name-as-directory (expand-file-name dir)))
                 dirs)
                #'string<))
         (ref dirs))
    ;; Delete subdirectories from the list.
    (while (cdr ref)
      (if (string-prefix-p (car ref) (cadr ref))
          (setcdr ref (cddr ref))
        (setq ref (cdr ref))))
    (cl-delete-if-not #'file-exists-p dirs)))

(defun project--value-in-dir (var dir)
  (with-temp-buffer
    (setq default-directory dir)
    (hack-dir-local-variables-non-file-buffer)
    (symbol-value var)))

(provide 'project)
;;; project.el ends here
