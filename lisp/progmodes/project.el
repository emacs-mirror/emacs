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
  "Function that returns a list of source directories.

The directories in which we can look for the declarations or
other references to the symbols used in the current buffer.
Depending on the language, it should include the headers search
path, load path, class path, and so on.

The directory names should be absolute.  Normally set by the
major mode.  Used in the default implementation of
`project-search-path'.")

;;;###autoload
(defun project-current (&optional dir)
  "Return the project instance in DIR or `default-directory'."
  (unless dir (setq dir default-directory))
  (run-hook-with-args-until-success 'project-find-functions dir))

(cl-defgeneric project-root (project)
  "Return the root directory of the current project.
The directory name should be absolute.")

(cl-defgeneric project-search-path (project)
  "Return the list of source directories.
Including any where source (or header, etc) files used by the
current project may be found, inside or outside of the project
tree.  The directory names should be absolute.

A specialized implementation should use the value
`project-search-path-function', or, better yet, call and combine
the results from the functions that this value is set to by all
major modes used in the project.  Alternatively, it can return a
user-configurable value."
  (project--prune-directories
   (nconc (funcall project-search-path-function)
          ;; Include these, because we don't know any better.
          ;; But a specialized implementation may include only some of
          ;; the project's subdirectories, if there are no source
          ;; files at the top level.
          (project-directories project))))

(cl-defgeneric project-directories (project)
  "Return the list of directories related to the current project.
It should include the current project root, as well as the roots
of any currently open related projects, if they're meant to be
edited together.  The directory names should be absolute."
  (list (project-root project)))

(cl-defgeneric project-ignores (_project)
  "Return the list of glob patterns that match ignored files.
To root an entry, start it with `./'.  To match directories only,
end it with `/'."
  (require 'grep)
  (defvar grep-find-ignored-files)
  (nconc
   (mapcar
    (lambda (dir)
      (concat dir "/"))
    vc-directory-exclusion-list)
   grep-find-ignored-files))

(defun project-try-vc (dir)
  (let* ((backend (ignore-errors (vc-responsible-backend dir)))
         (root (and backend (ignore-errors
                              (vc-call-backend backend 'root dir)))))
    (and root (cons 'vc root))))

(cl-defmethod project-root ((project (head vc)))
  (cdr project))

(cl-defmethod project-ignores ((project (head vc)))
  (nconc
   (let* ((dir (cdr project))
          (backend (vc-responsible-backend dir)))
     (mapcar
      (lambda (entry)
        (if (string-match "\\`/" entry)
            (replace-match "./" t t entry)
          entry))
      (vc-call-backend backend 'ignore-completion-table dir)))
   (cl-call-next-method)))

(defun project-ask-user (dir)
  (cons 'user (read-directory-name "Project root: " dir nil t)))

(cl-defmethod project-root ((project (head user)))
  (cdr project))

(defun project--prune-directories (dirs)
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

(provide 'project)
;;; project.el ends here
