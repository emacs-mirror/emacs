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
;; root, related project directories, and library directories.  This
;; list is to be extended in future versions.
;;
;; The goal is to make it easier for Lisp programs to operate on the
;; current project, without having to know which package handles
;; detection of that project type, parsing its config files, etc.

;;; Code:

(require 'cl-generic)

(defvar project-find-functions (list #'project-try-vc)
  "Special hook to find the project containing a given directory.
Each functions on this hook is called in turn with one
argument (the directory) and should return either nil to mean
that it is not applicable, or a project instance.")

;; FIXME: Using the current approach, major modes are supposed to set
;; this variable to a buffer-local value.  So we don't have access to
;; the "library roots" of language A from buffers of language B, which
;; seems desirable in multi-language projects, at least for some
;; potential uses, like "jump to a file in project or library".
;;
;; We can add a second argument to this function: a file extension, or
;; a language name.  Some projects will know the set of languages used
;; in them; for others, like VC-based projects, we'll need
;; auto-detection.  I see two options:
;;
;; - That could be implemented as a separate second hook, with a
;;   list of functions that return file extensions.
;;
;; - This variable will be turned into a hook with "append" semantics,
;;   and each function in it will perform auto-detection when passed
;;   nil instead of an actual file extension.  Then this hook will, in
;;   general, be modified globally, and not from major mode functions.
(defvar project-library-roots-function 'etags-library-roots
  "Function that returns a list of library roots.

It should return a list of directories that contain source files
related to the current buffer.  Depending on the language, it
should include the headers search path, load path, class path,
and so on.

The directory names should be absolute.  Used in the default
implementation of `project-library-roots'.")

;;;###autoload
(defun project-current (&optional maybe-prompt dir)
  "Return the project instance in DIR or `default-directory'.
When no project found in DIR, and MAYBE-PROMPT is non-nil, ask
the user for a different directory to look in."
  (unless dir (setq dir default-directory))
  (let ((pr (project--find-in-directory dir)))
    (cond
     (pr)
     (maybe-prompt
      (setq dir (read-directory-name "Choose the project directory: " dir nil t)
            pr (project--find-in-directory dir))
      (unless pr
        (user-error "No project found in `%s'" dir))))
    pr))

(defun project--find-in-directory (dir)
  (run-hook-with-args-until-success 'project-find-functions dir))

;; FIXME: Add MODE argument, like in `ede-source-paths'?
(cl-defgeneric project-library-roots (project)
  "Return the list of library roots for PROJECT.

It's the list of directories outside of the project that contain
related source files.

Project-specific version of `project-library-roots-function',
which see.  Unless it knows better, a specialized implementation
should use the value returned by that function."
  (project-subtract-directories
   (project-combine-directories
    (funcall project-library-roots-function))
   (project-roots project)))

(cl-defgeneric project-roots (project)
  "Return the list of directory roots belonging to the current project.

Most often it's just one directory, which contains the project
file and everything else in the project.  But in more advanced
configurations, a project can span multiple directories.

The rule of thumb for whether to include a directory here, and not
in `project-library-roots', is whether its contents are meant to
be edited together with the rest of the project.

The directory names should be absolute.")

(cl-defgeneric project-ignores (_project _dir)
  "Return the list of glob patterns to ignore inside DIR.
Patterns can match both regular files and directories.
To root an entry, start it with `./'.  To match directories only,
end it with `/'.  DIR must be one of `project-roots' or
`project-library-roots'."
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

(defcustom project-vc-library-roots nil
  "List ot directories to include in `project-library-roots'.
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

(cl-defmethod project-library-roots ((project (head vc)))
  (project-subtract-directories
   (project-combine-directories
    (append
     (let ((root (cdr project)))
       (mapcar
        (lambda (dir) (file-name-as-directory (expand-file-name dir root)))
        (project--value-in-dir 'project-vc-library-roots root)))
     (funcall project-library-roots-function)))
   (project-roots project)))

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

(defun project-combine-directories (&rest lists-of-dirs)
  "Return a sorted and culled list of directory names.
Appends the elements of LISTS-OF-DIRS together, removes
non-existing directories, as well as directories a parent of
whose is already in the list."
  (let* ((dirs (sort
                (mapcar
                 (lambda (dir)
                   (file-name-as-directory (expand-file-name dir)))
                 (apply #'append lists-of-dirs))
                #'string<))
         (ref dirs))
    ;; Delete subdirectories from the list.
    (while (cdr ref)
      (if (string-prefix-p (car ref) (cadr ref))
          (setcdr ref (cddr ref))
        (setq ref (cdr ref))))
    (cl-delete-if-not #'file-exists-p dirs)))

(defun project-subtract-directories (files dirs)
  "Return a list of elements from FILES that are outside of DIRS.
DIRS must contain directory names."
  ;; Sidestep the issue of expanded/abbreviated file names here.
  (cl-set-difference files dirs :test #'file-in-directory-p))

(defun project--value-in-dir (var dir)
  (with-temp-buffer
    (setq default-directory dir)
    (hack-dir-local-variables-non-file-buffer)
    (symbol-value var)))

(declare-function grep-read-files "grep")
(declare-function xref-collect-matches "xref")
(declare-function xref--show-xrefs "xref")

;;;###autoload
(defun project-find-regexp (regexp)
  "Find all matches for REGEXP in the current project.
With \\[universal-argument] prefix, you can specify the directory
to search in, and the file name pattern to search for."
  (interactive (list (project--read-regexp)))
  (let* ((pr (project-current t))
         (dirs (if current-prefix-arg
                   (list (read-directory-name "Base directory: "
                                              nil default-directory t))
                 (project-roots pr))))
    (project--find-regexp-in dirs regexp pr)))

;;;###autoload
(defun project-or-libraries-find-regexp (regexp)
  "Find all matches for REGEXP in the current project or libraries.
With \\[universal-argument] prefix, you can specify the file name
pattern to search for."
  (interactive (list (project--read-regexp)))
  (let* ((pr (project-current t))
         (dirs (append
                (project-roots pr)
                (project-library-roots pr))))
    (project--find-regexp-in dirs regexp pr)))

(defun project--read-regexp ()
  (defvar xref-identifier-at-point-function)
  (require 'xref)
  (read-regexp "Find regexp"
               (funcall xref-identifier-at-point-function)))

(defun project--find-regexp-in (dirs regexp project)
  (require 'grep)
  (let* ((files (if current-prefix-arg
                    (grep-read-files regexp)
                  "*"))
         (xrefs (cl-mapcan
                 (lambda (dir)
                   (xref-collect-matches regexp files dir
                                         (project-ignores project dir)))
                 dirs)))
    (unless xrefs
      (user-error "No matches for: %s" regexp))
    (xref--show-xrefs xrefs nil)))

(provide 'project)
;;; project.el ends here
