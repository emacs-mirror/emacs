(defpackage :lem-core/commands/project
  (:use :cl :lem-core :lem-core/commands/file)
  (:export :*root-directories*
           :*root-files*
           :*delete-repl-buffer*
           :*delete-last-buffer*
           :root-p
           :find-root
           :saved-projects
           :project-find-file
           :project-root-directory
           :project-root
           :project-kill-buffers
           :project-switch
           :project-save
           :project-save-prompt
           :project-unsave)
  #+sbcl
  (:lock t)
  (:documentation "Defines utilities to find a project root directory and related user-facing commands: project-find-file, project-kill-buffers, project-switch etc."))

(in-package :lem-core/commands/project)

(define-key *global-keymap* "C-x p f" 'project-find-file)
(define-key *global-keymap* "C-x p d" 'project-root-directory)
(define-key *global-keymap* "C-x p K" 'project-kill-buffers)
(define-key *global-keymap* "C-x p p" 'project-switch)
(define-key *global-keymap* "C-x p s" 'project-save)
(define-key *global-keymap* "C-x p u" 'project-unsave)

(defvar *root-directories*
  (list
   ".git"
   ".hg"
   "_FOSSIL_"
   ".bzr"
   "_darcs"
   )
  "A list of directory names to be seen as project roots.
  Sans trailing slash.")

(defvar *root-files*
  (list
   ".project"
   ".projectile"
   ".fslckout"
   "TAGS"
   "configure.ac"
   "configure.in"
   ".ignore"
   "Makefile"
   )
  "A list of files to be seen as project roots.")

(defvar *delete-repl-buffer* t
  "If non-t, don't delete the Lisp REPL.")
(defvar *delete-last-buffer* nil
  "If non-nil, delete Lem's last buffer. This makes Lem exit.")

(define-condition buffer-deleted-p ()
  ()
  (:documentation "Signaled when a command successfully deletes a buffer."))

(defun parent-directory (pathname)
  "Return the parent directory (pathname)."
  (uiop:pathname-parent-directory-pathname pathname))

(defun add-directory-separator (pathname)
  "Add a directory separator, according to the host: a slash on Linux, something else on other popular platforms."
  ;; a little back and force from a pathname, to a string, back to a pathname...
  (pathname
   (str:concat pathname (string (uiop:directory-separator-for-host)))))

(defun list-file-names (pathname)
  "List files in this `pathname'.
  Return a list of file names, as strings, sans the full path.

  (list-file-names \"./\")
  => (README.md lem.asd ...)"
  (mapcar #'file-namestring
          (uiop:directory-files pathname)))

(defun list-subdirectories (pathname)
  "Return a list of subdirectories (pathnames)."
  (uiop:subdirectories pathname))

(defun file-root-p (pathname)
  "Return t if this pathname contains any root markers from `*root-files*'."
  (when (intersection *root-files* (list-file-names pathname) :test #'equalp)
    t))

(defun directory-root-p (pathname)
  "Return t if this pathname contains any directory markers from `*root-directories*'."
  ;; Here we work with pathnames, not strings.
  (loop for marker in *root-directories*
        with subdirectories = (list-subdirectories pathname)
        ;; To use merge-pathnames, our subdirectory must end with a trailing /
        for subpath = (uiop:merge-pathnames*
                       ;; (try to) handle non-unix platforms path separators.
                       (add-directory-separator marker) pathname)
        if (member subpath subdirectories :test #'equal)
          return t
        finally (return)))

(defvar *recurse* t)

(defun find-root (pathname &key (recurse *recurse*) (recursing nil))
  "Search for a root directory up the file hierarchy.
  Return a pathname, or NIL.

  root: a buffer directory (pathname).
  recurse: if non T, do not recurse in parent directories (testing only)."
  ;; recursing: when calling find-root on the HOME directory, it's OK to return it as the root.
  ;; Otherwise, return nil (to get the caller directory).
  (check-type pathname (or pathname string))
  (setf pathname (pathname pathname))
  (cond
    ;; Stop at /
    ((equal (directory-namestring pathname)
            "/")
     (if recursing nil pathname))
    ;; Stop at HOME.
    ((equal (directory-namestring pathname)
            (directory-namestring (user-homedir-pathname)))
     (if recursing nil pathname))
    ;; Stop if found a directory root.
    ((directory-root-p pathname)
     pathname)
    ;; Stop if found a file root.
    ((file-root-p pathname)
     pathname)
    ;; Go up to the parent directory.
    (t
     (when recurse
       (or
        (find-root (parent-directory pathname) :recurse recurse :recursing t)
        pathname)))))

(define-command project-find-file (arg) (:universal)
  "Open a file, from the list of all files in this project."
  ;; ARG is currently not used, use it when needed.
  (declare (ignorable arg))
  (let* ((cwd (buffer-directory))
         (project-root (find-root cwd))
         (root (or project-root cwd)))
    (uiop:with-current-directory (root)
      (let ((filename (prompt-for-files-recursively))
            buffer)
        (when filename
          (setf buffer (execute-find-file *find-file-executor*
                                          (get-file-mode filename)
                                          filename))
          (when buffer
            (switch-to-buffer buffer t nil)))))))

(define-command project-root () ()
  "Display this buffer's project directory."
  (let* ((cwd (buffer-directory))
         (project-root (find-root cwd))
         (root (or project-root cwd)))
    (message "Current project root: ~a" root)))

(define-command project-root-directory (arg) (:universal)
  "Open this project's root directory."
  (declare (ignorable arg))
  (let* ((cwd (buffer-directory))
         (project-root (find-root cwd))
         (root (or project-root cwd)))
    (find-file root)))



(defun list-project-buffers (&optional (root (find-root (buffer-directory))) (buffers (buffer-list)))
  "List all buffers pertaining to this project root."
  (assert buffers)
  (loop for buffer in buffers
        with root = (namestring root)
        for project = (namestring (find-root (buffer-directory buffer)))
        if (equal project root)
          collect buffer))

(defun maybe-delete-repl-buffer (buffer)
  (if *delete-repl-buffer*
      (progn
        (delete-buffer buffer)
        (signal 'buffer-deleted-p))
      (message "Keeping the REPL buffer. You can change this by setting lem-core/commands/project::*delete-repl-buffer* to nil.")))

(defun maybe-delete-last-buffer (buffer)
  (if *delete-last-buffer*
      (progn
        (delete-buffer buffer)
        (signal 'buffer-deleted-p))
      (message "Keeping the last buffer. You can change this by setting lem-core/commands/project::*delete-last-buffer* to t.")))

(defmethod buffer-repl-p (buffer)
  "Return t if this buffer is named after a Lisp REPL."
  (equal "*lisp-repl*" (buffer-name buffer)))

(defun delete-buffers (&optional (buffers (list-project-buffers)))
  "Delete these buffers, except:

  - by default, don't delete the last existing buffer, this would cause Lem to exit.
    see *delete-last-buffer*
  - unless told otherwise, delete the Lisp REPL.
    see *delete-repl-buffer*."
  (loop for buffer in buffers
        with all-buffers = (buffer-list)
        with all-count = (length all-buffers)
        for i = all-count then (decf i)
        ;; Deleting the very last buffer makes Lem quit, so we don't do it by default.
        if (= 1 i)
          do (maybe-delete-last-buffer buffer)
        else
          ;; We might want to keep the REPL buffer around,
          ;; even if it seems that by deleting it we don't loose its history.
          do (if (buffer-repl-p buffer)
                 (maybe-delete-repl-buffer buffer)
                 (progn
                   (delete-buffer buffer)
                   (signal 'buffer-deleted-p)))))

(define-command project-kill-buffers () ()
  "Delete all this project's buffers, except:

  - if *delete-repl-buffer* is non t, we don't delete the REPL buffer.
  - if *delete-last-buffer* is non nil, we will delete the last buffer. This would cause Lem to exit."
  (let ((count 0))
    (handler-bind ((buffer-deleted-p (lambda (c)
                                       (declare (ignore c))
                                       (incf count))))
      (delete-buffers (list-project-buffers)))
    (message "~a buffers deleted." count)))



(defvar *projects-history*)

(defun history ()
  "Return or create the projects' history struct.
  The history file is saved on (lem-home)/history/projects"
  (unless (boundp '*projects-history*)
    (let* ((pathname (merge-pathnames "history/projects" (lem-home)))
           (history (lem/common/history:make-history :pathname pathname)))
      (setf *projects-history* history)))
  *projects-history*)

(defun remember-project (input)
  "Add this project path to the history file.
  Return t if the project was added, nil if it already existed."
  (let* ((history (history))
         ;; Project roots (pathnames) are converted to strings for the
         ;; string completion prompt.
         (input (namestring input)))
    (if (and (lem/common/history:add-history history input :allow-duplicates nil)
             (lem/common/history:save-file history))
        (message "Saved project: ~A" input)
        (message "Failed saving project: ~A" input))))

(defun forget-project (input)
  "Remove this project (string) from the projects' history file.
  Return the input."
  (let ((history (history)))
    (lem/common/history:remove-history history input)
    (lem/common/history:save-file history)
    input))

(defun saved-projects ()
  "Return the saved projects.
  Return: a list (of strings), not a vector."
  (lem/common/history:history-data-list (history)))

(define-command project-save () ()
  "Remember the current project to the projects list."
  (remember-project (find-root (buffer-directory))))

(define-command project-save-prompt () ()
  "Prompts for a directory to save to the projects list."
  (remember-project (prompt-for-directory "Save Project: " :directory (buffer-directory))))

(define-command project-unsave () ()
  "Prompt for a project and remove it from the list of saved projects."
  (let ((choice (prompt-for-project)))
    (and
     (forget-project choice)
     (lem/common/history:save-file (history))
     (message "Project removed: ~A" choice))))

(defun prompt-for-project ()
  "Prompt for a project saved in the projects history."
  (let ((candidates (saved-projects)))
    (if candidates
        (prompt-for-string
         "Project: "
         :completion-function (lambda (x) (completion-strings x candidates))
         :test-function (lambda (name) (member name candidates :test #'string=)))
        (editor-error "No projects."))))

(define-command project-switch () ()
  "Prompt for a saved project and find a file in this project."
  (let ((project-root (prompt-for-project)))
    (when project-root
      (uiop:with-current-directory (project-root)
        (let ((filename (prompt-for-files-recursively)))
          (alexandria:when-let (buffer (execute-find-file *find-file-executor*
                                                          (lem-core/commands/file::get-file-mode filename)
                                                          filename))
            (when buffer
              (switch-to-buffer buffer t nil))))))))
