;;; project.el --- Operations on the current project  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2022 Free Software Foundation, Inc.
;; Version: 0.9.3
;; Package-Requires: ((emacs "26.1") (xref "1.4.0"))

;; This is a GNU ELPA :core package.  Avoid using functionality that
;; not compatible with the version of Emacs recorded above.

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

;; NOTE: The project API is still experimental and can change in major,
;; backward-incompatible ways.  Everyone is encouraged to try it, and
;; report to us any problems or use cases we hadn't anticipated, by
;; sending an email to emacs-devel, or `M-x report-emacs-bug'.
;;
;; This file contains generic infrastructure for dealing with
;; projects, some utility functions, and commands using that
;; infrastructure.
;;
;; The goal is to make it easier for Lisp programs to operate on the
;; current project, without having to know which package handles
;; detection of that project type, parsing its config files, etc.
;;
;; This file consists of following parts:
;;
;; Infrastructure (the public API):
;;
;; Function `project-current' that returns the current project
;; instance based on the value of the hook `project-find-functions',
;; and several generic functions that act on it.
;;
;; `project-root' must be defined for every project.
;; `project-files' can be overridden for performance purposes.
;; `project-ignores' and `project-external-roots' describe the project
;; files and its relations to external directories.  `project-files'
;; should be consistent with `project-ignores'.
;;
;; `project-buffers' can be overridden if the project has some unusual
;; shape (e.g. it contains files residing outside of its root, or some
;; files inside the root must not be considered a part of it).  It
;; should be consistent with `project-files'.
;;
;; This list can change in future versions.
;;
;; Transient project:
;;
;; An instance of this type can be returned by `project-current' if no
;; project was detected automatically, and the user had to pick a
;; directory manually.  The fileset it describes is the whole
;; directory, with the exception of some standard ignored files and
;; directories.  This type has little purpose otherwise, as the only
;; generic function it provides an override for is `project-root'.
;;
;; VC-aware project:
;;
;; Originally conceived as an example implementation, now it's a
;; relatively fast backend that delegates to 'git ls-files' or 'hg
;; status' to list the project's files.  It honors the VC ignore
;; files, but supports additions to the list using the user option
;; `project-vc-ignores' (usually through .dir-locals.el).  See the
;; customization group `project-vc' for other options that control its
;; behavior.
;;
;; If the repository is using any other VCS than Git or Hg, the file
;; listing uses the default mechanism based on `find-program'.
;;
;; This project type can also be used for non-VCS controlled
;; directories, see the variable `project-vc-extra-root-markers'.
;;
;; Utils:
;;
;; `project-combine-directories' and `project-subtract-directories',
;; mainly for use in the abovementioned generics' implementations.
;;
;; `project-known-project-roots' and `project-remember-project' to
;; interact with the "known projects" list.
;;
;; Commands:
;;
;; `project-prefix-map' contains the full list of commands defined in
;; this package.  This map uses the prefix `C-x p' by default.
;; Type `C-x p f' to find file in the current project.
;; Type `C-x p C-h' to see all available commands and bindings.
;;
;; All commands defined in this package are implemented using the
;; public API only.  As a result, they will work with any project
;; backend that follows the protocol.
;;
;; Any third-party code that wants to use this package should likewise
;; target the public API.  Use any of the built-in commands as the
;; example.
;;
;; How to create a new backend:
;;
;; - Consider whether you really should, or whether there are other
;; ways to reach your goals.  If the backend's performance is
;; significantly lower than that of the built-in one, and it's first
;; in the list, it will affect all commands that use it.  Unless you
;; are going to be using it only yourself or in special circumstances,
;; you will probably want it to be fast, and it's unlikely to be a
;; trivial endeavor.  `project-files' is the method to optimize (the
;; default implementation gets slower the more files the directory
;; has, and the longer the list of ignores is).
;;
;; - Choose the format of the value that represents a project for your
;; backend (we call it project instance).  Don't use any of the
;; formats from other backends.  The format can be arbitrary, as long
;; as the datatype is something `cl-defmethod' can dispatch on.  The
;; value should be stable (when compared with `equal') across
;; invocations, meaning calls to that function from buffers belonging
;; to the same project should return equal values.
;;
;; - Write a new function that will determine the current project
;; based on the directory and add it to `project-find-functions'
;; (which see) using `add-hook'.  It is a good idea to depend on the
;; directory only, and not on the current major mode, for example.
;; Because the usual expectation is that all files in the directory
;; belong to the same project (even if some/most of them are ignored).
;;
;; - Define new methods for some or all generic functions for this
;; backend using `cl-defmethod'.  A `project-root' method is
;; mandatory, `project-files' is recommended, the rest are optional.

;;; TODO:

;; * Reliably cache the list of files in the project, probably using
;;   filenotify.el (if supported) to invalidate.  And avoiding caching
;;   if it's not available (manual cache invalidation is not nice).
;;
;; * Build tool related functionality.  Start with a `project-build'
;;   command, which should provide completions on tasks to run, and
;;   maybe allow entering some additional arguments.  This might
;;   be handled better with a separate API, though.  Then we won't
;;   force every project backend to be aware of the build tool(s) the
;;   project is using.
;;
;; * Command to (re)build the tag files in all project roots.  To that
;;   end, we might need to add a way to provide file whitelist
;;   wildcards for each root to limit etags to certain files (in
;;   addition to the blacklist provided by ignores), and/or allow
;;   specifying additional tag regexps.
;;
;; * UI for the user to be able to pick the current project for the
;;   whole Emacs session, independent of the current directory.  Or,
;;   in the more advanced case, open a set of projects, and have some
;;   project-related commands to use them all.  E.g., have a command
;;   to search for a regexp across all open projects.
;;
;; * Support for project-local variables: a UI to edit them, and a
;;   utility function to retrieve a value.  Probably useless without
;;   support in various built-in commands.  In the API, we might get
;;   away with only adding a `project-configuration-directory' method,
;;   defaulting to the project root the current file/buffer is in.
;;   And prompting otherwise.  How to best mix that with backends that
;;   want to set/provide certain variables themselves, is up for
;;   discussion.

;;; Code:

(require 'cl-generic)
(require 'seq)
(eval-when-compile (require 'subr-x))

(defgroup project nil
  "Operations on the current project."
  :version "28.1"
  :group 'tools)

(defvar project-find-functions (list #'project-try-vc)
  "Special hook to find the project containing a given directory.
Each functions on this hook is called in turn with one
argument, the directory in which to look, and should return
either nil to mean that it is not applicable, or a project instance.
The exact form of the project instance is up to each respective
function; the only practical limitation is to use values that
`cl-defmethod' can dispatch on, like a cons cell, or a list, or a
CL struct.")

(define-obsolete-variable-alias
  'project-current-inhibit-prompt
  'project-current-directory-override
  "29.1")

(defvar project-current-directory-override nil
  "Value to use instead of `default-directory' when detecting the project.
When it is non-nil, `project-current' will always skip prompting too.")

;;;###autoload
(defun project-current (&optional maybe-prompt directory)
  "Return the project instance in DIRECTORY, defaulting to `default-directory'.

When no project is found in that directory, the result depends on
the value of MAYBE-PROMPT: if it is nil or omitted, return nil,
else ask the user for a directory in which to look for the
project, and if no project is found there, return a \"transient\"
project instance.

The \"transient\" project instance is a special kind of value
which denotes a project rooted in that directory and includes all
the files under the directory except for those that match entries
in `vc-directory-exclusion-list' or `grep-find-ignored-files'.

See the doc string of `project-find-functions' for the general form
of the project instance object."
  (unless directory (setq directory (or project-current-directory-override
                                        default-directory)))
  (let ((pr (project--find-in-directory directory)))
    (cond
     (pr)
     ((unless project-current-directory-override
        maybe-prompt)
      (setq directory (project-prompt-project-dir)
            pr (project--find-in-directory directory))))
    (when maybe-prompt
      (if pr
          (project-remember-project pr)
        (project--remove-from-project-list
         directory "Project `%s' not found; removed from list")
        (setq pr (cons 'transient directory))))
    pr))

(defun project--find-in-directory (dir)
  (run-hook-with-args-until-success 'project-find-functions dir))

(defvar project--within-roots-fallback nil)

(cl-defgeneric project-root (project)
  "Return root directory of the current project.

It usually contains the main build file, dependencies
configuration file, etc. Though neither is mandatory.

The directory name must be absolute.")

(cl-defmethod project-root (project
                            &context (project--within-roots-fallback
                                      (eql nil)))
  (car (project-roots project)))

(cl-defgeneric project-roots (project)
  "Return the list containing the current project root.

The function is obsolete, all projects have one main root anyway,
and the rest should be possible to express through
`project-external-roots'."
  ;; FIXME: Can we specify project's version here?
  ;; FIXME: Could we make this affect cl-defmethod calls too?
  (declare (obsolete project-root "0.3.0"))
  (let ((project--within-roots-fallback t))
    (list (project-root project))))

;; FIXME: Add MODE argument, like in `ede-source-paths'?
(cl-defgeneric project-external-roots (_project)
  "Return the list of external roots for PROJECT.

It's the list of directories outside of the project that are
still related to it.  If the project deals with source code then,
depending on the languages used, this list should include the
headers search path, load path, class path, and so on."
  nil)

(cl-defgeneric project-name (project)
  "A human-readable name for the project.
Nominally unique, but not enforced."
  (file-name-nondirectory (directory-file-name (project-root project))))

(cl-defgeneric project-ignores (_project _dir)
  "Return the list of glob patterns to ignore inside DIR.
Patterns can match both regular files and directories.
To root an entry, start it with `./'.  To match directories only,
end it with `/'.  DIR must be either `project-root' or one of
`project-external-roots'."
  ;; TODO: Document and support regexp ignores as used by Hg.
  ;; TODO: Support whitelist entries.
  (require 'grep)
  (defvar grep-find-ignored-files)
  (nconc
   (mapcar
    (lambda (dir)
      (concat dir "/"))
    vc-directory-exclusion-list)
   grep-find-ignored-files))

(defun project--file-completion-table (all-files)
  (lambda (string pred action)
    (cond
     ((eq action 'metadata)
      '(metadata . ((category . project-file))))
     (t
      (complete-with-action action all-files string pred)))))

(cl-defmethod project-root ((project (head transient)))
  (cdr project))

(cl-defgeneric project-files (project &optional dirs)
  "Return a list of files in directories DIRS in PROJECT.
DIRS is a list of absolute directories; it should be some
subset of the project root and external roots.

The default implementation uses `find-program'.  PROJECT is used
to find the list of ignores for each directory."
  (mapcan
   (lambda (dir)
     (project--files-in-directory dir
                                  (project--dir-ignores project dir)))
   (or dirs
       (list (project-root project)))))

(defun project--files-in-directory (dir ignores &optional files)
  (require 'find-dired)
  (require 'xref)
  (let* ((default-directory dir)
         ;; Make sure ~/ etc. in local directory name is
         ;; expanded and not left for the shell command
         ;; to interpret.
         (localdir (file-name-unquote (file-local-name (expand-file-name dir))))
         (dfn (directory-file-name localdir))
         (command (format "%s -H . %s -type f %s -print0"
                          find-program
                          (xref--find-ignores-arguments ignores "./")
                          (if files
                              (concat (shell-quote-argument "(")
                                      " -name "
                                      (mapconcat
                                       #'shell-quote-argument
                                       (split-string files)
                                       (concat " -o -name "))
                                      " "
                                      (shell-quote-argument ")"))
                            "")))
         res)
    (with-temp-buffer
      (let ((status
             (process-file-shell-command command nil t))
            (pt (point-min)))
        (unless (zerop status)
          (goto-char (point-min))
          (if (and
               (not (eql status 127))
               (search-forward "Permission denied\n" nil t))
              (let ((end (1- (point))))
                (re-search-backward "\\`\\|\0")
                (error "File listing failed: %s"
                       (buffer-substring (1+ (point)) end)))
            (error "File listing failed: %s" (buffer-string))))
        (goto-char pt)
        (while (search-forward "\0" nil t)
          (push (buffer-substring-no-properties (1+ pt) (1- (point)))
                res)
          (setq pt (point)))))
    (project--remote-file-names
     (mapcar (lambda (s) (concat dfn s))
             (sort res #'string<)))))

(defun project--remote-file-names (local-files)
  "Return LOCAL-FILES as if they were on the system of `default-directory'.
Also quote LOCAL-FILES if `default-directory' is quoted."
  (let ((remote-id (file-remote-p default-directory)))
    (if (not remote-id)
        (if (file-name-quoted-p default-directory)
            (mapcar #'file-name-quote local-files)
          local-files)
      (mapcar (lambda (file)
                (concat remote-id file))
              local-files))))

(cl-defgeneric project-buffers (project)
  "Return the list of all live buffers that belong to PROJECT.

The default implementation matches each buffer to PROJECT root using
the buffer's value of `default-directory'."
  (let ((root (expand-file-name (file-name-as-directory (project-root project))))
        bufs)
    (dolist (buf (buffer-list))
      (when (string-prefix-p root (expand-file-name
                                   (buffer-local-value 'default-directory buf)))
        (push buf bufs)))
    (nreverse bufs)))

(defgroup project-vc nil
  "VC-aware project implementation."
  :version "25.1"
  :group 'project)

(defcustom project-vc-ignores nil
  "List of patterns to add to `project-ignores'."
  :type '(repeat string)
  :safe #'listp)

(defcustom project-vc-merge-submodules t
  "Non-nil to consider submodules part of the parent project.

After changing this variable (using Customize or .dir-locals.el)
you might have to restart Emacs to see the effect."
  :type 'boolean
  :version "28.1"
  :package-version '(project . "0.2.0")
  :safe #'booleanp)

(defcustom project-vc-include-untracked t
  "When non-nil, the VC-aware project backend includes untracked files."
  :type 'boolean
  :version "29.1"
  :safe #'booleanp)

(defcustom project-vc-name nil
  "When non-nil, the name of the current VC-aware project.

The best way to change the value a VC-aware project reports as
its name, is by setting this in .dir-locals.el."
  :type '(choice (const :tag "Default to the base name" nil)
                 (string :tag "Custom name"))
  :version "29.1"
  :package-version '(project . "0.9.0")
  :safe #'stringp)

;; Not using regexps because these wouldn't work in Git pathspecs, in
;; case we decide we need to be able to list nested projects.
(defcustom project-vc-extra-root-markers nil
  "List of additional markers to signal project roots.

A marker is either a base file name or a glob pattern for such.

A directory containing such a marker file or a file matching a
marker pattern will be recognized as the root of a VC-aware
project.

Example values: \".dir-locals.el\", \"package.json\", \"pom.xml\",
\"requirements.txt\", \"Gemfile\", \"*.gemspec\", \"autogen.sh\".

These will be used in addition to regular directory markers such
as \".git\", \".hg\", and so on, depending on the value of
`vc-handled-backends'.  It is most useful when a project has
subdirectories inside it that need to be considered as separate
projects.  It can also be used for projects outside of VC
repositories.

In either case, their behavior will still obey the relevant
variables, such as `project-vc-ignores' or `project-vc-name'."
  :type '(repeat string)
  :version "29.1"
  :package-version '(project . "0.9.0")
  :safe (lambda (val) (and (listp val) (cl-every #'stringp val))))

;; FIXME: Using the current approach, major modes are supposed to set
;; this variable to a buffer-local value.  So we don't have access to
;; the "external roots" of language A from buffers of language B, which
;; seems desirable in multi-language projects, at least for some
;; potential uses, like "jump to a file in project or external dirs".
;;
;; We could add a second argument to this function: a file extension,
;; or a language name.  Some projects will know the set of languages
;; used in them; for others, like the VC-aware type, we'll need
;; auto-detection.  I see two options:
;;
;; - That could be implemented as a separate second hook, with a
;;   list of functions that return file extensions.
;;
;; - This variable will be turned into a hook with "append" semantics,
;;   and each function in it will perform auto-detection when passed
;;   nil instead of an actual file extension.  Then this hook will, in
;;   general, be modified globally, and not from major mode functions.
;;
;; The second option seems simpler, but the first one has the
;; advantage that the user could override the list of languages used
;; in a project via a directory-local variable, thus skipping
;; languages they're not working on personally (in a big project), or
;; working around problems in language detection (the detection logic
;; might be imperfect for the project in question, or it might work
;; too slowly for the user's taste).
(defvar project-vc-external-roots-function (lambda () tags-table-list)
  "Function that returns a list of external roots.

It should return a list of directory roots that contain source
files related to the current buffer.

The directory names should be absolute.  Used in the VC-aware
project backend implementation of `project-external-roots'.")

(defun project-try-vc (dir)
  (defvar vc-svn-admin-directory)
  (require 'vc-svn)
  ;; FIXME: Learn to invalidate when the value of
  ;; `project-vc-merge-submodules' or `project-vc-extra-root-markers'
  ;; changes.
  (or (vc-file-getprop dir 'project-vc)
      (let* ((backend-markers-alist `((Git . ".git")
                                      (Hg . ".hg")
                                      (Bzr . ".bzr")
                                      (SVN . ,vc-svn-admin-directory)
                                      (DARCS . "_darcs")
                                      (Fossil . ".fslckout")))
             (backend-markers
              (delete
               nil
               (mapcar
                (lambda (b) (assoc-default b backend-markers-alist))
                vc-handled-backends)))
             (marker-re
              (mapconcat
               (lambda (m) (format "\\(%s\\)" (wildcard-to-regexp m)))
               (append backend-markers
                       (project--value-in-dir 'project-vc-extra-root-markers dir))
               "\\|"))
             (locate-dominating-stop-dir-regexp
              (or vc-ignore-dir-regexp locate-dominating-stop-dir-regexp))
             last-matches
             (root
              (locate-dominating-file
               dir
               (lambda (d)
                 ;; Maybe limit count to 100 when we can drop Emacs < 28.
                 (setq last-matches (directory-files d nil marker-re t)))))
             (backend
              (cl-find-if
               (lambda (b)
                 (member (assoc-default b backend-markers-alist)
                         last-matches))
               vc-handled-backends))
             project)
        (when (and
               (eq backend 'Git)
               (project--vc-merge-submodules-p root)
               (project--submodule-p root))
          (let* ((parent (file-name-directory (directory-file-name root))))
            (setq root (vc-call-backend 'Git 'root parent))))
        (when root
          (setq project (list 'vc backend root))
          ;; FIXME: Cache for a shorter time.
          (vc-file-setprop dir 'project-vc project)
          project))))

(defun project--submodule-p (root)
  ;; XXX: We only support Git submodules for now.
  ;;
  ;; For submodules, at least, we expect the users to prefer them to
  ;; be considered part of the parent project.  For those who don't,
  ;; there is the custom var now.
  ;;
  ;; Some users may also set up things equivalent to Git submodules
  ;; using "git worktree" (for example).  However, we expect that most
  ;; of them would prefer to treat those as separate projects anyway.
  (let* ((gitfile (expand-file-name ".git" root)))
    (cond
     ((file-directory-p gitfile)
      nil)
     ((with-temp-buffer
        (insert-file-contents gitfile)
        (goto-char (point-min))
        ;; Kind of a hack to distinguish a submodule from
        ;; other cases of .git files pointing elsewhere.
        (looking-at "gitdir: [./]+/\\.git/modules/"))
      t)
     (t nil))))

(cl-defmethod project-root ((project (head vc)))
  (nth 2 project))

(cl-defmethod project-external-roots ((project (head vc)))
  (project-subtract-directories
   (project-combine-directories
    (mapcar
     #'file-name-as-directory
     (funcall project-vc-external-roots-function)))
   (list (project-root project))))

(cl-defmethod project-files ((project (head vc)) &optional dirs)
  (mapcan
   (lambda (dir)
     (let ((ignores (project--value-in-dir 'project-vc-ignores (nth 2 project)))
           (backend (cadr project)))
       (when backend
         (require (intern (concat "vc-" (downcase (symbol-name backend))))))
       (if (and (file-equal-p dir (nth 2 project))
                (cond
                 ((eq backend 'Hg))
                 ((and (eq backend 'Git)
                       (or
                        (not ignores)
                        (version<= "1.9" (vc-git--program-version)))))))
           (project--vc-list-files dir backend ignores)
         (project--files-in-directory
          dir
          (project--dir-ignores project dir)))))
   (or dirs
       (list (project-root project)))))

(declare-function vc-git--program-version "vc-git")
(declare-function vc-git--run-command-string "vc-git")
(declare-function vc-hg-command "vc-hg")

(defun project--vc-list-files (dir backend extra-ignores)
  (defvar vc-git-use-literal-pathspecs)
  (pcase backend
    (`Git
     (let* ((default-directory (expand-file-name (file-name-as-directory dir)))
            (args '("-z"))
            (vc-git-use-literal-pathspecs nil)
            (include-untracked (project--value-in-dir
                                'project-vc-include-untracked
                                dir))
            files)
       (setq args (append args
                          '("-c" "--exclude-standard")
                          (and include-untracked '("-o"))))
       (when extra-ignores
         (setq args (append args
                            (cons "--"
                                  (mapcar
                                   (lambda (i)
                                     (format
                                      ":(exclude,glob,top)%s"
                                      (if (string-match "\\*\\*" i)
                                          ;; Looks like pathspec glob
                                          ;; format already.
                                          i
                                        (if (string-match "\\./" i)
                                            ;; ./abc -> abc
                                            (setq i (substring i 2))
                                          ;; abc -> **/abc
                                          (setq i (concat "**/" i))
                                          ;; FIXME: '**/abc' should also
                                          ;; match a directory with that
                                          ;; name, but doesn't (git 2.25.1).
                                          ;; Maybe we should replace
                                          ;; such entries with two.
                                          (if (string-match "/\\'" i)
                                              ;; abc/ -> abc/**
                                              (setq i (concat i "**"))))
                                        i)))
                                   extra-ignores)))))
       (setq files
             (mapcar
              (lambda (file) (concat default-directory file))
              (split-string
               (apply #'vc-git--run-command-string nil "ls-files" args)
               "\0" t)))
       (when (project--vc-merge-submodules-p default-directory)
         ;; Unfortunately, 'ls-files --recurse-submodules' conflicts with '-o'.
         (let* ((submodules (project--git-submodules))
                (sub-files
                 (mapcar
                  (lambda (module)
                    (when (file-directory-p module)
                      (project--vc-list-files
                       (concat default-directory module)
                       backend
                       extra-ignores)))
                  submodules)))
           (setq files
                 (apply #'nconc files sub-files))))
       ;; 'git ls-files' returns duplicate entries for merge conflicts.
       ;; XXX: Better solutions welcome, but this seems cheap enough.
       (delete-consecutive-dups files)))
    (`Hg
     (let* ((default-directory (expand-file-name (file-name-as-directory dir)))
            (include-untracked (project--value-in-dir
                                'project-vc-include-untracked
                                dir))
            (args (list (concat "-mcard" (and include-untracked "u"))
                        "--no-status"
                        "-0")))
       (when extra-ignores
         (setq args (nconc args
                           (mapcan
                            (lambda (i)
                              (list "--exclude" i))
                            extra-ignores))))
       (with-temp-buffer
         (apply #'vc-hg-command t 0 "." "status" args)
         (mapcar
          (lambda (s) (concat default-directory s))
          (split-string (buffer-string) "\0" t)))))))

(defun project--vc-merge-submodules-p (dir)
  (project--value-in-dir
   'project-vc-merge-submodules
   dir))

(defun project--git-submodules ()
  ;; 'git submodule foreach' is much slower.
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents ".gitmodules")
        (let (res)
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*path *= *\\(.+\\)" nil t)
            (push (match-string 1) res))
          (nreverse res)))
    (file-missing nil)))

(cl-defmethod project-ignores ((project (head vc)) dir)
  (let* ((root (nth 2 project))
         backend)
    (append
     (when (and backend
                (file-equal-p dir root))
       (setq backend (cadr project))
       (delq
        nil
        (mapcar
         (lambda (entry)
           (cond
            ((eq ?! (aref entry 0))
             ;; No support for whitelisting (yet).
             nil)
            ((string-match "\\(/\\)[^/]" entry)
             ;; FIXME: This seems to be Git-specific.
             ;; And / in the entry (start or even the middle) means
             ;; the pattern is "rooted".  Or actually it is then
             ;; relative to its respective .gitignore (of which there
             ;; could be several), but we only support .gitignore at
             ;; the root.
             (if (= (match-beginning 0) 0)
                 (replace-match "./" t t entry 1)
               (concat "./" entry)))
            (t entry)))
         (condition-case nil
             (vc-call-backend backend 'ignore-completion-table root)
           (vc-not-supported () nil)))))
     (project--value-in-dir 'project-vc-ignores root)
     (mapcar
      (lambda (dir)
        (concat dir "/"))
      vc-directory-exclusion-list))))

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
    (let ((enable-local-variables :all))
      (hack-dir-local-variables-non-file-buffer))
    (symbol-value var)))

(cl-defmethod project-buffers ((project (head vc)))
  (let* ((root (expand-file-name (file-name-as-directory (project-root project))))
         (modules (unless (or (project--vc-merge-submodules-p root)
                              (project--submodule-p root))
                    (mapcar
                     (lambda (m) (format "%s%s/" root m))
                     (project--git-submodules))))
         dd
         bufs)
    (dolist (buf (buffer-list))
      (setq dd (expand-file-name (buffer-local-value 'default-directory buf)))
      (when (and (string-prefix-p root dd)
                 (not (cl-find-if (lambda (module) (string-prefix-p module dd))
                                  modules)))
        (push buf bufs)))
    (nreverse bufs)))

(cl-defmethod project-name ((_project (head vc)))
  (or project-vc-name
      (cl-call-next-method)))


;;; Project commands

;;;###autoload
(defvar project-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "!" 'project-shell-command)
    (define-key map "&" 'project-async-shell-command)
    (define-key map "f" 'project-find-file)
    (define-key map "F" 'project-or-external-find-file)
    (define-key map "b" 'project-switch-to-buffer)
    (define-key map "s" 'project-shell)
    (define-key map "d" 'project-find-dir)
    (define-key map "D" 'project-dired)
    (define-key map "v" 'project-vc-dir)
    (define-key map "c" 'project-compile)
    (define-key map "e" 'project-eshell)
    (define-key map "k" 'project-kill-buffers)
    (define-key map "p" 'project-switch-project)
    (define-key map "g" 'project-find-regexp)
    (define-key map "G" 'project-or-external-find-regexp)
    (define-key map "r" 'project-query-replace-regexp)
    (define-key map "x" 'project-execute-extended-command)
    (define-key map "\C-b" 'project-list-buffers)
    map)
  "Keymap for project commands.")

;;;###autoload (define-key ctl-x-map "p" project-prefix-map)

;; We can't have these place-specific maps inherit from
;; project-prefix-map because project--other-place-command needs to
;; know which map the key binding came from, as if it came from one of
;; these maps, we don't want to set display-buffer-overriding-action

(defvar project-other-window-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-o" #'project-display-buffer)
    map)
  "Keymap for project commands that display buffers in other windows.")

(defvar project-other-frame-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-o" #'project-display-buffer-other-frame)
    map)
  "Keymap for project commands that display buffers in other frames.")

(defun project--other-place-command (action &optional map)
  (let* ((key (read-key-sequence-vector nil t))
         (place-cmd (lookup-key map key))
         (generic-cmd (lookup-key project-prefix-map key))
         (switch-to-buffer-obey-display-actions t)
         (display-buffer-overriding-action (unless place-cmd action)))
    (if-let ((cmd (or place-cmd generic-cmd)))
        (call-interactively cmd)
      (user-error "%s is undefined" (key-description key)))))

;;;###autoload
(defun project-other-window-command ()
  "Run project command, displaying resultant buffer in another window.

The following commands are available:

\\{project-prefix-map}
\\{project-other-window-map}"
  (interactive)
  (project--other-place-command '((display-buffer-pop-up-window)
                                  (inhibit-same-window . t))
                                project-other-window-map))

;;;###autoload (define-key ctl-x-4-map "p" #'project-other-window-command)

;;;###autoload
(defun project-other-frame-command ()
  "Run project command, displaying resultant buffer in another frame.

The following commands are available:

\\{project-prefix-map}
\\{project-other-frame-map}"
  (interactive)
  (project--other-place-command '((display-buffer-pop-up-frame))
                                project-other-frame-map))

;;;###autoload (define-key ctl-x-5-map "p" #'project-other-frame-command)

;;;###autoload
(defun project-other-tab-command ()
  "Run project command, displaying resultant buffer in a new tab.

The following commands are available:

\\{project-prefix-map}"
  (interactive)
  (project--other-place-command '((display-buffer-in-new-tab))))

;;;###autoload
(when (bound-and-true-p tab-prefix-map)
  (define-key tab-prefix-map "p" #'project-other-tab-command))

(declare-function grep-read-files "grep")
(declare-function xref--find-ignores-arguments "xref")

;;;###autoload
(defun project-find-regexp (regexp)
  "Find all matches for REGEXP in the current project's roots.
With \\[universal-argument] prefix, you can specify the directory
to search in, and the file name pattern to search for.  The
pattern may use abbreviations defined in `grep-files-aliases',
e.g. entering `ch' is equivalent to `*.[ch]'.  As whitespace
triggers completion when entering a pattern, including it
requires quoting, e.g. `\\[quoted-insert]<space>'."
  (interactive (list (project--read-regexp)))
  (require 'xref)
  (require 'grep)
  (let* ((caller-dir default-directory)
         (pr (project-current t))
         (default-directory (project-root pr))
         (files
          (if (not current-prefix-arg)
              (project-files pr)
            (let ((dir (read-directory-name "Base directory: "
                                            caller-dir nil t)))
              (project--files-in-directory dir
                                           nil
                                           (grep-read-files regexp))))))
    (xref-show-xrefs
     (apply-partially #'project--find-regexp-in-files regexp files)
     nil)))

(defun project--dir-ignores (project dir)
  (let ((root (project-root project)))
    (if (not (file-in-directory-p dir root))
        (project-ignores nil nil)       ;The defaults.
      (let ((ignores (project-ignores project root)))
        (if (file-equal-p root dir)
            ignores
          ;; FIXME: Update the "rooted" ignores to relate to DIR instead.
          (cl-delete-if (lambda (str) (string-prefix-p "./" str))
                        ignores))))))

;;;###autoload
(defun project-or-external-find-regexp (regexp)
  "Find all matches for REGEXP in the project roots or external roots.
With \\[universal-argument] prefix, you can specify the file name
pattern to search for."
  (interactive (list (project--read-regexp)))
  (require 'xref)
  (let* ((pr (project-current t))
         (default-directory (project-root pr))
         (files
          (project-files pr (cons
                             (project-root pr)
                             (project-external-roots pr)))))
    (xref-show-xrefs
     (apply-partially #'project--find-regexp-in-files regexp files)
     nil)))

(defun project--find-regexp-in-files (regexp files)
  (unless files
    (user-error "Empty file list"))
  (let ((xrefs (xref-matches-in-files regexp files)))
    (unless xrefs
      (user-error "No matches for: %s" regexp))
    xrefs))

(defvar project-regexp-history-variable 'grep-regexp-history)

(defun project--read-regexp ()
  (let ((sym (thing-at-point 'symbol t)))
    (read-regexp "Find regexp" (and sym (regexp-quote sym))
                 project-regexp-history-variable)))

;;;###autoload
(defun project-find-file (&optional include-all)
  "Visit a file (with completion) in the current project.

The filename at point (determined by `thing-at-point'), if any,
is available as part of \"future history\".

If INCLUDE-ALL is non-nil, or with prefix argument when called
interactively, include all files under the project root, except
for VCS directories listed in `vc-directory-exclusion-list'."
  (interactive "P")
  (let* ((pr (project-current t))
         (root (project-root pr))
         (dirs (list root)))
    (project-find-file-in
     (or (thing-at-point 'filename)
         (and buffer-file-name (file-relative-name buffer-file-name root)))
     dirs pr include-all)))

;;;###autoload
(defun project-or-external-find-file (&optional include-all)
  "Visit a file (with completion) in the current project or external roots.

The filename at point (determined by `thing-at-point'), if any,
is available as part of \"future history\".

If INCLUDE-ALL is non-nil, or with prefix argument when called
interactively, include all files under the project root, except
for VCS directories listed in `vc-directory-exclusion-list'."
  (interactive "P")
  (let* ((pr (project-current t))
         (dirs (cons
                (project-root pr)
                (project-external-roots pr))))
    (project-find-file-in (thing-at-point 'filename) dirs pr include-all)))

(defcustom project-read-file-name-function #'project--read-file-cpd-relative
  "Function to call to read a file name from a list.
For the arguments list, see `project--read-file-cpd-relative'."
  :type '(choice (const :tag "Read with completion from relative names"
                        project--read-file-cpd-relative)
                 (const :tag "Read with completion from absolute names"
                        project--read-file-absolute)
                 (function :tag "Custom function" nil))
  :group 'project
  :version "27.1")

(defun project--read-file-cpd-relative (prompt
                                        all-files &optional predicate
                                        hist mb-default)
  "Read a file name, prompting with PROMPT.
ALL-FILES is a list of possible file name completions.

PREDICATE and HIST have the same meaning as in `completing-read'.

MB-DEFAULT is used as part of \"future history\", to be inserted
by the user at will."
  (let* ((common-parent-directory
          (let ((common-prefix (try-completion "" all-files)))
            (if (> (length common-prefix) 0)
                (file-name-directory common-prefix))))
         (cpd-length (length common-parent-directory))
         (prompt (if (zerop cpd-length)
                     prompt
                   (concat prompt (format " in %s" common-parent-directory))))
         (included-cpd (when (member common-parent-directory all-files)
                         (setq all-files
                               (delete common-parent-directory all-files))
                         t))
         (substrings (mapcar (lambda (s) (substring s cpd-length)) all-files))
         (_ (when included-cpd
              (setq substrings (cons "./" substrings))))
         (new-collection (project--file-completion-table substrings))
         (relname (let ((history-add-new-input nil))
                    (project--completing-read-strict prompt
                                                     new-collection
                                                     predicate
                                                     hist mb-default)))
         (absname (expand-file-name relname common-parent-directory)))
    (when (and hist history-add-new-input)
      (add-to-history hist (abbreviate-file-name absname)))
    absname))

(defun project--read-file-absolute (prompt
                                    all-files &optional predicate
                                    hist mb-default)
  (project--completing-read-strict prompt
                                   (project--file-completion-table all-files)
                                   predicate
                                   hist mb-default))

(defun project-find-file-in (suggested-filename dirs project &optional include-all)
  "Complete a file name in DIRS in PROJECT and visit the result.

SUGGESTED-FILENAME is a relative file name, or part of it, which
is used as part of \"future history\".

If INCLUDE-ALL is non-nil, or with prefix argument when called
interactively, include all files from DIRS, except for VCS
directories listed in `vc-directory-exclusion-list'."
  (let* ((vc-dirs-ignores (mapcar
                           (lambda (dir)
                             (concat dir "/"))
                           vc-directory-exclusion-list))
         (all-files
          (if include-all
              (mapcan
               (lambda (dir) (project--files-in-directory dir vc-dirs-ignores))
               dirs)
            (project-files project dirs)))
         (completion-ignore-case read-file-name-completion-ignore-case)
         (file (funcall project-read-file-name-function
                        "Find file" all-files nil 'file-name-history
                        suggested-filename)))
    (if (string= file "")
        (user-error "You didn't specify the file")
      (find-file file))))

(defun project--completing-read-strict (prompt
                                        collection &optional predicate
                                        hist mb-default)
  (minibuffer-with-setup-hook
      (lambda ()
        (setq-local minibuffer-default-add-function
                    (lambda ()
                      (let ((minibuffer-default mb-default))
                        (minibuffer-default-add-completions)))))
    (completing-read (format "%s: " prompt)
                     collection predicate 'confirm
                     nil
                     hist)))

;;;###autoload
(defun project-find-dir ()
  "Start Dired in a directory inside the current project."
  (interactive)
  (let* ((project (project-current t))
         (all-files (project-files project))
         (completion-ignore-case read-file-name-completion-ignore-case)
         ;; FIXME: This misses directories without any files directly
         ;; inside.  Consider DIRS-ONLY as an argument for
         ;; `project-files-filtered', and see
         ;; https://stackoverflow.com/a/50685235/615245 for possible
         ;; implementation.
         (all-dirs (mapcar #'file-name-directory all-files))
         (dir (funcall project-read-file-name-function
                       "Dired"
                       ;; Some completion UIs show duplicates.
                       (delete-dups all-dirs)
                       nil 'file-name-history)))
    (dired dir)))

;;;###autoload
(defun project-dired ()
  "Start Dired in the current project's root."
  (interactive)
  (dired (project-root (project-current t))))

;;;###autoload
(defun project-vc-dir ()
  "Run VC-Dir in the current project's root."
  (interactive)
  (vc-dir (project-root (project-current t))))

(declare-function comint-check-proc "comint")

;;;###autoload
(defun project-shell ()
  "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists."
  (interactive)
  (require 'comint)
  (let* ((default-directory (project-root (project-current t)))
         (default-project-shell-name (project-prefixed-buffer-name "shell"))
         (shell-buffer (get-buffer default-project-shell-name)))
    (if (and shell-buffer (not current-prefix-arg))
        (if (comint-check-proc shell-buffer)
            (pop-to-buffer shell-buffer (bound-and-true-p display-comint-buffer-action))
          (shell shell-buffer))
      (shell (generate-new-buffer-name default-project-shell-name)))))

;;;###autoload
(defun project-eshell ()
  "Start Eshell in the current project's root directory.
If a buffer already exists for running Eshell in the project's root,
switch to it.  Otherwise, create a new Eshell buffer.
With \\[universal-argument] prefix arg, create a new Eshell buffer even
if one already exists."
  (interactive)
  (defvar eshell-buffer-name)
  (let* ((default-directory (project-root (project-current t)))
         (eshell-buffer-name (project-prefixed-buffer-name "eshell"))
         (eshell-buffer (get-buffer eshell-buffer-name)))
    (if (and eshell-buffer (not current-prefix-arg))
        (pop-to-buffer eshell-buffer (bound-and-true-p display-comint-buffer-action))
      (eshell t))))

;;;###autoload
(defun project-async-shell-command ()
  "Run `async-shell-command' in the current project's root directory."
  (declare (interactive-only async-shell-command))
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'async-shell-command)))

;;;###autoload
(defun project-shell-command ()
  "Run `shell-command' in the current project's root directory."
  (declare (interactive-only shell-command))
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'shell-command)))

(declare-function fileloop-continue "fileloop" ())

;;;###autoload
(defun project-search (regexp)
  "Search for REGEXP in all the files of the project.
Stops when a match is found.
To continue searching for the next match, use the
command \\[fileloop-continue]."
  (interactive "sSearch (regexp): ")
  (fileloop-initialize-search
   regexp (project-files (project-current t)) 'default)
  (fileloop-continue))

;;;###autoload
(defun project-query-replace-regexp (from to)
  "Query-replace REGEXP in all the files of the project.
Stops when a match is found and prompts for whether to replace it.
At that prompt, the user must type a character saying what to do
with the match.  Type SPC or `y' to replace the match,
DEL or `n' to skip and go to the next match.  For more directions,
type \\[help-command] at that time.
If you exit the `query-replace', you can later continue the
`query-replace' loop using the command \\[fileloop-continue]."
  (interactive
   (let ((query-replace-read-from-regexp-default 'find-tag-default-as-regexp))
     (pcase-let ((`(,from ,to)
                  (query-replace-read-args "Query replace (regexp)" t t)))
       (list from to))))
  (fileloop-initialize-replace
   from to
   ;; XXX: Filter out Git submodules, which are not regular files.
   ;; `project-files' can return those, which is arguably suboptimal,
   ;; but removing them eagerly has performance cost.
   (cl-delete-if-not #'file-regular-p (project-files (project-current t)))
   'default)
  (fileloop-continue))

(defvar compilation-read-command)
(declare-function compilation-read-command "compile")

(defun project-prefixed-buffer-name (mode)
  (concat "*"
          (file-name-nondirectory
           (directory-file-name default-directory))
          "-"
          (downcase mode)
          "*"))

(defcustom project-compilation-buffer-name-function nil
  "Function to compute the name of a project compilation buffer.
If non-nil, it overrides `compilation-buffer-name-function' for
`project-compile'."
  :version "28.1"
  :group 'project
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Prefixed with root directory name"
                        project-prefixed-buffer-name)
                 (function :tag "Custom function")))

;;;###autoload
(defun project-compile ()
  "Run `compile' in the project root."
  (declare (interactive-only compile))
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (call-interactively #'compile)))

(defcustom project-ignore-buffer-conditions nil
  "List of conditions to filter the buffers to be switched to.
If any of these conditions are satisfied for a buffer in the
current project, `project-switch-to-buffer',
`project-display-buffer' and `project-display-buffer-other-frame'
ignore it.
See the doc string of `project-kill-buffer-conditions' for the
general form of conditions."
  :type '(repeat (choice regexp function symbol
                         (cons :tag "Major mode"
                               (const major-mode) symbol)
                         (cons :tag "Derived mode"
                               (const derived-mode) symbol)
                         (cons :tag "Negation"
                               (const not) sexp)
                         (cons :tag "Conjunction"
                               (const and) sexp)
                         (cons :tag "Disjunction"
                               (const or) sexp)))
  :version "29.1"
  :group 'project
  :package-version '(project . "0.8.2"))

(defun project--read-project-buffer ()
  (let* ((pr (project-current t))
         (current-buffer (current-buffer))
         (other-buffer (other-buffer current-buffer))
         (other-name (buffer-name other-buffer))
         (buffers (project-buffers pr))
         (predicate
          (lambda (buffer)
            ;; BUFFER is an entry (BUF-NAME . BUF-OBJ) of Vbuffer_alist.
            (and (memq (cdr buffer) buffers)
                 (not
                  (project--buffer-check
                   (cdr buffer) project-ignore-buffer-conditions))))))
    (read-buffer
     "Switch to buffer: "
     (when (funcall predicate (cons other-name other-buffer))
       other-name)
     nil
     predicate)))

;;;###autoload
(defun project-switch-to-buffer (buffer-or-name)
  "Display buffer BUFFER-OR-NAME in the selected window.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical."
  (interactive (list (project--read-project-buffer)))
  (switch-to-buffer buffer-or-name))

;;;###autoload
(defun project-display-buffer (buffer-or-name)
  "Display BUFFER-OR-NAME in some window, without selecting it.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical.

This function uses `display-buffer' as a subroutine, which see
for how it is determined where the buffer will be displayed."
  (interactive (list (project--read-project-buffer)))
  (display-buffer buffer-or-name))

;;;###autoload
(defun project-display-buffer-other-frame (buffer-or-name)
  "Display BUFFER-OR-NAME preferably in another frame.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical.

This function uses `display-buffer-other-frame' as a subroutine,
which see for how it is determined where the buffer will be
displayed."
  (interactive (list (project--read-project-buffer)))
  (display-buffer-other-frame buffer-or-name))

;;;###autoload
(defun project-list-buffers (&optional arg)
  "Display a list of project buffers.
The list is displayed in a buffer named \"*Buffer List*\".

By default, all project buffers are listed except those whose names
start with a space (which are for internal use).  With prefix argument
ARG, show only buffers that are visiting files."
  (interactive "P")
  (let ((pr (project-current t)))
    (display-buffer
     (if (version< emacs-version "29.0.50")
         (let ((buf (list-buffers-noselect arg (project-buffers pr))))
           (with-current-buffer buf
             (setq-local revert-buffer-function
                         (lambda (&rest _ignored)
                           (list-buffers--refresh (project-buffers pr))
                           (tabulated-list-print t))))
           buf)
       (list-buffers-noselect
        arg nil (lambda (buf) (memq buf (project-buffers pr))))))))

(defcustom project-kill-buffer-conditions
  '(buffer-file-name    ; All file-visiting buffers are included.
    ;; Most of temp and logging buffers (aside from hidden ones):
    (and
     (major-mode . fundamental-mode)
     "\\`[^ ]")
    ;; non-text buffer such as xref, occur, vc, log, ...
    (and (derived-mode . special-mode)
         (not (major-mode . help-mode))
         (not (derived-mode . gnus-mode)))
    (derived-mode . compilation-mode)
    (derived-mode . dired-mode)
    (derived-mode . diff-mode)
    (derived-mode . comint-mode)
    (derived-mode . eshell-mode)
    (derived-mode . change-log-mode))
  "List of conditions to kill buffers related to a project.
This list is used by `project-kill-buffers'.
Each condition is either:
- a regular expression, to match a buffer name,
- a predicate function that takes a buffer object as argument
  and returns non-nil if the buffer should be killed,
- a cons-cell, where the car describes how to interpret the cdr.
  The car can be one of the following:
  * `major-mode': the buffer is killed if the buffer's major
    mode is eq to the cons-cell's cdr.
  * `derived-mode': the buffer is killed if the buffer's major
    mode is derived from the major mode in the cons-cell's cdr.
  * `not': the cdr is interpreted as a negation of a condition.
  * `and': the cdr is a list of recursive conditions, that all have
    to be met.
  * `or': the cdr is a list of recursive conditions, of which at
    least one has to be met.

If any of these conditions are satisfied for a buffer in the
current project, it will be killed."
  :type '(repeat (choice regexp function symbol
                         (cons :tag "Major mode"
                               (const major-mode) symbol)
                         (cons :tag "Derived mode"
                               (const derived-mode) symbol)
                         (cons :tag "Negation"
                               (const not) sexp)
                         (cons :tag "Conjunction"
                               (const and) sexp)
                         (cons :tag "Disjunction"
                               (const or) sexp)))
  :version "29.1"
  :group 'project
  :package-version '(project . "0.8.2"))

(defcustom project-kill-buffers-display-buffer-list nil
  "Non-nil to display list of buffers to kill before killing project buffers.
Used by `project-kill-buffers'."
  :type 'boolean
  :version "29.1"
  :group 'project
  :package-version '(project . "0.8.2")
  :safe #'booleanp)

(defun project--buffer-check (buf conditions)
  "Check if buffer BUF matches any element of the list CONDITIONS.
See `project-kill-buffer-conditions' or
`project-ignore-buffer-conditions' for more details on the
form of CONDITIONS."
  (catch 'match
    (dolist (c conditions)
      (when (cond
             ((stringp c)
              (string-match-p c (buffer-name buf)))
             ((functionp c)
              (funcall c buf))
             ((eq (car-safe c) 'major-mode)
              (eq (buffer-local-value 'major-mode buf)
                  (cdr c)))
             ((eq (car-safe c) 'derived-mode)
              (provided-mode-derived-p
               (buffer-local-value 'major-mode buf)
               (cdr c)))
             ((eq (car-safe c) 'not)
              (not (project--buffer-check buf (cdr c))))
             ((eq (car-safe c) 'or)
              (project--buffer-check buf (cdr c)))
             ((eq (car-safe c) 'and)
              (seq-every-p
               (apply-partially #'project--buffer-check
                                buf)
               (mapcar #'list (cdr c)))))
        (throw 'match t)))))

(defun project--buffers-to-kill (pr)
  "Return list of buffers in project PR to kill.
What buffers should or should not be killed is described
in `project-kill-buffer-conditions'."
  (let (bufs)
    (dolist (buf (project-buffers pr))
      (when (project--buffer-check buf project-kill-buffer-conditions)
        (push buf bufs)))
    bufs))

;;;###autoload
(defun project-kill-buffers (&optional no-confirm)
  "Kill the buffers belonging to the current project.
Two buffers belong to the same project if their project
instances, as reported by `project-current' in each buffer, are
identical.  Only the buffers that match a condition in
`project-kill-buffer-conditions' will be killed.  If NO-CONFIRM
is non-nil, the command will not ask the user for confirmation.
NO-CONFIRM is always nil when the command is invoked
interactively.

Also see the `project-kill-buffers-display-buffer-list' variable."
  (interactive)
  (let* ((pr (project-current t))
         (bufs (project--buffers-to-kill pr))
         (query-user (lambda ()
                       (yes-or-no-p
                        (format "Kill %d buffers in %s? "
                                (length bufs)
                                (project-root pr))))))
    (cond (no-confirm
           (mapc #'kill-buffer bufs))
          ((null bufs)
           (message "No buffers to kill"))
          (project-kill-buffers-display-buffer-list
           (when
               (with-current-buffer-window
                   (get-buffer-create "*Buffer List*")
                   `(display-buffer--maybe-at-bottom
                     (dedicated . t)
                     (window-height . (fit-window-to-buffer))
                     (preserve-size . (nil . t))
                     (body-function
                      . ,#'(lambda (_window)
                             (list-buffers-noselect nil bufs))))
                   #'(lambda (window _value)
                       (with-selected-window window
                         (unwind-protect
                             (funcall query-user)
                           (when (window-live-p window)
                             (quit-restore-window window 'kill))))))
             (mapc #'kill-buffer bufs)))
          ((funcall query-user)
           (mapc #'kill-buffer bufs)))))


;;; Project list

(defcustom project-list-file (locate-user-emacs-file "projects")
  "File in which to save the list of known projects."
  :type 'file
  :version "28.1"
  :group 'project)

(defvar project--list 'unset
  "List structure containing root directories of known projects.
With some possible metadata (to be decided).")

(defun project--read-project-list ()
  "Initialize `project--list' using contents of `project-list-file'."
  (let ((filename project-list-file))
    (setq project--list
          (when (file-exists-p filename)
            (with-temp-buffer
              (insert-file-contents filename)
              (read (current-buffer)))))
    (unless (seq-every-p
             (lambda (elt) (stringp (car-safe elt)))
             project--list)
      (warn "Contents of %s are in wrong format, resetting"
            project-list-file)
      (setq project--list nil))))

(defun project--ensure-read-project-list ()
  "Initialize `project--list' if it isn't already initialized."
  (when (eq project--list 'unset)
    (project--read-project-list)))

(defun project--write-project-list ()
  "Save `project--list' in `project-list-file'."
  (let ((filename project-list-file))
    (with-temp-buffer
      (insert ";;; -*- lisp-data -*-\n")
      (let ((print-length nil)
            (print-level nil))
        (pp project--list (current-buffer)))
      (write-region nil nil filename nil 'silent))))

;;;###autoload
(defun project-remember-project (pr &optional no-write)
  "Add project PR to the front of the project list.
Save the result in `project-list-file' if the list of projects
has changed, and NO-WRITE is nil."
  (project--ensure-read-project-list)
  (let ((dir (project-root pr)))
    (unless (equal (caar project--list) dir)
      (dolist (ent project--list)
        (when (equal dir (car ent))
          (setq project--list (delq ent project--list))))
      (push (list dir) project--list)
      (unless no-write
        (project--write-project-list)))))

(defun project--remove-from-project-list (project-root report-message)
  "Remove directory PROJECT-ROOT of a missing project from the project list.
If the directory was in the list before the removal, save the
result in `project-list-file'.  Announce the project's removal
from the list using REPORT-MESSAGE, which is a format string
passed to `message' as its first argument."
  (project--ensure-read-project-list)
  (when-let ((ent (assoc project-root project--list)))
    (setq project--list (delq ent project--list))
    (message report-message project-root)
    (project--write-project-list)))

;;;###autoload
(defun project-forget-project (project-root)
  "Remove directory PROJECT-ROOT from the project list.
PROJECT-ROOT is the root directory of a known project listed in
the project list."
  (interactive (list (project-prompt-project-dir)))
  (project--remove-from-project-list
   project-root "Project `%s' removed from known projects"))

(defun project-prompt-project-dir ()
  "Prompt the user for a directory that is one of the known project roots.
The project is chosen among projects known from the project list,
see `project-list-file'.
It's also possible to enter an arbitrary directory not in the list."
  (project--ensure-read-project-list)
  (let* ((dir-choice "... (choose a dir)")
         (choices
          ;; XXX: Just using this for the category (for the substring
          ;; completion style).
          (project--file-completion-table
           (append project--list `(,dir-choice))))
         (pr-dir ""))
    (while (equal pr-dir "")
      ;; If the user simply pressed RET, do this again until they don't.
      (setq pr-dir (completing-read "Select project: " choices nil t)))
    (if (equal pr-dir dir-choice)
        (read-directory-name "Select directory: " default-directory nil t)
      pr-dir)))

;;;###autoload
(defun project-known-project-roots ()
  "Return the list of root directories of all known projects."
  (project--ensure-read-project-list)
  (mapcar #'car project--list))

;;;###autoload
(defun project-execute-extended-command ()
  "Execute an extended command in project root."
  (declare (interactive-only command-execute))
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'execute-extended-command)))

(defun project-remember-projects-under (dir &optional recursive)
  "Index all projects below a directory DIR.
If RECURSIVE is non-nil, recurse into all subdirectories to find
more projects.  After finishing, a message is printed summarizing
the progress.  The function returns the number of detected
projects."
  (interactive "DDirectory: \nP")
  (project--ensure-read-project-list)
  (let ((queue (list dir))
        (count 0)
        (known (make-hash-table
                :size (* 2 (length project--list))
                :test #'equal )))
    (dolist (project (mapcar #'car project--list))
      (puthash project t known))
    (while queue
      (when-let ((subdir (pop queue))
                 ((file-directory-p subdir)))
        (when-let ((project (project--find-in-directory subdir))
                   (project-root (project-root project))
                   ((not (gethash project-root known))))
          (project-remember-project project t)
          (puthash project-root t known)
          (message "Found %s..." project-root)
          (setq count (1+ count)))
        (when (and recursive (file-directory-p subdir))
          (setq queue
                (nconc
                 (directory-files
                  subdir t directory-files-no-dot-files-regexp t)
                 queue)))))
    (unless (eq recursive 'in-progress)
      (if (zerop count)
          (message "No projects were found")
        (project--write-project-list)
        (message "%d project%s were found"
                 count (if (= count 1) "" "s"))))
    count))

(defun project-forget-zombie-projects ()
  "Forget all known projects that don't exist any more."
  (interactive)
  (dolist (proj (project-known-project-roots))
    (unless (file-exists-p proj)
      (project-forget-project proj))))

(defun project-forget-projects-under (dir &optional recursive)
  "Forget all known projects below a directory DIR.
If RECURSIVE is non-nil, recurse into all subdirectories to
remove all known projects.  After finishing, a message is printed
summarizing the progress.  The function returns the number of
forgotten projects."
  (interactive "DDirectory: \nP")
  (let ((count 0))
    (if recursive
        (dolist (proj (project-known-project-roots))
          (when (file-in-directory-p proj dir)
            (project-forget-project proj)
            (setq count (1+ count))))
      (dolist (proj (project-known-project-roots))
        (when (file-equal-p (file-name-directory proj) dir)
          (project-forget-project proj)
          (setq count (1+ count)))))
    (if (zerop count)
        (message "No projects were forgotten")
      (project--write-project-list)
      (message "%d project%s were forgotten"
               count (if (= count 1) "" "s")))
    count))


;;; Project switching

(defcustom project-switch-commands
  '((project-find-file "Find file")
    (project-find-regexp "Find regexp")
    (project-find-dir "Find directory")
    (project-vc-dir "VC-Dir")
    (project-eshell "Eshell"))
  "Alist mapping commands to descriptions.
Used by `project-switch-project' to construct a dispatch menu of
commands available upon \"switching\" to another project.

Each element is of the form (COMMAND LABEL &optional KEY) where
COMMAND is the command to run when KEY is pressed.  LABEL is used
to distinguish the menu entries in the dispatch menu.  If KEY is
absent, COMMAND must be bound in `project-prefix-map', and the
key is looked up in that map.

The value can also be a symbol, the name of the command to be
invoked immediately without any dispatch menu."
  :version "28.1"
  :group 'project
  :package-version '(project . "0.6.0")
  :type '(choice
          (repeat :tag "Commands menu"
           (list
            (symbol :tag "Command")
            (string :tag "Label")
            (choice :tag "Key to press"
                    (const :tag "Infer from the keymap" nil)
                    (character :tag "Explicit key"))))
          (symbol :tag "Single command")))

(defcustom project-switch-use-entire-map nil
  "Make `project-switch-project' use entire `project-prefix-map'.
If nil, `project-switch-project' will only recognize commands
listed in `project-switch-commands' and signal an error when
others are invoked.  Otherwise, all keys in `project-prefix-map'
are legal even if they aren't listed in the dispatch menu."
  :type 'boolean
  :group 'project
  :version "28.1")

(defun project--keymap-prompt ()
  "Return a prompt for the project switching dispatch menu."
  (mapconcat
   (pcase-lambda (`(,cmd ,label ,key))
     (when (characterp cmd) ; Old format, apparently user-customized.
       (let ((tmp cmd))
         ;; TODO: Add a deprecation warning, probably.
         (setq cmd key
               key tmp)))
     (let ((key (if key
                    (vector key)
                  (where-is-internal cmd (list project-prefix-map) t))))
       (format "[%s] %s"
               (propertize (key-description key) 'face 'bold)
               label)))
   project-switch-commands
   "  "))

(defun project--switch-project-command ()
  (let* ((commands-menu
          (mapcar
           (lambda (row)
             (if (characterp (car row))
                 ;; Deprecated format.
                 ;; XXX: Add a warning about it?
                 (reverse row)
               row))
           project-switch-commands))
         (commands-map
          (let ((temp-map (make-sparse-keymap)))
            (set-keymap-parent temp-map project-prefix-map)
            (dolist (row commands-menu temp-map)
              (when-let ((cmd (nth 0 row))
                         (keychar (nth 2 row)))
                (define-key temp-map (vector keychar) cmd)))))
         command)
    (while (not command)
      (let* ((overriding-local-map commands-map)
             (choice (read-key-sequence (project--keymap-prompt))))
        (when (setq command (lookup-key commands-map choice))
          (unless (or project-switch-use-entire-map
                      (assq command commands-menu))
            ;; TODO: Add some hint to the prompt, like "key not
            ;; recognized" or something.
            (setq command nil)))
        (let ((global-command (lookup-key (current-global-map) choice)))
          (when (memq global-command
                      '(keyboard-quit keyboard-escape-quit))
            (call-interactively global-command)))))
    command))

;;;###autoload
(defun project-switch-project (dir)
  "\"Switch\" to another project by running an Emacs command.
The available commands are presented as a dispatch menu
made from `project-switch-commands'.

When called in a program, it will use the project corresponding
to directory DIR."
  (interactive (list (project-prompt-project-dir)))
  (let ((command (if (symbolp project-switch-commands)
                     project-switch-commands
                   (project--switch-project-command))))
    (let ((project-current-directory-override dir))
      (call-interactively command))))

(provide 'project)
;;; project.el ends here
