;;; project.el --- Operations on the current project  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 Free Software Foundation, Inc.
;; Version: 0.5.2
;; Package-Requires: ((emacs "26.3") (xref "1.0.2"))

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
;; This list can change in future versions.
;;
;; VC project:
;;
;; Originally conceived as an example implementation, now it's a
;; relatively fast backend that delegates to 'git ls-files' or 'hg
;; status' to list the project's files.  It honors the VC ignore
;; files, but supports additions to the list using the user option
;; `project-vc-ignores' (usually through .dir-locals.el).
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
;; (which see) using `add-hook'. It is a good idea to depend on the
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

(defvar project-current-inhibit-prompt nil
  "Non-nil to skip prompting the user in `project-current'.")

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
the files under the directory except for those that should be
ignored (per `project-ignores').

See the doc string of `project-find-functions' for the general form
of the project instance object."
  (unless directory (setq directory default-directory))
  (let ((pr (project--find-in-directory directory)))
    (cond
     (pr)
     ((unless project-current-inhibit-prompt
        maybe-prompt)
      (setq directory (project-prompt-project-dir)
            pr (project--find-in-directory directory))))
    (when maybe-prompt
      (if pr
          (project-remember-project pr)
        (project--remove-from-project-list directory)
        (setq pr (cons 'transient directory))))
    pr))

(defun project--find-in-directory (dir)
  (run-hook-with-args-until-success 'project-find-functions dir))

(cl-defgeneric project-root (project)
  "Return root directory of the current project.

It usually contains the main build file, dependencies
configuration file, etc. Though neither is mandatory.

The directory name must be absolute."
  (car (project-roots project)))

(cl-defgeneric project-roots (project)
  "Return the list containing the current project root.

The function is obsolete, all projects have one main root anyway,
and the rest should be possible to express through
`project-external-roots'."
  ;; FIXME: Can we specify project's version here?
  ;; FIXME: Could we make this affect cl-defmethod calls too?
  (declare (obsolete project-root "0.3.0"))
  (list (project-root project)))

;; FIXME: Add MODE argument, like in `ede-source-paths'?
(cl-defgeneric project-external-roots (_project)
  "Return the list of external roots for PROJECT.

It's the list of directories outside of the project that are
still related to it.  If the project deals with source code then,
depending on the languages used, this list should include the
headers search path, load path, class path, and so on."
  nil)

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
  (defvar find-name-arg)
  (let* ((default-directory dir)
         ;; Make sure ~/ etc. in local directory name is
         ;; expanded and not left for the shell command
         ;; to interpret.
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "%s %s %s -type f %s -print0"
                          find-program
                          localdir
                          (xref--find-ignores-arguments ignores localdir)
                          (if files
                              (concat (shell-quote-argument "(")
                                      " " find-name-arg " "
                                      (mapconcat
                                       #'shell-quote-argument
                                       (split-string files)
                                       (concat " -o " find-name-arg " "))
                                      " "
                                      (shell-quote-argument ")"))"")
                          )))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

(defun project--remote-file-names (local-files)
  "Return LOCAL-FILES as if they were on the system of `default-directory'."
  (let ((remote-id (file-remote-p default-directory)))
    (if (not remote-id)
        local-files
      (mapcar (lambda (file)
                (concat remote-id file))
              local-files))))

(defgroup project-vc nil
  "Project implementation based on the VC package."
  :version "25.1"
  :group 'project)

(defcustom project-vc-ignores nil
  "List of patterns to include in `project-ignores'."
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

;; FIXME: Using the current approach, major modes are supposed to set
;; this variable to a buffer-local value.  So we don't have access to
;; the "external roots" of language A from buffers of language B, which
;; seems desirable in multi-language projects, at least for some
;; potential uses, like "jump to a file in project or external dirs".
;;
;; We could add a second argument to this function: a file extension,
;; or a language name.  Some projects will know the set of languages
;; used in them; for others, like VC-based projects, we'll need
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

The directory names should be absolute.  Used in the VC project
backend implementation of `project-external-roots'.")

(defun project-try-vc (dir)
  (let* ((backend
          ;; FIXME: This is slow. Cache it.
          (ignore-errors (vc-responsible-backend dir)))
         (root
          (pcase backend
            ('Git
             ;; Don't stop at submodule boundary.
             ;; FIXME: Cache for a shorter time.
             (or (vc-file-getprop dir 'project-git-root)
                 (let ((root (vc-call-backend backend 'root dir)))
                   (vc-file-setprop
                    dir 'project-git-root
                    (if (and
                         ;; FIXME: Invalidate the cache when the value
                         ;; of this variable changes.
                         (project--vc-merge-submodules-p root)
                         (project--submodule-p root))
                        (let* ((parent (file-name-directory
                                        (directory-file-name root))))
                          (vc-call-backend backend 'root parent))
                      root)))))
            ('nil nil)
            (_ (ignore-errors (vc-call-backend backend 'root dir))))))
    (and root (cons 'vc root))))

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
  (cdr project))

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
     (let (backend)
       (if (and (file-equal-p dir (cdr project))
                (setq backend (vc-responsible-backend dir))
                (cond
                 ((eq backend 'Hg))
                 ((and (eq backend 'Git)
                       (or
                        (not project-vc-ignores)
                        (version<= "1.9" (vc-git--program-version)))))))
           (project--vc-list-files dir backend project-vc-ignores)
         (project--files-in-directory
          dir
          (project--dir-ignores project dir)))))
   (or dirs
       (list (project-root project)))))

(declare-function vc-git--program-version "vc-git")
(declare-function vc-git--run-command-string "vc-git")
(declare-function vc-hg-command "vc-hg")

(defun project--vc-list-files (dir backend extra-ignores)
  (pcase backend
    (`Git
     (let ((default-directory (expand-file-name (file-name-as-directory dir)))
           (args '("-z"))
           files)
       ;; Include unregistered.
       (setq args (append args '("-c" "-o" "--exclude-standard")))
       (when extra-ignores
         (setq args (append args
                            (cons "--"
                                  (mapcar
                                   (lambda (i)
                                     (if (string-match "\\./" i)
                                         (format ":!/:%s" (substring i 2))
                                       (format ":!:%s" i)))
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
     (let ((default-directory (expand-file-name (file-name-as-directory dir)))
           args)
       ;; Include unregistered.
       (setq args (nconc args '("-mcardu" "--no-status" "-0")))
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
          (while (re-search-forward "path *= *\\(.+\\)" nil t)
            (push (match-string 1) res))
          (nreverse res)))
    (file-missing nil)))

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
    (define-key map "d" 'project-dired)
    (define-key map "v" 'project-vc-dir)
    (define-key map "c" 'project-compile)
    (define-key map "e" 'project-eshell)
    (define-key map "k" 'project-kill-buffers)
    (define-key map "p" 'project-switch-project)
    (define-key map "g" 'project-find-regexp)
    (define-key map "G" 'project-or-external-find-regexp)
    (define-key map "r" 'project-query-replace-regexp)
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
(declare-function xref--show-xrefs "xref")
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
  (let* ((pr (project-current t))
         (files
          (if (not current-prefix-arg)
              (project-files pr)
            (let ((dir (read-directory-name "Base directory: "
                                            nil default-directory t)))
              (project--files-in-directory dir
                                           nil
                                           (grep-read-files regexp))))))
    (xref--show-xrefs
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
         (files
          (project-files pr (cons
                             (project-root pr)
                             (project-external-roots pr)))))
    (xref--show-xrefs
     (apply-partially #'project--find-regexp-in-files regexp files)
     nil)))

(defun project--find-regexp-in-files (regexp files)
  (unless files
    (user-error "Empty file list"))
  (let ((xrefs (xref-matches-in-files regexp files)))
    (unless xrefs
      (user-error "No matches for: %s" regexp))
    xrefs))

(defun project--read-regexp ()
  (let ((sym (thing-at-point 'symbol)))
    (read-regexp "Find regexp" (and sym (regexp-quote sym)))))

;;;###autoload
(defun project-find-file ()
  "Visit a file (with completion) in the current project.
The completion default is the filename at point, if one is
recognized."
  (interactive)
  (let* ((pr (project-current t))
         (dirs (list (project-root pr))))
    (project-find-file-in (thing-at-point 'filename) dirs pr)))

;;;###autoload
(defun project-or-external-find-file ()
  "Visit a file (with completion) in the current project or external roots.
The completion default is the filename at point, if one is
recognized."
  (interactive)
  (let* ((pr (project-current t))
         (dirs (cons
                (project-root pr)
                (project-external-roots pr))))
    (project-find-file-in (thing-at-point 'filename) dirs pr)))

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
                                        hist default)
  "Read a file name, prompting with PROMPT.
ALL-FILES is a list of possible file name completions.
PREDICATE, HIST, and DEFAULT have the same meaning as in
`completing-read'."
  (let* ((common-parent-directory
          (let ((common-prefix (try-completion "" all-files)))
            (if (> (length common-prefix) 0)
                (file-name-directory common-prefix))))
         (cpd-length (length common-parent-directory))
         (prompt (if (zerop cpd-length)
                     prompt
                   (concat prompt (format " in %s" common-parent-directory))))
         (substrings (mapcar (lambda (s) (substring s cpd-length)) all-files))
         (new-collection (project--file-completion-table substrings))
         (res (project--completing-read-strict prompt
                                               new-collection
                                               predicate
                                               hist default)))
    (concat common-parent-directory res)))

(defun project--read-file-absolute (prompt
                                    all-files &optional predicate
                                    hist default)
  (project--completing-read-strict prompt
                                   (project--file-completion-table all-files)
                                   predicate
                                   hist default))

(defun project-find-file-in (filename dirs project)
  "Complete FILENAME in DIRS in PROJECT and visit the result."
  (let* ((all-files (project-files project dirs))
         (completion-ignore-case read-file-name-completion-ignore-case)
         (file (funcall project-read-file-name-function
                        "Find file" all-files nil nil
                        filename)))
    (if (string= file "")
        (user-error "You didn't specify the file")
      (find-file file))))

(defun project--completing-read-strict (prompt
                                        collection &optional predicate
                                        hist default)
  ;; Tried both expanding the default before showing the prompt, and
  ;; removing it when it has no matches.  Neither seems natural
  ;; enough.  Removal is confusing; early expansion makes the prompt
  ;; too long.
  (let* ((new-prompt (if (and default (not (string-equal default "")))
                         (format "%s (default %s): " prompt default)
                       (format "%s: " prompt)))
         (res (completing-read new-prompt
                               collection predicate t
                               nil ;; initial-input
                               hist default)))
    (when (and (equal res default)
               (not (test-completion res collection predicate)))
      (setq res
            (completing-read (format "%s: " prompt)
                             collection predicate t res hist nil)))
    res))

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

;;;###autoload
(defun project-shell ()
  "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (default-project-shell-name
           (concat "*" (file-name-nondirectory
                        (directory-file-name
                         (file-name-directory default-directory)))
                   "-shell*"))
         (shell-buffer (get-buffer default-project-shell-name)))
    (if (and shell-buffer (not current-prefix-arg))
        (pop-to-buffer shell-buffer)
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
         (eshell-buffer-name
          (concat "*" (file-name-nondirectory
                       (directory-file-name
                        (file-name-directory default-directory)))
                  "-eshell*"))
         (eshell-buffer (get-buffer eshell-buffer-name)))
    (if (and eshell-buffer (not current-prefix-arg))
        (pop-to-buffer eshell-buffer)
      (eshell t))))

;;;###autoload
(defun project-async-shell-command ()
  "Run `async-shell-command' in the current project's root directory."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'async-shell-command)))

;;;###autoload
(defun project-shell-command ()
  "Run `shell-command' in the current project's root directory."
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
If you exit the query-replace, you can later continue the query-replace
loop using the command \\[fileloop-continue]."
  (interactive
   (pcase-let ((`(,from ,to)
                (query-replace-read-args "Query replace (regexp)" t t)))
     (list from to)))
  (fileloop-initialize-replace
   from to (project-files (project-current t)) 'default)
  (fileloop-continue))

(defvar compilation-read-command)
(declare-function compilation-read-command "compile")

;;;###autoload
(defun project-compile (command &optional comint)
  "Run `compile' in the project root.
Arguments the same as in `compile'."
  (interactive
   (list
    (let ((command (eval compile-command)))
      (require 'compile)
      (if (or compilation-read-command current-prefix-arg)
	  (compilation-read-command command)
	command))
    (consp current-prefix-arg)))
  (let* ((pr (project-current t))
         (default-directory (project-root pr)))
    (compile command comint)))

(defun project--read-project-buffer ()
  (let* ((pr (project-current t))
         (current-buffer (current-buffer))
         (other-buffer (other-buffer current-buffer))
         (other-name (buffer-name other-buffer))
         (predicate
          (lambda (buffer)
            ;; BUFFER is an entry (BUF-NAME . BUF-OBJ) of Vbuffer_alist.
            (and (cdr buffer)
                 (equal pr
                        (with-current-buffer (cdr buffer)
                          (project-current)))))))
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

(defcustom project-kill-buffer-conditions
  '(buffer-file-name    ; All file-visiting buffers are included.
    ;; Most of the temp buffers in the background:
    (major-mode . fundamental-mode)
    ;; non-text buffer such as xref, occur, vc, log, ...
    (and (derived-mode . special-mode)
         (not (major-mode . help-mode)))
    (derived-mode . compilation-mode)
    (derived-mode . dired-mode)
    (derived-mode . diff-mode))
  "List of conditions to kill buffers related to a project.
This list is used by `project-kill-buffers'.
Each condition is either:
- a regular expression, to match a buffer name,
- a predicate function that takes a buffer object as argument
  and returns non-nil if the buffer should be killed,
- a cons-cell, where the car describes how to interpret the cdr.
  The car can be one of the following:
  * `major-mode': the buffer is killed if the buffer's major
    mode is eq to the cons-cell's cdr
  * `derived-mode': the buffer is killed if the buffer's major
    mode is derived from the major mode denoted by the cons-cell's
    cdr
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
  :version "28.1"
  :group 'project
  :package-version '(project . "0.6.0"))

(defun project--buffer-list (pr)
  "Return the list of all buffers in project PR."
  (let (bufs)
    (dolist (buf (buffer-list))
      (when (equal pr
                   (with-current-buffer buf
                     (project-current)))
        (push buf bufs)))
    (nreverse bufs)))

(defun project--kill-buffer-check (buf conditions)
  "Check if buffer BUF matches any element of the list CONDITIONS.
See `project-kill-buffer-conditions' for more details on the form
of CONDITIONS."
  (catch 'kill
    (dolist (c conditions)
      (when (cond
             ((stringp c)
              (string-match-p c (buffer-name buf)))
             ((symbolp c)
              (funcall c buf))
             ((eq (car-safe c) 'major-mode)
              (eq (buffer-local-value 'major-mode buf)
                  (cdr c)))
             ((eq (car-safe c) 'derived-mode)
              (provided-mode-derived-p
               (buffer-local-value 'major-mode buf)
               (cdr c)))
             ((eq (car-safe c) 'not)
              (not (project--kill-buffer-check buf (cdr c))))
             ((eq (car-safe c) 'or)
              (project--kill-buffer-check buf (cdr c)))
             ((eq (car-safe c) 'and)
              (seq-every-p
               (apply-partially #'project--kill-buffer-check
                                buf)
               (mapcar #'list (cdr c)))))
        (throw 'kill t)))))

(defun project--buffers-to-kill (pr)
  "Return list of buffers in project PR to kill.
What buffers should or should not be killed is described
in `project-kill-buffer-conditions'."
  (let (bufs)
    (dolist (buf (project--buffer-list pr))
      (when (project--kill-buffer-check buf project-kill-buffer-conditions)
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
interactively."
  (interactive)
  (let* ((pr (project-current t))
         (bufs (project--buffers-to-kill pr)))
    (cond (no-confirm
           (mapc #'kill-buffer bufs))
          ((null bufs)
           (message "No buffers to kill"))
          ((yes-or-no-p (format "Kill %d buffers in %s? "
                                (length bufs)
                                (project-root pr)))
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
      (pp project--list (current-buffer))
      (write-region nil nil filename nil 'silent))))

;;;###autoload
(defun project-remember-project (pr)
  "Add project PR to the front of the project list.
Save the result in `project-list-file' if the list of projects has changed."
  (project--ensure-read-project-list)
  (let ((dir (project-root pr)))
    (unless (equal (caar project--list) dir)
      (dolist (ent project--list)
        (when (equal dir (car ent))
          (setq project--list (delq ent project--list))))
      (push (list dir) project--list)
      (project--write-project-list))))

(defun project--remove-from-project-list (pr-dir)
  "Remove directory PR-DIR of a missing project from the project list.
If the directory was in the list before the removal, save the
result in `project-list-file'.  Announce the project's removal
from the list."
  (project--ensure-read-project-list)
  (when-let ((ent (assoc pr-dir project--list)))
    (setq project--list (delq ent project--list))
    (message "Project `%s' not found; removed from list" pr-dir)
    (project--write-project-list)))

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
         (pr-dir (completing-read "Select project: " choices nil t)))
    (if (equal pr-dir dir-choice)
        (read-directory-name "Select directory: " default-directory nil t)
      pr-dir)))

;;;###autoload
(defun project-known-project-roots ()
  "Return the list of root directories of all known projects."
  (project--ensure-read-project-list)
  (mapcar #'car project--list))


;;; Project switching

;;;###autoload
(defvar project-switch-commands
  '((?f "Find file" project-find-file)
    (?g "Find regexp" project-find-regexp)
    (?d "Dired" project-dired)
    (?v "VC-Dir" project-vc-dir)
    (?e "Eshell" project-eshell))
  "Alist mapping keys to project switching menu entries.
Used by `project-switch-project' to construct a dispatch menu of
commands available upon \"switching\" to another project.

Each element is of the form (KEY LABEL COMMAND), where COMMAND is the
command to run when KEY is pressed.  LABEL is used to distinguish
the menu entries in the dispatch menu.")

(defun project--keymap-prompt ()
  "Return a prompt for the project switching dispatch menu."
  (mapconcat
   (pcase-lambda (`(,key ,label))
     (format "[%s] %s"
             (propertize (key-description `(,key)) 'face 'bold)
             label))
   project-switch-commands
   "  "))

;;;###autoload
(defun project-switch-project ()
  "\"Switch\" to another project by running an Emacs command.
The available commands are presented as a dispatch menu
made from `project-switch-commands'."
  (interactive)
  (let ((dir (project-prompt-project-dir))
        (choice nil))
    (while (not choice)
      (setq choice (assq (read-event (project--keymap-prompt))
                         project-switch-commands)))
    (let ((default-directory dir)
          (project-current-inhibit-prompt t))
      (call-interactively (nth 2 choice)))))

(provide 'project)
;;; project.el ends here
