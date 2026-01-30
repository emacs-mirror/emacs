;;; vc-git.el --- VC backend for the git version control system -*- lexical-binding: t -*-

;; Copyright (C) 2006-2026 Free Software Foundation, Inc.

;; Author: Alexandre Julliard <julliard@winehq.org>
;; Keywords: vc tools
;; Package: vc

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

;; This file contains a VC backend for the git version control
;; system.
;;

;;; Todo:
;;  - check if more functions could use vc-git-command instead
;;     of start-process.
;;  - changelog generation

;; Implement the rest of the vc interface. See the comment at the
;; beginning of vc.el. The current status is:
;; ("??" means: "figure out what to do about it")
;;
;; FUNCTION NAME                                   STATUS
;; BACKEND PROPERTIES
;; * revision-granularity                          OK
;; - update-on-retrieve-tag                        OK
;; STATE-QUERYING FUNCTIONS
;; * registered (file)                             OK
;; * state (file)                                  OK
;; - dir-status-files (dir files uf)               OK
;; * working-revision (file)                       OK
;; * checkout-model (files)                        OK
;; - mode-line-string (file)                       OK
;; STATE-CHANGING FUNCTIONS
;; * create-repo ()                                OK
;; * register (files &optional rev comment)        OK
;; - responsible-p (file)                          OK
;; - receive-file (file rev)                       NOT NEEDED
;; - unregister (file)                             OK
;; * checkin (files comment rev)                   OK
;; - checkin-patch (patch-string comment)          OK
;; * find-revision (file rev buffer)               OK
;; * checkout (file &optional rev)                 OK
;; * revert (file &optional contents-done)         OK
;; - merge-file (file rev1 rev2)            It would be possible to merge
;;                                          changes into a single file, but
;;                                          when committing they wouldn't
;;                                          be identified as a merge
;;                                          by git, so it's probably
;;                                          not a good idea.
;; - merge-news (file)                      see `merge-file'
;; - mark-resolved (files)                         OK
;; - steal-lock (file &optional revision)          NOT NEEDED
;; - get-change-comment (files rev)                OK
;; HISTORY FUNCTIONS
;; * print-log (files buffer &optional shortlog start-revision limit)   OK
;; * incoming-revision (&optional upstream-location refresh)   OK
;; - log-search (buffer pattern)                   OK
;; - log-view-mode ()                              OK
;; - show-log-entry (revision)                     OK
;; - comment-history (file)                        ??
;; - update-changelog (files)                      COULD BE SUPPORTED
;; * diff (file &optional rev1 rev2 buffer async)  OK
;; - revision-completion-table (files)             OK
;; - annotate-command (file buf &optional rev)     OK
;; - annotate-time ()                              OK
;; - annotate-current-time ()                      NOT NEEDED
;; - annotate-extract-revision-at-line ()          OK
;; TAG/BRANCH SYSTEM
;; - create-tag (dir name branchp)                 OK
;; - retrieve-tag (dir name update)                OK
;; MISCELLANEOUS
;; - make-version-backups-p (file)                 NOT NEEDED
;; - previous-revision (file rev)                  OK
;; - next-revision (file rev)                      OK
;; - file-name-changes (rev)                       OK
;; - check-headers ()                              COULD BE SUPPORTED
;; - delete-file (file)                            OK
;; - rename-file (old new)                         OK
;; - find-file-hook ()                             OK
;; - conflicted-files                              OK
;; - repository-url (file-or-dir)                  OK
;; - prepare-patch (rev)                           OK

;;; Code:

(require 'cl-lib)
(require 'vc-dispatcher)
(eval-when-compile
  (require 'subr-x) ; for string-trim-right
  (require 'vc)
  (require 'vc-dir))

;; Pacify "free variable" warning.
(defvar log-edit-font-lock-keywords)

(defgroup vc-git nil
  "VC Git backend."
  :version "24.1"
  :group 'vc)

(defcustom vc-git-diff-switches t
  "String or list of strings specifying switches for Git diff under VC.
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "23.1")

(defcustom vc-git-annotate-switches nil
  "String or list of strings specifying switches for Git blame under VC.
If nil, use the value of `vc-annotate-switches'.  If t, use no switches.

Tip: Set this to \"-w\" to make Git blame ignore whitespace when
comparing changes.  See Man page `git-blame' for more."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "25.1")

;; Check if local value of `vc-git-annotate-switches' is safe.
;; Currently only "-w" (ignore whitespace) is considered safe, but
;; this list might be extended in the future (probably most options
;; are perfectly safe.)
;;;###autoload(put 'vc-git-annotate-switches 'safe-local-variable (lambda (switches) (equal switches "-w")))

(defcustom vc-git-log-switches nil
  "String or list of strings giving Git log switches for non-shortlogs."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string))
  :version "28.1")

(defcustom vc-git-shortlog-switches nil
  "String or list of strings giving Git log switches for shortlogs."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string))
  :version "30.1")

(defcustom vc-git-file-name-changes-switches '("-M" "-C")
  "String or list of string to pass to Git when finding previous names.

This option should usually at least contain '-M'.  You can adjust
the flags to change the similarity thresholds (default 50%).  Or
add `--find-copies-harder' (slower in large projects, since it
uses a full scan)."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string))
  :version "30.1")

(defcustom vc-git-resolve-conflicts 'default
  "Whether to mark conflicted file as resolved upon saving.
That is performed after all conflict markers in it have been removed.

If this is t and there are no more conflict markers in the file,
VC will mark the conflicts in the saved file as resolved.

If the value is `unstage-maybe', and no merge, rebase or similar
operation is in progress, then after the last conflict is resolved, also
clear the staging area.

A value of `default' means to use the value of `vc-resolve-conflicts'."
  :type '(choice (const :tag "Don't resolve" nil)
                 (const :tag "Resolve" t)
                 (const :tag "Resolve and maybe unstage all files"
                        unstage-maybe)
                 (const :tag "Use vc-resolve-conflicts" default))
  :version "31.1")

(defcustom vc-git-program "git"
  "Name of the Git executable (excluding any arguments)."
  :version "24.1"
  :type 'string)

(defcustom vc-git-root-log-format
  '("%d%h..: %an %ad %s"
    ;; The first shy group matches the characters drawn by --graph.
    ;; We use numbered groups because `log-view-message-re' wants the
    ;; revision number to be group 1.
    "^\\(?:[*/\\| ]+ \\)?\\(?2: ([^)]+)\\)?\\(?1:[0-9a-z]+\\)\\.\\.: \
\\(?3:.*?\\)[ \t]+\\(?4:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
    ((1 'log-view-message)
     (2 'change-log-list nil lax)
     (3 'change-log-name)
     (4 'change-log-date)))
  "Git log format for `vc-print-root-log'.
This should be a list (FORMAT REGEXP KEYWORDS), where FORMAT is a
format string (which is passed to \"git log\" via the argument
\"--pretty=tformat:FORMAT\"), REGEXP is a regular expression
matching the resulting Git log output, and KEYWORDS is a list of
`font-lock-keywords' for highlighting the Log View buffer."
  :type '(list string regexp (repeat sexp))
  :version "24.1")

(defcustom vc-git-commits-coding-system 'utf-8
  "Default coding system for sending commit log messages to Git.

Should be consistent with the Git config value i18n.commitEncoding,
and should also be consistent with `locale-coding-system'."
  :type '(coding-system :tag "Coding system to encode Git commit logs")
  :version "25.1")

(defcustom vc-git-log-output-coding-system 'utf-8
  "Default coding system for receiving log output from Git.

Should be consistent with the Git config value i18n.logOutputEncoding."
  :type '(coding-system :tag "Coding system to decode Git log output")
  :version "25.1")

(defcustom vc-git-grep-template "git --no-pager grep -n <C> -e <R> -- <F>"
  "The default command to run for \\[vc-git-grep].
The following place holders should be present in the string:
 <C> - place to put the options like -i.
 <F> - file names and wildcards to search.
 <R> - the regular expression searched for."
  :type 'string
  :version "27.1")

(defcustom vc-git-show-stash t
  "How much of the git stash list to show by default.
Default t means all, otherwise an integer specifying the maximum
number to show.  A text button is always shown allowing you to
toggle display of the entire list."
  :type `(choice (const :tag "All" t)
                 (integer :tag "Limit"
                          :validate
                          ,(lambda (widget)
                             (unless (>= (widget-value widget) 0)
                               (widget-put widget :error
                                           "Invalid value: must be a non-negative integer")
                               widget))))
  :version "27.1")

(defcustom vc-git-revision-complete-only-branches nil
  "Control whether tags are returned by revision completion for Git.

When non-nil, only branches and remotes will be returned by
`vc-git-revision-completion-table'.  This is used by various VC
commands when completing branch names.  When nil, tags are also
included in the completions."
  :type 'boolean
  :version "28.1")

;; History of Git commands.
(defvar vc-git-history nil)

;; Default to t because commands which don't support literal pathspecs
;; ignore the environment variable silently.
(defvar vc-git-use-literal-pathspecs t
  "Non-nil to treat pathspecs in commands literally.
Good example of file name that needs this: \"test[56].xx\".")

;; Clear up the cache to force vc-call to check again and discover
;; new functions when we reload this file.
(put 'Git 'vc-functions nil)

;;; BACKEND PROPERTIES

(defun vc-git-revision-granularity () 'repository)
(defun vc-git-checkout-model (_files) 'implicit)
(defun vc-git-update-on-retrieve-tag () nil)

;;; STATE-QUERYING FUNCTIONS

;;;###autoload (defun vc-git-registered (file)
;;;###autoload   "Return non-nil if FILE is registered with git."
;;;###autoload   (if (vc-find-root file ".git")       ; Short cut.
;;;###autoload       (progn
;;;###autoload         (load "vc-git" nil t)
;;;###autoload         (vc-git-registered file))))

(defun vc-git-registered (file)
  "Check whether FILE is registered with git."
  (let ((dir (vc-git-root file)))
    (and dir
         ;; If git(1) isn't installed then the `with-demoted-errors'
         ;; below will mean we get an error message echoed about that
         ;; fact with every `find-file'.  That's noisy, and inconsistent
         ;; with other backend's `vc-*-registered' functions which are
         ;; quieter in the case that the VCS isn't installed.  So check
         ;; up here that git(1) is available.  See also bug#18481.
         (executable-find vc-git-program t)
         (with-temp-buffer
           (let* (process-file-side-effects
                  ;; Do not use the `file-name-directory' here: git-ls-files
                  ;; sometimes fails to return the correct status for relative
                  ;; path specs.
                  ;; See also: https://marc.info/?l=git&m=125787684318129&w=2
                  (name (file-relative-name file dir))
                  (str (with-demoted-errors "Error: %S"
                         (cd dir)
                         (vc-git--out-ok "ls-files" "-c" "-z" "--" name)
                         ;; If result is empty, use ls-tree to check for deleted
                         ;; file.
                         (when (eq (point-min) (point-max))
                           (vc-git--out-ok "ls-tree" "--name-only" "-z" "HEAD"
                                           "--" name))
                         (buffer-string))))
             (and str
                  (> (length str) (length name))
                  (string= (substring str 0 (1+ (length name)))
                           (concat name "\0"))))))))

(defvar vc-git--program-version nil)

(connection-local-set-profile-variables
 'vc-git-connection-default-profile
 '((vc-git--program-version . nil)))

(connection-local-set-profiles
 '(:application vc-git)
 'vc-git-connection-default-profile)

(defun vc-git--program-version ()
  (with-connection-local-application-variables 'vc-git
   (or vc-git--program-version
       (let ((version-string
              (vc-git--run-command-string nil "version")))
         (setq-connection-local
              vc-git--program-version
              (if (and version-string
                       ;; Some Git versions append additional strings
                       ;; to the numerical version string. E.g., Git
                       ;; for Windows appends ".windows.N", while Git
                       ;; for Mac appends " (Apple Git-N)". Capture
                       ;; numerical version and ignore the rest.
                       (string-match "git version \\([0-9][0-9.]+\\)"
                                     version-string))
                  (string-trim-right (match-string 1 version-string) "\\.")
                "0"))))))

(defun vc-git--git-path (&optional path)
  "Resolve .git/PATH for the current working tree.
In particular, handle the case where this is a linked working
tree, such that .git is a plain file.

See the --git-dir and --git-path options to git-rev-parse(1)."
  (if (and path (not (string-empty-p path)))
      ;; Canonicalize in this branch because --git-dir always returns
      ;; an absolute file name.
      (expand-file-name
       (string-trim-right
        (vc-git--run-command-string nil "rev-parse"
                                    "--git-path" path)))
    (concat (string-trim-right
             (vc-git--run-command-string nil "rev-parse" "--git-dir"))
            "/")))

(defun vc-git--git-status-to-vc-state (code-list)
  "Convert CODE-LIST to a VC status.

Each element of CODE-LIST comes from the first two characters of
a line returned by `git status --porcelain' and should be passed
in the order given by `git status'."
  ;; It is necessary to allow CODE-LIST to be a list because sometimes git
  ;; status returns multiple lines, e.g. for a file that is removed from
  ;; the index but is present in the HEAD and working tree.
  (pcase code-list
    ('nil 'up-to-date)
    (`(,code)
     (pcase code
       ("!!" 'ignored)
       ("??" 'unregistered)
       ("D " 'removed)
       (_ (cond
           ((string-match-p "^.D$" code) 'missing)
           ((string-match-p "^[ M]+$" code) 'edited)
           ((string-match-p "^[ A]+$" code) 'added)
           ((string-match-p "^[ U]+$" code) 'conflict)
           (t 'edited)))))
    ;;  I know of two cases when git state returns more than one element,
    ;;  in both cases returning '("D " "??")':
    ;;  1. When a file is removed from the index but present in the
    ;;     HEAD and working tree.
    ;;  2. When a file A is renamed to B in the index and then back to A
    ;;     in the working tree.
    ;;  In both of these instances, `unregistered' is a reasonable response.
    ('("D " "??") 'unregistered)
    ;;  In other cases, let us return `edited'.
    (_ 'edited)))

(defun vc-git-state (file)
  "Git-specific version of `vc-state'."
  ;; It can't set `needs-update' or `needs-merge'. The rough
  ;; equivalent would be that upstream branch for current branch is in
  ;; fast-forward state i.e. current branch is direct ancestor of
  ;; corresponding upstream branch, and the file was modified
  ;; upstream.  We'd need to check against the upstream tracking
  ;; branch for that (an extra process call or two).
  (let* ((args
          `("status" "--porcelain" "-z"
            ;; Just to be explicit, it's the default anyway.
            "--untracked-files"
            ,@(when (version<= "1.7.6.3" (vc-git--program-version))
                '("--ignored"))
            "--"))
        (status (apply #'vc-git--run-command-string file args)))
    (if (null status)
        ;; If status is nil, there was an error calling git, likely because
        ;; the file is not in a git repo.
        'unregistered
      ;; If this code is adapted to parse 'git status' for a directory,
      ;; note that a renamed file takes up two null values and needs to be
      ;; treated slightly more carefully.
      (vc-git--git-status-to-vc-state
       (mapcar (lambda (s)
                 (substring s 0 2))
               (split-string status "\0" t))))))

(defun vc-git-working-revision (_file)
  "Git-specific version of `vc-working-revision'."
  (let (process-file-side-effects)
    (vc-git--rev-parse "HEAD")))

(defun vc-git--symbolic-ref (file)
  (or
   (vc-file-getprop file 'vc-git-symbolic-ref)
   (let* (process-file-side-effects
          (str (vc-git--run-command-string nil "symbolic-ref" "HEAD")))
     (vc-file-setprop file 'vc-git-symbolic-ref
                      (if str
                          (if (string-match "^\\(refs/heads/\\)?\\(.+\\)$" str)
                              (match-string 2 str)
                            str))))))

(defun vc-git-mode-line-string (file)
  "Return a string for `vc-mode-line' to put in the mode line for FILE."
  (pcase-let* ((backend-name "Git")
               (state (vc-state file))
               (`(,state-echo ,face ,indicator)
                (vc-mode-line-state state))
               (rev (vc-working-revision file 'Git))
               (disp-rev (or (vc-git--symbolic-ref file)
                             (and rev (substring rev 0 7))))
               (state-string (concat (unless (eq vc-display-status 'no-backend)
                                       backend-name)
                                     indicator disp-rev)))
    (propertize state-string 'face face 'help-echo
                (concat state-echo " under the " backend-name
                        " version control system"
                        "\nCurrent revision: " rev))))

(cl-defstruct (vc-git-extra-fileinfo
            (:copier nil)
            (:constructor vc-git-create-extra-fileinfo
                          (old-perm new-perm &optional rename-state orig-name))
            (:conc-name vc-git-extra-fileinfo->))
  old-perm new-perm   ;; Permission flags.
  rename-state        ;; Rename or copy state.
  orig-name)          ;; Original name for renames or copies.

(defun vc-git-escape-file-name (name)
  "Escape filename NAME if necessary."
  (if (string-match "[\n\t\"\\]" name)
      (concat "\""
              (mapconcat (lambda (c)
                   (pcase c
                     (?\n "\\n")
                     (?\t "\\t")
                     (?\\ "\\\\")
                     (?\" "\\\"")
                     (_ (char-to-string c))))
                 name "")
              "\"")
    name))

(defun vc-git-file-type-as-string (old-perm new-perm)
  "Return a string describing the file type based on its permissions."
  (let* ((old-type (ash (or old-perm 0) -9))
	 (new-type (ash (or new-perm 0) -9))
	 (str (pcase new-type
		(?\100  ;; File.
		 (pcase old-type
		   (?\100 nil)
		   (?\120 "   (type change symlink -> file)")
		   (?\160 "   (type change subproject -> file)")))
		 (?\120  ;; Symlink.
		  (pcase old-type
		    (?\100 "   (type change file -> symlink)")
		    (?\160 "   (type change subproject -> symlink)")
		    (_ "   (symlink)")))
		  (?\160  ;; Subproject.
		   (pcase old-type
		     (?\100 "   (type change file -> subproject)")
		     (?\120 "   (type change symlink -> subproject)")
		     (_ "   (subproject)")))
                  (?\110 nil)  ;; Directory (internal, not a real git state).
		  (?\000  ;; Deleted or unknown.
		   (pcase old-type
		     (?\120 "   (symlink)")
		     (?\160 "   (subproject)")))
		  (_ (format "   (unknown type %o)" new-type)))))
    (cond (str (propertize str 'face 'font-lock-comment-face))
          ((eq new-type ?\110) "/")
          (t ""))))

(defun vc-git-rename-as-string (state extra)
  "Return a string describing the copy or rename associated with INFO,
or an empty string if none."
  (let ((rename-state (when extra
			(vc-git-extra-fileinfo->rename-state extra))))
    (if rename-state
        (propertize
         (concat "   ("
                 (if (eq rename-state 'copy) "copied from "
                   (if (eq state 'added) "renamed from "
                     "renamed to "))
                 (vc-git-escape-file-name
                  (vc-git-extra-fileinfo->orig-name extra))
                 ")")
         'face 'font-lock-comment-face)
      "")))

(defun vc-git-permissions-as-string (old-perm new-perm)
  "Format a permission change as string."
  (propertize
   (if (or (not old-perm)
           (not new-perm)
           (eq 0 (logand ?\111 (logxor old-perm new-perm))))
       "  "
     (if (eq 0 (logand ?\111 old-perm)) "+x" "-x"))
  'face 'vc-dir-header))

(defun vc-git-dir-printer (info)
  "Pretty-printer for the vc-dir-fileinfo structure."
  (let* ((isdir (vc-dir-fileinfo->directory info))
	 (state (if isdir "" (vc-dir-fileinfo->state info)))
         (extra (vc-dir-fileinfo->extra info))
         (old-perm (when extra (vc-git-extra-fileinfo->old-perm extra)))
         (new-perm (when extra (vc-git-extra-fileinfo->new-perm extra))))
    (insert
     "  "
     (propertize (format "%c" (if (vc-dir-fileinfo->marked info) ?* ? ))
                 'face 'vc-dir-mark-indicator)
     "  "
     (propertize
      (format "%-12s" state)
      'face (cond ((eq state 'up-to-date) 'vc-dir-status-up-to-date)
                  ((memq state '(missing conflict)) 'vc-dir-status-warning)
                  ((eq state 'ignored) 'vc-dir-status-ignored)
                  (t 'vc-dir-status-edited))
      'mouse-face 'highlight
      'keymap vc-dir-status-mouse-map)
     "  " (vc-git-permissions-as-string old-perm new-perm)
     "    "
     (propertize (vc-git-escape-file-name (vc-dir-fileinfo->name info))
                 'face (if isdir 'vc-dir-directory
                         'vc-dir-file)
		 'help-echo
		 (if isdir
		     "Directory\nVC operations can be applied to it\nmouse-3: Pop-up menu"
		   "File\nmouse-3: Pop-up menu")
		 'keymap vc-dir-filename-mouse-map
		 'mouse-face 'highlight)
     (vc-git-file-type-as-string old-perm new-perm)
     (vc-git-rename-as-string state extra))))

(cl-defstruct (vc-git-dir-status-state
               (:copier nil)
               (:conc-name vc-git-dir-status-state->))
  ;; Current stage.
  stage
  ;; List of files still to be processed.
  files
  ;; Update function to be called at the end.
  update-function
  ;; Hash table of entries for files we've computed so far.
  (hash (make-hash-table :test 'equal)))

(defsubst vc-git-dir-status-update-file (state filename file-state file-info)
  (puthash filename (list file-state file-info)
           (vc-git-dir-status-state->hash state))
  (setf (vc-git-dir-status-state->files state)
        (delete filename (vc-git-dir-status-state->files state))))

(defun vc-git-after-dir-status-stage (git-state)
  "Process sentinel for the various dir-status stages."
  (let (next-stage
        (files (vc-git-dir-status-state->files git-state)))
    ;; First stage is always update-index.
    ;;   After that, if no commits yet, ls-files-added.
    ;;   Otherwise (there are commits), diff-index then ls-files-missing.
    ;;     After ls-files-missing, if FILES non-nil, ls-files-up-to-date.
    ;;     After ls-files-missing, if FILES     nil, ls-files-conflict.
    ;; Then always ls-files-unknown.
    ;; Finally, if FILES non-nil, ls-files-ignored.
    (goto-char (point-min))
    (pcase (vc-git-dir-status-state->stage git-state)
      ('update-index
       (setq next-stage (if (vc-git--empty-db-p) 'ls-files-added 'diff-index)))
      ('ls-files-added
       (setq next-stage 'ls-files-unknown)
       (while (re-search-forward "\\([0-7]\\{6\\}\\) [0-9a-f]\\{40\\} 0\t\\([^\0]+\\)\0" nil t)
         (let ((new-perm (string-to-number (match-string 1) 8))
               (name (match-string 2)))
           (vc-git-dir-status-update-file
            git-state name 'added
            (vc-git-create-extra-fileinfo 0 new-perm)))))
      ('ls-files-up-to-date
       (setq next-stage 'ls-files-unknown)
       (while (re-search-forward "\\([0-7]\\{6\\}\\) [0-9a-f]\\{40\\} \\([0-3]\\)\t\\([^\0]+\\)\0" nil t)
         (let* ((perm (string-to-number (match-string 1) 8))
                (state (match-string 2))
                (name (match-string 3))
                (file-info (vc-git-create-extra-fileinfo perm perm)))
           (if (equal state "0")
               (unless (gethash name (vc-git-dir-status-state->hash git-state))
                 ;; `diff-index' stage has not produced a more precise info.
                 (vc-git-dir-status-update-file
                  git-state name 'up-to-date file-info))
             ;; `diff-index' assigns `edited' status to conflicted
             ;; files, so we can't do the above in both cases.
             (vc-git-dir-status-update-file
              git-state name 'conflict file-info)))))
      ('ls-files-conflict
       (setq next-stage 'ls-files-unknown)
       ;; It's enough to look for "3" to notice a conflict.
       (while (re-search-forward "\\([0-7]\\{6\\}\\) [0-9a-f]\\{40\\} 3\t\\([^\0]+\\)\0" nil t)
         (let ((perm (string-to-number (match-string 1) 8))
               (name (match-string 2)))
           (vc-git-dir-status-update-file
            git-state name 'conflict
            (vc-git-create-extra-fileinfo perm perm)))))
      ('ls-files-missing
       (setq next-stage (if files 'ls-files-up-to-date 'ls-files-conflict))
       (while (re-search-forward "\\([^\0]*?\\)\0" nil t 1)
         (vc-git-dir-status-update-file git-state (match-string 1) 'missing
                                        (vc-git-create-extra-fileinfo 0 0))))
      ('ls-files-unknown
       (when files (setq next-stage 'ls-files-ignored))
       (while (re-search-forward "\\([^\0]*?\\)\0" nil t 1)
         (vc-git-dir-status-update-file git-state (match-string 1) 'unregistered
                                        (vc-git-create-extra-fileinfo 0 0))))
      ('ls-files-ignored
       (while (re-search-forward "\\([^\0]*?\\)\0" nil t 1)
         (vc-git-dir-status-update-file git-state (match-string 1) 'ignored
                                        (vc-git-create-extra-fileinfo 0 0))))
      ('diff-index
       ;; This is output from 'git diff-index' without --cached.
       ;; Therefore this stage compares HEAD and the working tree and
       ;; ignores the index (cf. git-diff-index(1) "RAW OUTPUT FORMAT").
       ;; In particular that means it cannot distinguish between
       ;; `removed' (deletion staged) and `missing' (deleted only in
       ;; working tree).  Set them all to `removed' and then do
       ;; `ls-files-missing' as the next stage to possibly change some
       ;; of those just set to `removed', to `missing'.
       (setq next-stage 'ls-files-missing)
       (while (re-search-forward
               ":\\([0-7]\\{6\\}\\) \\([0-7]\\{6\\}\\) [0-9a-f]\\{40\\} [0-9a-f]\\{40\\} \\(\\([ADMUT]\\)\0\\([^\0]+\\)\\|\\([CR]\\)[0-9]*\0\\([^\0]+\\)\0\\([^\0]+\\)\\)\0"
               nil t 1)
         (let ((old-perm (string-to-number (match-string 1) 8))
               (new-perm (string-to-number (match-string 2) 8))
               (state (or (match-string 4) (match-string 6)))
               (name (or (match-string 5) (match-string 7)))
               (new-name (match-string 8)))
           (if new-name                 ; Copy or rename.
               (if (eq ?C (string-to-char state))
                   (vc-git-dir-status-update-file
                    git-state new-name 'added
                    (vc-git-create-extra-fileinfo old-perm new-perm
                                                  'copy name))
                 (vc-git-dir-status-update-file
                  git-state name 'removed
                  (vc-git-create-extra-fileinfo 0 0 'rename new-name))
                 (vc-git-dir-status-update-file
                  git-state new-name 'added
                  (vc-git-create-extra-fileinfo old-perm new-perm
                                                'rename name)))
             (vc-git-dir-status-update-file
              git-state name (pcase (string-to-char state)
                               (?M 'edited)
                               (?A 'added)
                               (?C 'added)
                               (?D 'removed)
                               (?U 'edited)  ;; FIXME
                               (?T 'edited)) ;; FIXME
              (vc-git-create-extra-fileinfo old-perm new-perm)))))))
    ;; If we had files but now we don't, it's time to stop.
    (when (and files (not (vc-git-dir-status-state->files git-state)))
      (setq next-stage nil))
    (setf (vc-git-dir-status-state->stage git-state) next-stage)
    (setf (vc-git-dir-status-state->files git-state) files)
    (if next-stage
        (vc-git-dir-status-goto-stage git-state)
      (funcall (vc-git-dir-status-state->update-function git-state)
               (let ((result nil))
                 (maphash (lambda (key value)
                            (push (cons key value) result))
                          (vc-git-dir-status-state->hash git-state))
                 result)
               nil))))

;; Follows vc-git-command (or vc-do-async-command), which uses vc-do-command
;; from vc-dispatcher.
(declare-function vc-exec-after "vc-dispatcher" (code &optional okstatus proc))
;; Follows vc-exec-after.
(declare-function vc-set-async-update "vc-dispatcher" (process-buffer))

(defun vc-git-dir-status-goto-stage (git-state)
  ;; TODO: Look into reimplementing this using `git status --porcelain=v2'.
  (let ((files (vc-git-dir-status-state->files git-state))
        (allowed-exit 1))
    (erase-buffer)
    (pcase (vc-git-dir-status-state->stage git-state)
      ('update-index
       (if files
           (progn (vc-git-command (current-buffer) 'async files
                                  "add" "--refresh" "--")
                  ;; git-add exits 128 if some of FILES are untracked;
                  ;; we can ignore that (bug#79999).
                  (setq allowed-exit 128))
         (vc-git-command (current-buffer) 'async nil
                         "update-index" "--refresh")))
      ('ls-files-added
       (vc-git-command (current-buffer) 'async files
                       "ls-files" "-z" "-c" "-s" "--"))
      ('ls-files-up-to-date
       (vc-git-command (current-buffer) 'async files
                       "ls-files" "-z" "-c" "-s" "--"))
      ('ls-files-conflict
       (vc-git-command (current-buffer) 'async files
                       "ls-files" "-z" "-u" "--"))
      ('ls-files-missing
       (vc-git-command (current-buffer) 'async files
                       "ls-files" "-z" "-d" "--"))
      ('ls-files-unknown
       (vc-git-command (current-buffer) 'async files
                       "ls-files" "-z" "-o" "--exclude-standard" "--"))
      ('ls-files-ignored
       (vc-git-command (current-buffer) 'async files
                       "ls-files" "-z" "-o" "-i" "--directory"
                       "--no-empty-directory" "--exclude-standard" "--"))
      ;; --relative added in Git 1.5.5.
      ('diff-index
       (vc-git-command (current-buffer) 'async files
                       "diff-index" "--relative" "-z" "-M" "HEAD" "--")))
    (vc-run-delayed-success allowed-exit
      (vc-git-after-dir-status-stage git-state))))

(defun vc-git-dir-status-files (_dir files update-function)
  "Return a list of (FILE STATE EXTRA) entries for DIR."
  ;; Further things that would have to be fixed later:
  ;; - how to handle unregistered directories
  ;; - how to support vc-dir on a subdir of the project tree
  (vc-git-dir-status-goto-stage
   (make-vc-git-dir-status-state :stage 'update-index
                                 :files files
                                 :update-function update-function)))

(defun vc-git-working-branch ()
  "Return the name of the current branch, or nil if HEAD is detached."
  (vc-git--out-match '("symbolic-ref" "HEAD")
                     "^\\(refs/heads/\\)?\\(.+\\)$" 2))

(defun vc-git--branch-remotes ()
  "Return alist of configured remote branches for current branch.
If there is a configured upstream, return the remote-tracking branch
with key `upstream'.  If there is a distinct configured push remote,
return the remote-tracking branch there with key `push'.
A configured push remote that's just the same as the upstream remote is
ignored because that means we're not actually in a triangular workflow."
  ;; Possibly we could simplify this using @{push}, but that may involve
  ;; an unwanted dependency on the setting of push.default.
  (cl-flet ((get (key)
              (string-trim-right (vc-git--out-str "config" key))))
    (let* ((branch (vc-git-working-branch))
           (pull (get (format "branch.%s.remote" branch)))
           (merge (string-remove-prefix "refs/heads/"
                                        (get (format "branch.%s.merge"
                                                     branch))))
           (push (get (format "branch.%s.pushRemote" branch)))
           (push (if (string-empty-p push)
                     (get "remote.pushDefault")
                   push))
           (alist (and (not (string-empty-p pull))
                       (not (string-empty-p merge))
                       `((upstream . ,(format "%s/%s" pull merge))))))
      (if (or (string-empty-p push) (equal push pull))
          alist
        (cl-acons 'push (format "%s/%s" push branch) alist)))))

(defun vc-git-trunk-or-topic-p ()
  "Return `topic' if branch has distinct pull and push remotes, else nil.
This is able to identify topic branches for certain forge workflows."
  (let ((remotes (vc-git--branch-remotes)))
    (and (assq 'upstream remotes) (assq 'push remotes) 'topic)))

(defun vc-git-topic-outgoing-base ()
  "Return the outgoing base for the current branch as a string.
This works by considering the current branch as a topic branch
(whether or not it actually is).

If there is a distinct push remote for this branch, assume the target
for outstanding changes is the tracking branch, and return that.

Otherwise, fall back to the following algorithm, which requires that the
corresponding trunk exists as a local branch.  Find all merge bases
between the current branch and other local branches.  Each of these is a
commit on the current branch.  Use `git merge-base --independent' on
them all to find the topologically most recent.  Take the branch for
which that commit is a merge base with the current branch to be the
branch into which the current branch will eventually be merged.  Find
its upstream.  (If there is more than one branch whose merge base with
the current branch is that same topologically most recent commit, try
them one-by-one, accepting the first that has an upstream.)"
  (if-let* ((remotes (vc-git--branch-remotes))
            (_ (assq 'push remotes))
            (upstream (assq 'upstream remotes)))
      (cdr upstream)
    (cl-flet ((get-line () (buffer-substring (point) (pos-eol))))
      (let* ((branches (vc-git-branches))
             (current (pop branches))
             merge-bases)
        (with-temp-buffer
          (dolist (branch branches)
            (erase-buffer)
            (when (vc-git--out-ok "merge-base" "--all" branch current)
              (goto-char (point-min))
              (while (not (eobp))
                (push branch (alist-get (get-line) merge-bases
                                        nil nil #'equal))
                (forward-line 1))))
          (erase-buffer)
          (unless (apply #'vc-git--out-ok "merge-base" "--independent"
                         (mapcar #'car merge-bases))
            (error "`git merge-base --independent' failed"))
          ;; If 'git merge-base --independent' printed more than one
          ;; line, just pick the first.
          (goto-char (point-min))
          (catch 'ret
            (dolist (target (cdr (assoc (get-line) merge-bases)))
              (erase-buffer)
              (when (vc-git--out-ok "for-each-ref"
                                    "--format=%(upstream:short)"
                                    (concat "refs/heads/" target))
                (goto-char (point-min))
                (let ((outgoing-base (get-line)))
                  (unless (string-empty-p outgoing-base)
                    (throw 'ret outgoing-base)))))))))))

(defun vc-git-dir--branch-headers ()
  "Return headers for branch-related information."
  (let ((branch (vc-git-working-branch))
        tracking remote-url)
    (if branch
        (when-let* ((branch-merge
                     (vc-git--out-match
                      `("config" ,(concat "branch." branch ".merge"))
                      "^\\(refs/heads/\\)?\\(.+\\)$" 2))
                    (branch-remote
                     (vc-git--out-match
                      `("config" ,(concat "branch." branch ".remote"))
                      "\\([^\n]+\\)" 1)))
          (if (string= branch-remote ".")
              (setq tracking branch-merge
                    remote-url "none (tracking local branch)")
            (setq tracking (concat branch-remote "/" branch-merge)
                  remote-url (vc-git-repository-url
                              default-directory branch-remote))))
      (setq branch "none (detached HEAD)"))
    (cl-flet ((fmt (key value)
                (concat
                 (propertize (format "% -11s: " key) 'face 'vc-dir-header)
                 (propertize value 'face 'vc-dir-header-value))))
      (remove nil (list
                   (fmt "Branch" branch)
                   (and tracking (fmt "Tracking" tracking))
                   (and remote-url (fmt "Remote" remote-url)))))))

(defun vc-git--cmds-in-progress ()
  "Return a list of Git commands in progress in this worktree."
  (let ((gitdir (vc-git--git-path))
        cmds)
    ;; See contrib/completion/git-prompt.sh in git.git.
    (when (file-exists-p (expand-file-name "REVERT_HEAD" gitdir))
      (push 'revert cmds))
    (when (file-exists-p (expand-file-name "CHERRY_PICK_HEAD" gitdir))
      (push 'cherry-pick cmds))
    (when (or (file-directory-p
	       (expand-file-name "rebase-merge" gitdir))
	      (file-exists-p
	       (expand-file-name "rebase-apply/rebasing" gitdir)))
      (push 'rebase cmds))
    (when (file-exists-p
	   (expand-file-name "rebase-apply/applying" gitdir))
      (push 'am cmds))
    (when (file-exists-p (expand-file-name "MERGE_HEAD" gitdir))
      (push 'merge cmds))
    (when (file-exists-p (expand-file-name "BISECT_START" gitdir))
      (push 'bisect cmds))
    cmds))

(defun vc-git-dir--in-progress-headers ()
  "Return headers for Git commands in progress in this worktree."
  (let ((cmds (vc-git--cmds-in-progress)))
    (cl-flet ((fmt (cmd name)
                (when (memq cmd cmds)
                  ;; For now just a heading, key bindings can be added
                  ;; later for various bisect actions.
                  (propertize (format "% -11s: in progress" name)
                              'face 'vc-dir-status-warning))))
      (remove nil (list (fmt 'bisect "Bisect")
                        (fmt 'rebase "Rebase"))))))

(defvar-keymap vc-git-stash-shared-map
  "S" #'vc-git-stash-snapshot
  "C" #'vc-git-stash)

(defvar-keymap vc-git-stash-map
  :parent vc-git-stash-shared-map
  ;; Turn off vc-dir marking
  "<mouse-2>"      #'ignore

  "<down-mouse-3>" #'vc-git-stash-menu
  "C-k"            #'vc-git-stash-delete-at-point
  "="              #'vc-git-stash-show-at-point
  "RET"            #'vc-git-stash-show-at-point
  "A"              #'vc-git-stash-apply-at-point
  "P"              #'vc-git-stash-pop-at-point
  "D"              #'vc-git-stash-delete-at-point)

(defvar-keymap vc-git-stash-button-map
  :parent vc-git-stash-shared-map
  "<mouse-2>" #'push-button
  "RET"     #'push-button)

(defconst vc-git-stash-shared-help
  "\\<vc-git-stash-shared-map>\\[vc-git-stash]: Create named stash\n\\[vc-git-stash-snapshot]: Snapshot: stash from current tree")

(defconst vc-git-stash-list-help (concat "\\<vc-git-stash-map>mouse-3: Show stash menu\n\\[vc-git-stash-show-at-point], =: Show stash\n\\[vc-git-stash-apply-at-point]: Apply stash\n\\[vc-git-stash-pop-at-point]: Apply and remove stash (pop)\n\\[vc-git-stash-delete-at-point]: Delete (drop) stash\n"
                                         vc-git-stash-shared-help))

(defun vc-git--make-button-text (show count1 count2)
  (propertize
   (if show
       (format "Show all stashes (%s)" count2)
     (if (= count1 count2)
         (format "Hide all stashes (%s)" count2)
       (format "Show %s stash%s (of %s)" count1 (if (= count1 1) "" "es") count2)))
   'keymap vc-git-stash-button-map))

(defun vc-git-make-stash-button (show count1 count2)
  (let ((orig-text (vc-git--make-button-text show count1 count2)))
    (make-text-button
     orig-text nil
     'action
     (lambda (counts)
       (let* ((inhibit-read-only t)
              (start (next-single-property-change
                      (point-min) 'vc-git-hideable))
              (end (next-single-property-change
                    start 'vc-git-hideable))
              (state (get-text-property start 'invisible)))
         (add-text-properties
          start end
          `(invisible ,(not state)))
         (save-excursion
           (delete-region (button-start (point)) (button-end (point)))
           (insert (vc-git-make-stash-button
                    (not state) (car counts) (cdr counts))))))
     'button-data (cons count1 count2)
     'help-echo (concat "mouse-2, RET: Show/hide stashes\n" vc-git-stash-shared-help))))

(defvar vc-git-stash-menu-map
  (let ((map (make-sparse-keymap "Git Stash")))
    (define-key map [sn]
      '(menu-item "Snapshot Stash" vc-git-stash-snapshot
		  :help "Create stash from the current tree state"))
    (define-key map [cr]
      '(menu-item "Create Named Stash" vc-git-stash
		  :help "Create named stash"))
    (define-key map [de]
      '(menu-item "Delete Stash" vc-git-stash-delete-at-point
		  :help "Delete (drop) the current stash"))
    (define-key map [ap]
      '(menu-item "Apply Stash" vc-git-stash-apply-at-point
		  :help "Apply the current stash and keep it in the stash list"))
    (define-key map [po]
      '(menu-item "Apply and Remove Stash (Pop)" vc-git-stash-pop-at-point
		  :help "Apply the current stash and remove it (pop)"))
    (define-key map [sh]
      '(menu-item "Show Stash" vc-git-stash-show-at-point
		  :help "Show the contents of the current stash"))
    map))

(defun vc-git-dir--stash-headers ()
  "Return headers describing the current stashes."
  (list
   (concat
    (propertize "Stash      : " 'face 'vc-dir-header)
    (if-let* ((stash-list (vc-git-stash-list)))
        (let* ((len (length stash-list))
               (limit
                (if (integerp vc-git-show-stash)
                    (min vc-git-show-stash len)
                  len))
               (shown-stashes (cl-subseq stash-list 0 limit))
               (hidden-stashes (cl-subseq stash-list limit))
               (all-hideable (or (eq vc-git-show-stash t)
                                 (<= len vc-git-show-stash))))
          (concat
           ;; Button to toggle visibility.
           (if all-hideable
               (vc-git-make-stash-button nil limit limit)
             (vc-git-make-stash-button t vc-git-show-stash len))
           ;; Stash list.
           (when shown-stashes
             (concat
              (propertize "\n"
                          'vc-git-hideable all-hideable)
              (mapconcat
               (lambda (x)
                 (propertize x
                             'face 'vc-dir-header-value
                             'mouse-face 'highlight
                             'vc-git-hideable all-hideable
                             'help-echo vc-git-stash-list-help
                             'keymap vc-git-stash-map))
               shown-stashes
               (propertize "\n"
                           'vc-git-hideable all-hideable))))
           (when hidden-stashes
             (concat
              (propertize "\n"
                          'invisible t
                          'vc-git-hideable t)
              (mapconcat
               (lambda (x)
                 (propertize x
                             'face 'vc-dir-header-value
                             'mouse-face 'highlight
                             'invisible t
                             'vc-git-hideable t
                             'help-echo vc-git-stash-list-help
                             'keymap vc-git-stash-map))
               hidden-stashes
               (propertize "\n"
                           'invisible t
                           'vc-git-hideable t))))))
      (propertize "Nothing stashed"
		  'help-echo vc-git-stash-shared-help
                  'keymap vc-git-stash-shared-map
		  'face 'vc-dir-header-value)))))

(defun vc-git-dir-extra-headers (dir)
  (let ((default-directory dir))
    (string-join
     (append
      ;; Each helper returns a list of headers.  Each header must be a
      ;; propertized string with no final newline.
      (vc-git-dir--branch-headers)
      (vc-git-dir--in-progress-headers)
      (vc-git-dir--stash-headers))
     "\n")))

(defun vc-git-branches ()
  "Return the existing branches, as a list of strings.
The car of the list is the current branch."
  (with-temp-buffer
    ;; 'git branch' is a porcelain command whose output could change in
    ;; the future.
    (vc-git--call nil t "for-each-ref"
                  "--format=%(HEAD) %(refname:short)" "refs/heads/")
    (goto-char (point-min))
    (let (current-branch branches)
      (while (not (eobp))
	(when (looking-at "^\\([ *]\\) \\(.+\\)$")
	  (if (string-equal (match-string 1) "*")
	      (setq current-branch (match-string 2))
	    (push (match-string 2) branches)))
	(forward-line 1))
      (cons current-branch (nreverse branches)))))

;;; STATE-CHANGING FUNCTIONS

(defcustom vc-git-log-edit-summary-target-len nil
  "Target length for Git commit summary lines.
If a number, characters in Summary: lines beyond this length are
displayed in the `vc-git-log-edit-summary-target-warning' face.
A value of any other type means no highlighting.

By setting this to an integer around 50, you can improve the
compatibility of your commit messages with Git commands that
print the summary line in width-constrained contexts.  However,
many commit summaries will need to exceed this length.

See also `vc-git-log-edit-summary-max-len'."
  :type '(choice (const :tag "No target" nil)
                 (natnum :tag "Target length"))
  :safe (lambda (x) (or (not x) (natnump x))))

(defface vc-git-log-edit-summary-target-warning
  '((t :inherit warning))
  "Face for Git commit summary lines beyond the target length.
See `vc-git-log-edit-summary-target-len'.")

(defcustom vc-git-log-edit-summary-max-len 68
  "Maximum length for Git commit summary lines.
If a number, characters in summary lines beyond this length are
displayed in the `vc-git-log-edit-summary-max-warning' face.
A value of any other type means no highlighting.

It is good practice to avoid writing summary lines longer than
this because otherwise the summary line will be truncated in many
contexts in which Git commands display summary lines.

See also `vc-git-log-edit-summary-target-len'."
  :type '(choice (const :tag "No target" nil)
                 (natnum :tag "Target length"))
  :safe (lambda (x) (or (not x) (natnump x))))

(defface vc-git-log-edit-summary-max-warning
  '((t :inherit error))
  "Face for Git commit summary lines beyond the maximum length.
See `vc-git-log-edit-summary-max-len'.")

(defun vc-git-create-repo ()
  "Create a new Git repository."
  (vc-git-command nil 0 nil "init"))

(defun vc-git-register (files &optional _comment)
  "Register FILES into the git version-control system."
  (let (flist dlist)
    (dolist (crt files)
      (if (file-directory-p crt)
	  (push crt dlist)
	(push crt flist)))
    (when flist
      (vc-git-command nil 0 flist "update-index" "--add" "--"))
    (when dlist
      (vc-git-command nil 0 dlist "add"))))

(defalias 'vc-git-responsible-p #'vc-git-root)

(defun vc-git-unregister (file)
  (vc-git-command nil 0 file "rm" "-f" "--cached" "--"))

(declare-function log-edit-mode "log-edit" ())
(declare-function log-edit-toggle-header "log-edit" (header value))
(declare-function log-edit-extract-headers "log-edit" (headers string))
(declare-function log-edit--toggle-amend "log-edit" (last-msg-fn))

(defun vc-git-log-edit-toggle-signoff ()
  "Toggle whether to add the \"Signed-off-by\" line at the end of commit message."
  (interactive)
  (log-edit-toggle-header "Sign-Off" "yes"))

(defun vc-git-log-edit-toggle-no-verify ()
  "Toggle whether to bypass the pre-commit and commit-msg hooks."
  (interactive)
  (log-edit-toggle-header "No-Verify" "yes"))

(defun vc-git-log-edit-toggle-amend ()
  "Toggle whether this will amend the previous commit.
If toggling on, also insert its message into the buffer."
  (interactive)
  (vc-git--assert-allowed-rewrite (vc-git--rev-parse "HEAD"))
  (log-edit--toggle-amend (lambda ()
                            (vc-git-get-change-comment nil "HEAD"))))

(defvar-keymap vc-git-log-edit-mode-map
  :name "Git-Log-Edit"
  "C-c C-s" #'vc-git-log-edit-toggle-signoff
  "C-c C-n" #'vc-git-log-edit-toggle-no-verify
  "C-c C-e" #'vc-git-log-edit-toggle-amend)

(defun vc-git--log-edit-summary-check (limit)
  (and-let* (((re-search-forward "^Summary: " limit t))
             (regex
              (cond ((and (natnump vc-git-log-edit-summary-max-len)
                          (natnump vc-git-log-edit-summary-target-len))
                     (format ".\\{,%d\\}\\(.\\{,%d\\}\\)\\(.*\\)"
                             vc-git-log-edit-summary-target-len
                             (- vc-git-log-edit-summary-max-len
                                vc-git-log-edit-summary-target-len)))
                    ((natnump vc-git-log-edit-summary-max-len)
                     (format ".\\{,%d\\}\\(?2:.*\\)"
                             vc-git-log-edit-summary-max-len))
                    ((natnump vc-git-log-edit-summary-target-len)
                     (format ".\\{,%d\\}\\(.*\\)"
                             vc-git-log-edit-summary-target-len)))))
    (re-search-forward regex limit t)))

(define-derived-mode vc-git-log-edit-mode log-edit-mode "Log-Edit/git"
  "Major mode for editing Git log messages.
It is based on `log-edit-mode', and has Git-specific extensions."
  (setq-local
   log-edit-font-lock-keywords
   (append log-edit-font-lock-keywords
           '((vc-git--log-edit-summary-check
	      (1 'vc-git-log-edit-summary-target-warning prepend t)
              (2 'vc-git-log-edit-summary-max-warning prepend t))))))

(autoload 'vc-switches "vc")

(defun vc-git--log-edit-extract-headers (comment)
  (cl-flet ((boolean-arg-fn (argument)
              (lambda (v) (and (equal v "yes") (list argument)))))
    (log-edit-extract-headers
     `(("Author" . "--author")
       ("Date" . "--date")
       ("Amend" . ,(boolean-arg-fn "--amend"))
       ("No-Verify" . ,(boolean-arg-fn "--no-verify"))
       ("Sign-Off" . ,(boolean-arg-fn "--signoff")))
     comment)))

(cl-defmacro vc-git--with-apply-temp
    ((temp &optional buffer okstatus &rest args) &body body)
  (declare (indent 1))
  `(let ((,temp (make-nearby-temp-file ,(format "git-%s" temp))))
     (unwind-protect (progn ,@body
                            ;; This uses `file-local-name' to strip the
                            ;; TRAMP prefix, not `file-relative-name',
                            ;; because we've had at least one problem
                            ;; report where relativizing the file name
                            ;; meant that Git failed to find it.
                            (vc-git-command ,buffer ,(or okstatus 0)
                                            nil "apply"
                                            ,@(or args '("--cached"))
                                            (file-local-name ,temp)))
       (delete-file ,temp))))

(defalias 'vc-git-async-checkins #'always)

(defalias 'vc-git-working-revision-symbol (cl-constantly "HEAD"))

(defun vc-git--checkin (comment &optional files patch-string)
  "Workhorse routine for `vc-git-checkin' and `vc-git-checkin-patch'.
COMMENT is the commit message; must be non-nil.
For a regular checkin, FILES is the list of files to check in.
To check in a patch, PATCH-STRING is the patch text.
It is an error to supply both or neither."
  (unless (xor files patch-string)
    (error "Invalid call to `vc-git--checkin'"))
  (let* ((file1 (or (car files) default-directory))
         (root (vc-git-root file1))
         (default-directory (expand-file-name root))
         (only (or (cdr files)
                   (not (equal root (abbreviate-file-name file1)))))
         (pcsw coding-system-for-write)
         (coding-system-for-write
          ;; On MS-Windows, we must encode command-line arguments in
          ;; the system codepage.
          (if (eq system-type 'windows-nt)
              locale-coding-system
            (or coding-system-for-write vc-git-commits-coding-system)))
         (msg-file
          ;; On MS-Windows, pass the commit log message through a
          ;; file, to work around the limitation that command-line
          ;; arguments must be in the system codepage, and therefore
          ;; might not support the non-ASCII characters in the log
          ;; message.  Handle also remote files.
          (if (eq system-type 'windows-nt)
              (let ((default-directory (or (file-name-directory file1)
                                           default-directory)))
                (make-nearby-temp-file "git-msg"))))
         to-stash)
    (when patch-string
      (unless (zerop (vc-git-command nil t nil "diff" "--cached" "--quiet"))
        ;; Check that what's already staged is compatible with what
        ;; we want to commit (bug#60126).
        ;;
        ;; 1. If the changes to a file in the index are identical to
        ;;    the changes to that file we want to commit, remove the
        ;;    changes from our patch, and let the commit take them
        ;;    from the index.  This is necessary for adding and
        ;;    removing files to work.
        ;;
        ;; 2. If the changes to a file in the index are different to
        ;;    changes to that file we want to commit, then we have to
        ;;    unstage the changes or abort.
        ;;
        ;; 3. If there are changes to a file in the index but we don't
        ;;    want to commit any changes to that file, we need to
        ;;    stash those changes before committing.
        (with-temp-buffer
          ;; If the user has switches like -D, -M etc. in their
          ;; `vc-git-diff-switches', we must pass them here too, or
          ;; our string matches will fail.
          (if vc-git-diff-switches
              (apply #'vc-git-command (current-buffer) t nil
                     "diff" "--cached" (vc-switches 'git 'diff))
            ;; Following code doesn't understand plain diff(1) output.
            (user-error "Cannot commit patch with nil `vc-git-diff-switches'"))
          (goto-char (point-min))
          (let ((pos (point)) file-name file-header file-diff file-beg)
            (while (not (eobp))
              (when (and (looking-at "^diff --git a/\\(.+\\) b/\\(.+\\)")
                         (string= (match-string 1) (match-string 2)))
                (setq file-name (match-string 1)))
              (forward-line 1)          ; skip current "diff --git" line
              (setq file-header (buffer-substring pos (point)))
              (search-forward "diff --git" nil 'move)
              (move-beginning-of-line 1)
              (setq file-diff (buffer-substring pos (point)))
              (cond ((and (setq file-beg
                                (string-search file-diff patch-string))
                          ;; Check that file diff ends with an empty string
                          ;; or the beginning of the next file diff.
                          (string-match-p "\\`\\'\\|\\`diff --git"
                                          (substring patch-string
                                                     (+ file-beg
                                                        (length file-diff)))))
                     (setq patch-string
                           (string-replace file-diff "" patch-string)))
                    ((string-match (format "^%s" (regexp-quote file-header))
                                   patch-string)
                     (if (and file-name
                              (yes-or-no-p
                               (format "Unstage already-staged changes to %s?"
                                       file-name)))
                         (vc-git-command nil 0 file-name "reset" "-q" "--")
                       (user-error "Index not empty")))
                    (t (push file-name to-stash)))
              (setq pos (point))))))
      (unless (string-empty-p patch-string)
        (let (;; Temporarily countermand the let-binding at the
              ;; beginning of this function.
              (coding-system-for-write
               (coding-system-change-eol-conversion
                ;; On DOS/Windows, it is important for the patch file
                ;; to have the Unix EOL format, because Git expects
                ;; that, even on Windows.
                (or pcsw vc-git-commits-coding-system) 'unix)))
          (vc-git--with-apply-temp (patch)
            (with-temp-file patch
              (insert patch-string)))))
      (when to-stash (vc-git--stash-staged-changes to-stash)))
    (let ((files (and only (not patch-string) files))
          (args (vc-git--log-edit-extract-headers comment))
          (buffer (format "*vc-git : %s*" (expand-file-name root)))
          (post
           (lambda ()
             (when (and msg-file (file-exists-p msg-file))
               (delete-file msg-file))
             ;; If PATCH-STRING didn't come from C-x v = or C-x v D, we
             ;; now need to update the working tree to include the
             ;; changes from the commit we just created.
             ;; If there are conflicts we want to favor the working
             ;; tree's version and the version from the commit will just
             ;; show up in the diff of uncommitted changes.
             ;;
             ;; 'git apply --3way --ours' is the way Git provides to
             ;; achieve this.  This requires that the index match the
             ;; working tree and also implies the --index option, which
             ;; means applying the changes to the index in addition to
             ;; the working tree.  These are both okay here because
             ;; before doing this we know the index is empty (we just
             ;; committed) and so we can just make use of it and reset
             ;; afterwards.
             (when patch-string
               (vc-git-command nil 0 nil "add" "--all")
               (with-temp-buffer
                 (vc-git--with-apply-temp (patch t 1 "--3way")
                   (with-temp-file patch
                     (insert patch-string)))
                 ;; We could delete the following if we could also pass
                 ;; --ours to git-apply, but that is only available in
                 ;; recent versions of Git.  --3way is much older.
                 (cl-loop
                  initially (goto-char (point-min))
                  ;; git-apply doesn't apply Git's usual quotation and
                  ;; escape rules for printing file names so we can do
                  ;; this simple regexp processing.
                  ;; (Passing -z does not affect the relevant output.)
                  while (re-search-forward "^U " nil t)
                  collect (buffer-substring-no-properties (point)
                                                          (pos-eol))
                  into paths
                  finally (when paths
                            (vc-git-command nil 0 paths
                                            "checkout" "--ours"))))
               (vc-git-command nil 0 nil "reset"))
             (when to-stash
               (vc-git--with-apply-temp (cached)
                 (with-temp-file cached
                   (vc-git-command t 0 nil "stash" "show" "-p")))
               (vc-git-command nil 0 nil "stash" "drop")))))
      (when msg-file
        (let ((coding-system-for-write
               (or pcsw vc-git-commits-coding-system)))
          (write-region (car args) nil msg-file))
        (setq args (cdr args)))
      (setq args (nconc (if msg-file
                            (list "commit" "-F"
                                  (file-local-name msg-file))
                          (list "commit" "-m"))
                        args
                        ;; When operating on the whole tree, better pass
                        ;; "-a" than ".", since "."  fails when we're
                        ;; committing a merge.
                        (and (not patch-string)
                             (if only (list "--only" "--") '("-a")))))
      (if vc-async-checkin
          (let ((proc (apply #'vc-do-async-command buffer root
                             vc-git-program (nconc args files))))
            (set-process-query-on-exit-flag proc t)
            (vc-wait-for-process-before-save
             proc
             "Finishing checking in files...")
            (with-current-buffer buffer
              (vc-run-delayed
                (vc-compilation-mode 'git)
                (funcall post)))
            (vc-set-async-update buffer)
            (list 'async (get-buffer-process buffer)))
        (apply #'vc-git-command nil 0 files args)
        (funcall post)))))

(defun vc-git--mailinfo (patch-string)
  "Pipe PATCH-STRING to git-mailinfo(1) and return an alist of its output.

The alist always contains an entry with key `message'.
This contains the commit log message.
In the case that there is also an alist entry with key \"Subject\", the
first line of the commit message is missing from the `message' entry.
To recover the full commit message, concatenate the \"Subject\" and
`message' entries, interpolating two newline characters.

The alist also always contains an entry with key `patch'.
This contains the patch extracted from PATCH-STRING.
If there is text in PATCH-STRING occurring before the actual hunks but
after the commit message, separated from the latter with a line
consisting of three hyphens, then that extra text is included in this
alist entry.  (This space between the line of three hyphens and the
hunks is conventionally used for a diffstat, and/or additional
explanatory text submitted with the patch but not to be included in the
commit log message.)

The remaining entries in the alist correspond to the information
returned by git-mailinfo(1) on standard output.  These specify the
authorship and date information for the commit, and sometimes the first
line of the commit message in an entry with key \"Subject\"."
  (let ((input-file (make-nearby-temp-file "git-mailinfo-input"))
        (msg-file (make-nearby-temp-file "git-mailinfo-msg"))
        (patch-file (make-nearby-temp-file "git-mailinfo-patch"))
        (coding-system-for-read (or coding-system-for-read
                                    vc-git-log-output-coding-system))
        res)
    (unwind-protect
        (with-temp-buffer
          (let ((coding-system-for-write
                 ;; Git expects Unix line endings here even on Windows.
                 (coding-system-change-eol-conversion
                  (or coding-system-for-write vc-git-commits-coding-system)
                  'unix)))
            (with-temp-file input-file
              (insert patch-string)))
          (let ((coding-system-for-write
                 ;; On MS-Windows, we must encode command-line arguments
                 ;; in the system codepage.
                 (if (eq system-type 'windows-nt)
                     locale-coding-system
                   coding-system-for-write)))
            (vc-git--call input-file t "mailinfo" msg-file patch-file))
          (goto-char (point-min))
          ;; git-mailinfo joins up any header continuation lines for us.
          (while (re-search-forward "^\\([^\t\n\s:]+\\):\\(.*\\)$" nil t)
            (push (cons (match-string 1) (string-trim (match-string 2)))
                  res))
          (erase-buffer)
          (insert-file-contents-literally patch-file)
          (push (cons 'patch (buffer-string)) res)
          (erase-buffer)
          (insert-file-contents-literally msg-file)
          (push (cons 'message (string-trim (buffer-string))) res))
      (dolist (file (list input-file msg-file patch-file))
        (when (file-exists-p file)
          (delete-file file))))
    res))

(defun vc-git-checkin-patch (patch-string comment)
  "Git-specific version of `vc-BACKEND-checkin-patch'."
  (let ((mailinfo (vc-git--mailinfo patch-string)))
    (unless comment
      (setq comment (if-let* ((subject (assoc "Subject" mailinfo)))
                        (format "Summary: %s\n\n%s"
                                (cdr subject)
                                (cdr (assq 'message mailinfo)))
                      (cdr (assq 'message mailinfo)))))
    (when-let* ((date (assoc "Date" mailinfo)))
      (setq comment (format "Date: %s\n%s" (cdr date) comment)))
    (when-let* ((author (assoc "Author" mailinfo))
                (email (assoc "Email" mailinfo)))
      (setq comment (format "Author: %s <%s>\n%s"
                            (cdr author) (cdr email) comment)))
    (vc-git--checkin comment nil (cdr (assq 'patch mailinfo)))))

(defun vc-git-checkin (files comment &optional _rev)
  "Git-specific version of `vc-BACKEND-checkin'.
REV is ignored."
  (vc-git--checkin comment files nil))

(defun vc-git--stash-staged-changes (files)
  "Stash only the staged changes to FILES."
  ;; This is necessary because even if you pass a list of file names
  ;; to 'git stash push', it will stash any and all staged changes.
  (unless (zerop
           (vc-git-command nil t files "diff" "--cached" "--quiet"))
    (cl-flet
        ((git-string (&rest args)
           (string-trim-right
            (with-output-to-string
              (apply #'vc-git-command standard-output 0 nil args)))))
      (let ((cached (make-nearby-temp-file "git-cached"))
            (message "Previously staged changes")
            tree)
        ;; Use a temporary index to create a tree object corresponding
        ;; to the staged changes to FILES.
        (unwind-protect
            (progn
              (with-temp-file cached
                (vc-git-command t 0 files "diff" "--cached" "--"))
              (let* ((index (make-nearby-temp-file "git-index"))
                     (process-environment
                      (cons (format "GIT_INDEX_FILE=%s" index)
                            process-environment)))
                (unwind-protect
                    (progn
                      (vc-git-command nil 0 nil "read-tree" "HEAD")
                      ;; See `vc-git--with-apply-temp'
                      ;; regarding use of `file-local-name'.
                      (vc-git-command nil 0 nil "apply" "--cached"
                                      (file-local-name cached))
                      (setq tree (git-string "write-tree")))
                  (delete-file index))))
          (delete-file cached))
        ;; Prepare stash commit object, which has a special structure.
        (let* ((tree-commit (git-string "commit-tree" "-m" message
                                        "-p" "HEAD" tree))
               (stash-commit (git-string "commit-tree" "-m" message
                                         "-p" "HEAD" "-p" tree-commit
                                         tree)))
          ;; Push the new stash entry.
          (vc-git-command nil 0 nil "update-ref" "--create-reflog"
                          "-m" message "refs/stash" stash-commit)
          ;; Unstage the changes we've now stashed.
          (vc-git-command nil 0 files "reset" "--"))))))

(defun vc-git-find-revision (file rev buffer)
  (let* (process-file-side-effects
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (fullname
	  (let ((fn (vc-git--run-command-string
		     file "ls-files" "-z" "--full-name" "--")))
	    ;; ls-files does not return anything when looking for a
	    ;; revision of a file that has been renamed or removed.
	    (if (string= fn "")
		(file-relative-name file (vc-git-root default-directory))
	      (substring fn 0 -1)))))
    (vc-git-command buffer 0 nil "cat-file" "--filters"
                    (concat (or rev "HEAD") ":" fullname))))

(defun vc-git-find-ignore-file (file)
  "Return the git ignore file that controls FILE."
  (expand-file-name ".gitignore"
		    (vc-git-root file)))

(defun vc-git-checkout (file &optional rev)
  (vc-git-command nil 0 file "checkout" (or rev "HEAD")))

(defun vc-git-revert (file &optional contents-done)
  "Revert FILE to the version stored in the Git repository."
  (if contents-done
      (vc-git-command nil 0 file "update-index" "--")
    (vc-git-command nil 0 file "reset" "-q" "--")
    (vc-git-command nil nil file "checkout" "-q" "--")))

(defun vc-git-revert-files (files)
  "Revert FILES to the versions stored in the Git repository."
  (vc-git-command nil 0 files "reset" "-q" "--")
  (vc-git-command nil nil files "checkout" "-q" "--"))

(defvar vc-git-error-regexp-alist
  '(("^ \\(.+\\)\\> *|" 1 nil nil 0))
  "Value of `compilation-error-regexp-alist' in *vc-git* buffers.")

;; To be called via vc-pull from vc.el, which requires vc-dispatcher.
(declare-function vc-compilation-mode "vc-dispatcher" (backend))
(defvar compilation-directory)
(defvar compilation-arguments)

(defun vc-git--pushpull (command prompt extra-args)
  "Run COMMAND (a string; either push or pull) on the current Git branch.
If PROMPT is non-nil, prompt for the Git command to run."
  (require 'vc-dispatcher)
  (let* ((root (vc-git-root default-directory))
	 (buffer (format "*vc-git : %s*" (expand-file-name root)))
         (git-program vc-git-program)
         ;; TODO if pushing, prompt if no default push location - cf bzr.
         (vc-filter-command-function
          (if prompt
              (lambda (&rest args)
                (cl-destructuring-bind (&whole args git _ flags)
                    (apply #'vc-user-edit-command args)
                  (setq git-program git
                        command (car flags)
                        extra-args (cdr flags))
                  args))
            vc-filter-command-function))
         (proc (apply #'vc-do-async-command
                      buffer root git-program command extra-args)))
    (set-process-query-on-exit-flag proc t)
    ;; "git pull" includes progress output that uses ^M to move point
    ;; to the beginning of the line.  Just translate these to newlines
    ;; (but don't do anything with the CRLF sequence).
    (add-function :around (process-filter proc)
                  (lambda (filter process string)
                    (funcall filter process
                             (replace-regexp-in-string "\r\\(\\'\\|[^\n]\\)"
                                                       "\n\\1" string))))
    (with-current-buffer buffer
      (vc-run-delayed
        (vc-compilation-mode 'git)
        (setq-local compile-command
                    (concat git-program " " command " "
                            (mapconcat #'identity extra-args " ")))
        (setq-local compilation-directory root)
        ;; Either set `compilation-buffer-name-function' locally to nil
        ;; or use `compilation-arguments' to set `name-function'.
        ;; See `compilation-buffer-name'.
        (setq-local compilation-arguments
                    (list compile-command nil
                          (lambda (_name-of-mode) buffer)
                          nil))))
    (vc-set-async-update buffer)
    ;; Return the process for `vc-pull-and-push'
    proc))

(defun vc-git-pull (prompt)
  "Pull changes into the current Git branch.
Normally, this runs \"git pull\".  If PROMPT is non-nil, prompt
for the Git command to run."
  (vc-git--pushpull "pull" prompt '("--stat")))

(defun vc-git-push (prompt)
  "Push changes from the current Git branch.
Normally, this runs \"git push\".  If PROMPT is non-nil, prompt
for the Git command to run."
  (vc-git--pushpull "push" prompt nil))

(defun vc-git-merge-branch ()
  "Merge changes into the current Git branch.
This prompts for a branch to merge from."
  (let* ((root (vc-git-root default-directory))
	 (buffer (format "*vc-git : %s*" (expand-file-name root)))
	 (branches (cdr (vc-git-branches)))
	 (merge-source
	  (completing-read "Merge from branch: "
			   (if (or (member "FETCH_HEAD" branches)
				   (not (file-readable-p
                                         (vc-git--git-path "FETCH_HEAD"))))
			       branches
			     (cons "FETCH_HEAD" branches))
			   nil t)))
    (apply #'vc-do-async-command buffer root vc-git-program "merge"
	   (list merge-source))
    (with-current-buffer buffer (vc-run-delayed (vc-compilation-mode 'git)))
    (vc-set-async-update buffer)))

(defun vc-git-conflicted-files (directory)
  "Return the list of files with conflicts in DIRECTORY."
  (let* ((status
          (vc-git--run-command-string directory "status" "--porcelain" "--"))
         (lines (when status (split-string status "\n" 'omit-nulls)))
         files)
    (dolist (line lines files)
      (when (string-match "\\([ MADRCU?!][ MADRCU?!]\\) \\(.+\\)\\(?: -> \\(.+\\)\\)?"
                          line)
        (let ((state (match-string 1 line))
              (file (match-string 2 line)))
          ;; See git-status(1).
          (when (member state '("AU" "UD" "UA" ;; "DD"
                                "DU" "AA" "UU"))
            (push (expand-file-name file directory) files)))))))

(defun vc-git-repository-url (file-or-dir &optional remote-name)
  (let ((default-directory (vc-git-root file-or-dir)))
    (with-temp-buffer
      ;; The "get-url" subcommand of "git remote" was new in git 2.7.0;
      ;; "git config" also works in older versions.  -- rgr, 15-Aug-23.
      (let ((opt-name (concat "remote." (or remote-name "origin") ".url")))
	(vc-git-command (current-buffer) 0 (list "config" "--get" opt-name)))
      (buffer-substring-no-properties (point-min) (1- (point-max))))))

;; Everywhere but here, follows vc-git-command, which uses vc-do-command
;; from vc-dispatcher.
(autoload 'vc-resynch-buffer "vc-dispatcher")

(defun vc-git-resolve-when-done ()
  "Call \"git add\" if the conflict markers have been removed."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^<<<<<<< " nil t)
      (vc-git-command nil 0 buffer-file-name "add")
      (unless (or
               (not (eq vc-git-resolve-conflicts 'unstage-maybe))
               ;; Doing a merge or rebase-like operation, so bug#20292
               ;; doesn't apply.
               ;;
               ;; If we were to 'git reset' in the middle of a
               ;; cherry-pick, for example, it would effectively abort
               ;; the cherry-pick, losing the user's progress.
               (cl-intersection '(merge rebase am revert cherry-pick)
                                (vc-git--cmds-in-progress))
               (vc-git-conflicted-files (vc-git-root buffer-file-name)))
        (vc-git-command nil 0 nil "reset"))
      (vc-resynch-buffer buffer-file-name t t)
      ;; Remove the hook so that it is not called multiple times.
      (remove-hook 'after-save-hook #'vc-git-resolve-when-done t))))

(defun vc-git-find-file-hook ()
  "Activate `smerge-mode' if there is a conflict."
  (when (and buffer-file-name
             (eq (vc-state buffer-file-name 'Git) 'conflict)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "^<<<<<<< " nil 'noerror)))
    (smerge-start-session)
    (unless (or (null vc-git-resolve-conflicts)
                (and (eq vc-git-resolve-conflicts 'default)
                     (not vc-resolve-conflicts)))
      (add-hook 'after-save-hook #'vc-git-resolve-when-done nil 'local))
    (vc-message-unresolved-conflicts buffer-file-name)))

(defun vc-git-clone (remote directory rev)
  "Attempt to clone REMOTE repository into DIRECTORY at revision REV."
  (cond
   ((null rev)
    (vc-git--out-ok "clone" remote directory))
   ((ignore-errors
      (vc-git--out-ok "clone" "--branch" rev remote directory)))
   ((vc-git--out-ok "clone" remote directory)
    (let ((default-directory directory))
      (vc-git--out-ok "checkout" rev)))
   ((error "Failed to check out %s at %s" remote rev)))
  directory)

;;; HISTORY FUNCTIONS

(autoload 'vc-setup-buffer "vc-dispatcher")

;; It's a weird option due to how Git handles '--follow', and it can
;; hide certain (usually merge) commits in the `vc-print-log' buffers.
;;
;; (setq vc-git-log-switches '("-m")) can fix that, but at the cost of
;; duplicating many merge commits in the log.
;;
;; Long explanation here:
;; https://stackoverflow.com/questions/46487476/git-log-follow-graph-skips-commits
(defcustom vc-git-print-log-follow nil
  "If non-nil, use the flag `--follow' when producing single file logs.

A non-nil value will make the printed log automatically follow
the file renames.  The downsides is that the log produced this
way may omit certain (merge) commits, and that `log-view-diff'
fails on commits that used the previous name, in that log buffer.

When this variable is nil, and the log ends with a rename, there is a
button which you can press to show the log for the file name before the
rename."
  :type 'boolean
  :version "26.1")

(defun vc-git-print-log (files buffer &optional shortlog start-revision limit)
  "Print commit log associated with FILES into specified BUFFER.
If SHORTLOG is non-nil, use a short format based on `vc-git-root-log-format'.
\(This requires at least Git version 1.5.6, for the --graph option.)
If START-REVISION is non-nil, it is the newest revision to show.
If LIMIT is a number, show no more than this many entries.
If LIMIT is a non-empty string, use it as a base revision."
  (let ((coding-system-for-read
         (or coding-system-for-read vc-git-log-output-coding-system)))
    ;; `vc-do-command' creates the buffer, but we need it before running
    ;; the command.
    (vc-setup-buffer buffer)
    ;; If the buffer exists from a previous invocation it might be
    ;; read-only.
    (let ((inhibit-read-only t)
          ;; In some parts of Git's revision and revision range
          ;; notation, an empty string is equivalent to "HEAD", but not
          ;; everywhere.  For simplicity we'll always be explicit.
          (start-revision (if (member start-revision '(nil ""))
                              "HEAD"
                            start-revision))
          ;; An empty string LIMIT doesn't make sense given the
          ;; specification of this VC backend function, and is tricky to
          ;; deal with in combination with Git's double-dot notation for
          ;; specifying revision ranges.  So discard it right away.
          (limit (and (not (equal limit ""))
                      limit)))
      (with-current-buffer buffer
	(apply #'vc-git-command buffer
	       'async files
	       (append
		'("log" "--no-color")
                (when (and vc-git-print-log-follow
                           (null (cdr files))
                           (car files)
                           (not (file-directory-p (car files))))
                  ;; "--follow" on directories or multiple files is broken
                  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=8756
                  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=16422
                  (list "--follow"))
		(when shortlog
		  `("--graph" "--decorate" "--date=short"
                    ,(format "--pretty=tformat:%s"
                             (car vc-git-root-log-format))
                    "--abbrev-commit"))
                (ensure-list
                 (if shortlog vc-git-shortlog-switches vc-git-log-switches))
                (when (numberp limit)
                  (list "-n" (format "%s" limit)))
                (when (eq vc-log-view-type 'with-diff)
                  (list "-p"))
                (list (concat (and (stringp limit)
                                   (concat limit ".."))
                              start-revision))
		'("--")))))))

(defun vc-git-incoming-revision (&optional upstream-location refresh)
  (let ((rev (or upstream-location "@{upstream}")))
    (when (and (or refresh (null (vc-git--rev-parse rev)))
               ;; If the branch has no upstream, and we weren't supplied
               ;; with one, then fetching is always useless (bug#79952).
               (or upstream-location
                   (and-let* ((branch (vc-git-working-branch)))
                     (with-temp-buffer
                       (vc-git--out-ok "config" "--get"
                                       (format "branch.%s.remote"
                                               branch))))))
      (vc-git-command nil 0 nil "fetch"
                      (and upstream-location
                           ;; Extract remote from "remote/branch".
                           (replace-regexp-in-string "/.*" ""
                                                     upstream-location))))
    (ignore-errors            ; in order to return nil if no such branch
      (with-output-to-string
        (vc-git-command standard-output 0 nil
                        "log" "--max-count=1" "--pretty=format:%H" rev)))))

(defun vc-git-log-search (buffer pattern)
  "Search the log of changes for PATTERN and output results into BUFFER.

PATTERN is a basic regular expression by default in Git.

Display all entries that match log messages in long format.
With a prefix argument, ask for a command to run that will output
log entries."
  (let ((args `("log" "--no-color" "-i"
                ,@(ensure-list vc-git-log-switches)
                ,(format "--grep=%s" (or pattern "")))))
    (when current-prefix-arg
      (setq args (cdr (split-string
		       (read-shell-command
                        "Search log with command: "
                        (format "%s %s" vc-git-program
                                (mapconcat #'identity args " "))
                        'vc-git-history)
		       " " t))))
    (vc-setup-buffer buffer)
    (apply #'vc-git-command buffer 'async nil args)))

(defun vc-git-mergebase (rev1 &optional rev2)
  (unless rev2 (setq rev2 "HEAD"))
  (let ((base (vc-git--run-command-string nil "merge-base" rev1 rev2)))
    (if base
        (string-trim-right base)
      (error "No common ancestor for merge base"))))

(defvar log-view-message-re)
(defvar log-view-file-re)
(defvar log-view-font-lock-keywords)
(defvar log-view-per-file-logs)
(defvar log-view-expanded-log-entry-function)

(define-derived-mode vc-git-log-view-mode log-view-mode "Git-Log-View"
  (require 'add-log) ;; We need the faces add-log.
  ;; Don't have file markers, so use impossible regexp.
  (setq-local log-view-file-re regexp-unmatchable)
  (setq-local log-view-per-file-logs nil)
  (setq-local log-view-message-re
              (if (not (memq vc-log-view-type '(long log-search with-diff)))
                  (cadr vc-git-root-log-format)
                "^commit +\\([0-9a-z]+\\)"))
  ;; Allow expanding short log entries.
  (when (memq vc-log-view-type '(short log-outgoing log-incoming mergebase))
    (setq truncate-lines t)
    (setq-local log-view-expanded-log-entry-function
                'vc-git-expanded-log-entry))
  (setq-local log-view-font-lock-keywords
       (if (not (memq vc-log-view-type '(long log-search with-diff)))
	   (list (cons (nth 1 vc-git-root-log-format)
		       (nth 2 vc-git-root-log-format)))
	 (append
	  `((,log-view-message-re (1 'change-log-acknowledgment)))
	  ;; Handle the case:
	  ;; user: foo@bar
	  '(("^\\(?:Author\\|Commit\\):[ \t]+\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)"
	     (1 'change-log-email))
	    ;; Handle the case:
	    ;; user: FirstName LastName <foo@bar>
	    ("^\\(?:Author\\|Commit\\):[ \t]+\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]"
	     (1 'change-log-name)
	     (2 'change-log-email))
	    ("^ +\\(?:\\(?:[Aa]cked\\|[Ss]igned-[Oo]ff\\)-[Bb]y:\\)[ \t]+\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)"
	     (1 'change-log-name))
	    ("^ +\\(?:\\(?:[Aa]cked\\|[Ss]igned-[Oo]ff\\)-[Bb]y:\\)[ \t]+\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]"
	     (1 'change-log-name)
	     (2 'change-log-email))
	    ("^Merge: \\([0-9a-z]+\\) \\([0-9a-z]+\\)"
	     (1 'change-log-acknowledgment)
	     (2 'change-log-acknowledgment))
	    ("^\\(?:Date:   \\|AuthorDate: \\|CommitDate: \\)\\(.+\\)" (1 'change-log-date))
	    ("^summary:[ \t]+\\(.+\\)" (1 'log-view-message)))))))


(defun vc-git-show-log-entry (revision)
  "Move to the log entry for REVISION.
REVISION may have the form BRANCH, BRANCH~N,
or BRANCH^ (where \"^\" can be repeated)."
  (goto-char (point-min))
  (prog1
      (when revision
        (search-forward
         (format "\ncommit %s" revision) nil t
         (cond ((string-match "~\\([0-9]\\)\\'" revision)
                (1+ (string-to-number (match-string 1 revision))))
               ((string-match "\\^+\\'" revision)
                (1+ (length (match-string 0 revision))))
               (t nil))))
    (beginning-of-line)))

(defun vc-git-expanded-log-entry (revision)
  (with-temp-buffer
    (apply #'vc-git-command t nil nil
           `("log"
             ,revision
             "-1"  "--no-color" ,@(ensure-list vc-git-log-switches)
             "--"))
    (goto-char (point-min))
    (unless (eobp)
      ;; Indent the expanded log entry.
      (while (re-search-forward "^  " nil t)
        (replace-match "")
        (forward-line))
      (buffer-string))))

(defun vc-git-region-history (file buffer lfrom lto)
  "Insert into BUFFER the history of FILE for lines LFROM to LTO.
This requires git 1.8.4 or later, for the \"-L\" option of \"git log\"."
  ;; The "git log" command below interprets the line numbers as applying
  ;; to the HEAD version of the file, not to the current state of the file.
  ;; So we need to look at all the local changes and adjust lfrom/lto
  ;; accordingly.
  ;; FIXME: Maybe this should be done in vc.el (i.e. for other backends),
  ;; but since Git is one of the two backends that support this operation
  ;; so far, it's hard to tell; hg doesn't need this.
  (with-temp-buffer
    (vc-call-backend 'Git 'diff (list file) "HEAD" nil (current-buffer))
    (goto-char (point-min))
    (let ((last-offset 0)
          (from-offset nil)
          (to-offset nil))
      (while (re-search-forward
              "^@@ -\\([0-9]+\\),\\([0-9]+\\) \\+\\([0-9]+\\),\\([0-9]+\\) @@" nil t)
        (let ((headno (string-to-number (match-string 1)))
              (headcnt (string-to-number (match-string 2)))
              (curno (string-to-number (match-string 3)))
              (curcnt (string-to-number (match-string 4))))
          (cl-assert (equal (- curno headno) last-offset))
          (and (null from-offset) (> curno lfrom)
               (setq from-offset last-offset))
          (and (null to-offset) (> curno lto)
               (setq to-offset last-offset))
          (setq last-offset
                (- (+ curno curcnt) (+ headno headcnt)))))
      (setq lto (- lto (or to-offset last-offset)))
      (setq lfrom (- lfrom (or to-offset last-offset)))))
  (vc-git-command buffer 'async nil "log" "-p" ;"--follow" ;FIXME: not supported?
                  (format "-L%d,%d:%s" lfrom lto (file-relative-name file))))

(require 'diff-mode)

(defvar vc-git-region-history-mode-map
  (let ((map (make-composed-keymap
              nil (make-composed-keymap
                   (list diff-mode-map vc-git-log-view-mode-map)))))
    map))

(defvar vc-git--log-view-long-font-lock-keywords nil)
(defvar vc-git-region-history-font-lock-keywords
  '((vc-git-region-history-font-lock)))

(defun vc-git-region-history-font-lock (limit)
  (let ((in-diff (save-excursion
                   (beginning-of-line)
                   (or (looking-at "^\\(?:diff\\|commit\\)\\>")
                       (re-search-backward "^\\(?:diff\\|commit\\)\\>" nil t))
                   (eq ?d (char-after (match-beginning 0))))))
    (while
        (let ((end (save-excursion
                     (if (re-search-forward "\n\\(diff\\|commit\\)\\>"
                                            limit t)
                         (match-beginning 1)
                       limit))))
          (let ((font-lock-keywords (if in-diff diff-font-lock-keywords
                                      vc-git--log-view-long-font-lock-keywords)))
            (font-lock-fontify-keywords-region (point) end))
          (goto-char end)
          (prog1 (< (point) limit)
            (setq in-diff (eq ?d (char-after))))))
    nil))

(define-derived-mode vc-git-region-history-mode
    vc-git-log-view-mode "Git-Region-History"
  "Major mode to browse Git's \"log -p\" output."
  (setq-local vc-git--log-view-long-font-lock-keywords
              log-view-font-lock-keywords)
  (setq-local font-lock-defaults
              (cons 'vc-git-region-history-font-lock-keywords
                    (cdr font-lock-defaults))))

(defun vc-git--asciify-coding-system ()
  ;; Try to reconcile the content encoding with the encoding of Git's
  ;; auxiliary output (which is ASCII or ASCII-compatible), bug#23595.
  (unless (let ((samp "Binary files differ"))
            (string-equal samp (decode-coding-string
                                samp coding-system-for-read t)))
    (setq coding-system-for-read 'undecided)))

(defconst vc-git--empty-tree "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
  "Git object ID of the empty tree object.")

(defun vc-git-diff (files &optional rev1 rev2 buffer async)
  "Get a difference report using Git between two revisions of FILES."
  (let (process-file-side-effects
        (command "diff-tree"))
    (vc-git--asciify-coding-system)
    (if rev2
        (unless rev1 (setq rev1 vc-git--empty-tree))
      (setq command "diff-index")
      (unless rev1
        ;; If there aren't any commits yet then there is no HEAD.
        ;; So diff against the empty tree object.
        (setq rev1 (if (vc-git--empty-db-p) vc-git--empty-tree "HEAD"))))
    (if vc-git-diff-switches
        (apply #'vc-git-command (or buffer "*vc-diff*")
               (if async 'async 1)
               files
               command
               "--exit-code"
               (append (vc-switches 'git 'diff)
                       (list "-p" (or rev1 "HEAD") rev2 "--")))
      (vc-git-command (or buffer "*vc-diff*") 1 files
                      "difftool" "--exit-code" "--no-prompt" "-x"
                      (concat "diff "
                              (mapconcat #'identity
                                         (vc-switches nil 'diff) " "))
                      rev1 rev2 "--"))))

(defun vc-git-revision-table (_files)
  ;; What about `files'?!?  --Stef
  (let (process-file-side-effects
	(table (list "HEAD")))
    (with-temp-buffer
      (vc-git-command t nil nil "for-each-ref" "--format=%(refname)")
      (goto-char (point-min))
      (let ((regexp (if vc-git-revision-complete-only-branches
                        "^refs/\\(heads\\|remotes\\)/\\(.*\\)$"
                      "^refs/\\(heads\\|tags\\|remotes\\)/\\(.*\\)$")))
        (while (re-search-forward regexp nil t)
          (push (match-string 2) table))))
    (nreverse table)))

(defun vc-git-revision-completion-table (files)
  (letrec ((table (lazy-completion-table
                   table (lambda () (vc-git-revision-table files)))))
    table))

(defun vc-git-annotate-command (file buf &optional rev)
  (vc-git--asciify-coding-system)
  (let ((name (file-relative-name file)))
    (apply #'vc-git-command buf 'async nil "blame" "--date=short"
	   (append (vc-switches 'git 'annotate)
		   (list rev "--" name)))))

(declare-function vc-annotate-convert-time "vc-annotate" (&optional time))

(autoload 'decoded-time-set-defaults "time-date")
(autoload 'iso8601-parse "iso8601")

(defun vc-git-annotate-time ()
  (and (re-search-forward "^[0-9a-f^]+[^()]+(.*?\\([0-9]+-[0-9]+-[0-9]+\\)\\(?: \\([0-9]+:[0-9]+:[0-9]+\\) \\([-+0-9]+\\)\\)? +[0-9]+) " nil t)
       (let* ((dt (match-string 1))
              (dt (if (not (match-beginning 2)) dt
                    ;; Format as ISO 8601.
                    (concat dt "T" (match-string 2) (match-string 3))))
              (decoded (ignore-errors (iso8601-parse dt))))
         (and decoded
              (vc-annotate-convert-time
               (encode-time (decoded-time-set-defaults decoded)))))))

(defun vc-git-annotate-extract-revision-at-line ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at "\\^?\\([0-9a-f]+\\) \\(\\([^(]+\\) \\)?")
      (let ((revision (match-string-no-properties 1)))
	(if (match-beginning 2)
	    (let ((fname (match-string-no-properties 3)))
	      ;; Remove trailing whitespace from the file name.
	      (when (string-match " +\\'" fname)
		(setq fname (substring fname 0 (match-beginning 0))))
	      (cons revision
		    (expand-file-name fname (vc-git-root default-directory))))
	  revision)))))

(defun vc-git-last-change (file line)
  (vc-buffer-sync)
  (let ((file (file-relative-name file (vc-git-root (buffer-file-name)))))
    (with-temp-buffer
      (when (vc-git--out-ok
             "blame" "--porcelain"
             (format "-L%d,+1" line)
             file)
        (goto-char (point-min))
        (save-match-data
          (when (looking-at "\\`\\([[:alnum:]]+\\)[[:space:]]+")
            (match-string 1)))))))

;;; TAG/BRANCH SYSTEM

(declare-function vc-read-revision "vc"
                  (prompt &optional files backend default initial-input))

(defun vc-git--read-start-point (&optional dir)
  (let ((branch (car (vc-git-branches))))
    (vc-read-revision (format-prompt "Start point" branch)
                      (list (or dir (vc-git-root default-directory)))
                      'Git branch)))

(defun vc-git-create-tag (dir name branchp)
  (let ((default-directory dir)
        (start-point (and branchp (vc-git--read-start-point dir))))
    (and (or (zerop (vc-git-command nil t nil "update-index" "--refresh"))
             (y-or-n-p "Modified files exist.  Proceed? ")
             (user-error (format "Can't create %s with modified files"
                                 (if branchp "branch" "tag"))))
         (if branchp
             (vc-git-command nil 0 nil "checkout" "-b" name
                             (when (and start-point
                                        (not (equal start-point "")))
                               start-point))
           (vc-git-command nil 0 nil "tag" name)))))

(defun vc-git-retrieve-tag (dir name _update)
  (let ((default-directory dir))
    (vc-git-command nil 0 nil "checkout" name)))


;;; MISCELLANEOUS

(defsubst vc-git--maybe-abbrev ()
  (if vc-use-short-revision "--abbrev-commit" "--no-abbrev-commit"))

(defun vc-git-previous-revision (file rev)
  "Git-specific version of `vc-previous-revision'."
  (cond ((string-match "\\`HEAD\\(\\^*\\)\\'" rev)
         (format "HEAD~%d" (1+ (length (match-string 1 rev)))))
        ((string-match "\\`HEAD~\\([0-9]+\\)\\'" rev)
         (format "HEAD~%d" (1+ (string-to-number (match-string 1 rev)))))
        (file
         (let* ((fname (file-relative-name file))
                (prev-rev (with-temp-buffer
                            (and
                             (vc-git--out-ok "rev-list"
                                             (vc-git--maybe-abbrev)
                                             "-2" rev "--" fname)
                             (goto-char (point-max))
                             (bolp)
                             (zerop (forward-line -1))
                             (not (bobp))
                             (buffer-substring-no-properties
                              (point)
                              (1- (point-max)))))))
           (or (vc-git-symbolic-commit prev-rev) prev-rev)))
        (t
         ;; We used to use "^" here, but that fails on MS-Windows if git
         ;; is invoked via a batch file, in which case cmd.exe strips
         ;; the "^" because it is a special character for cmd which
         ;; process-file does not (and cannot) quote.
         (vc-git--rev-parse (concat rev "~1")))))

(defun vc-git--rev-parse (rev)
  (with-temp-buffer
    (and
     (apply #'vc-git--out-ok "rev-parse"
            (append (when vc-use-short-revision '("--short"))
                    (list rev)))
     (goto-char (point-min))
     (buffer-substring-no-properties (point) (pos-eol)))))

(defun vc-git-next-revision (file rev)
  "Git-specific version of `vc-next-revision'."
  (let* ((default-directory (vc-git-root file))
         (file (file-relative-name file))
         (current-rev
          (with-temp-buffer
            (and
             (vc-git--out-ok "rev-list"
                             (vc-git--maybe-abbrev)
                             "-1" rev "--" file)
             (goto-char (point-max))
             (bolp)
             (zerop (forward-line -1))
             (bobp)
             (buffer-substring-no-properties
              (point)
              (1- (point-max))))))
         (next-rev
          (and current-rev
               (with-temp-buffer
                 (and
                  (vc-git--out-ok "rev-list"
                                  (vc-git--maybe-abbrev)
                                  "HEAD" "--" file)
                  (goto-char (point-min))
                  (search-forward current-rev nil t)
                  (zerop (forward-line -1))
                  (buffer-substring-no-properties
                   (point)
                   (progn (forward-line 1) (1- (point)))))))))
    (or (vc-git-symbolic-commit next-rev) next-rev)))

(defun vc-git-file-name-changes (rev)
  (with-temp-buffer
    (let ((root (vc-git-root default-directory)))
      (unless vc-git-print-log-follow
        (apply #'vc-git-command (current-buffer) t nil
               "diff"
               "--name-status"
               "--diff-filter=ADCR"
               (concat rev "^") rev
               (vc-switches 'git 'file-name-changes)))
      (let (res)
        (goto-char (point-min))
        (while (re-search-forward "^\\([ADCR]\\)[0-9]*\t\\([^\n\t]+\\)\\(?:\t\\([^\n\t]+\\)\\)?" nil t)
          (pcase (match-string 1)
            ("A" (push (cons nil (match-string 2)) res))
            ("D" (push (cons (match-string 2) nil) res))
            ((or "C" "R") (push (cons (match-string 2) (match-string 3)) res))
            ;; ("M" (push (cons (match-string 1) (match-string 1)) res))
            ))
        (mapc (lambda (c)
                (if (car c) (setcar c (expand-file-name (car c) root)))
                (if (cdr c) (setcdr c (expand-file-name (cdr c) root))))
              res)
        (nreverse res)))))

(defun vc-git-delete-file (file)
  (vc-git-command nil 0 file "rm" "-f" "--"))

(defun vc-git-rename-file (old new)
  (vc-git-command nil 0 (list old new) "mv" "-f" "--"))

(defun vc-git-mark-resolved (files)
  (vc-git-command nil 0 files "add"))

(defun vc-git-get-change-comment (_files rev)
  (with-output-to-string
    (vc-git-command standard-output 1 nil
                    "log" "--max-count=1" "--pretty=format:%B" rev)))

(defun vc-git-cherry-pick-comment (_files rev reverse)
  ;; Don't just call `vc-git-get-change-comment' in order to make one
  ;; fewer call out to Git.  We have to resolve REV because even if it's
  ;; already a hash it may be an abbreviated one.
  (let (comment)
    (with-temp-buffer
      (vc-git-command t 0 nil "log" "-n1" "--pretty=format:%H%n%B" rev)
      (goto-char (point-min))
      (setq rev (buffer-substring-no-properties (point) (pos-eol)))
      (forward-line 1)
      (setq comment (buffer-substring-no-properties (point) (point-max))))
    (if reverse
        (format "Summary: Revert \"%s\"\n\nThis reverts commit %s.\n"
                (car (split-string comment "\n" t
                                   split-string-default-separators))
                rev)
      (format "Summary: %s\n(cherry picked from commit %s)\n"
              comment rev))))

(defun vc-git--assert-revision-on-branch (rev branch)
  "Signal an error unless REV is on BRANCH."
  ;; 'git branch --contains' is a porcelain command whose output could
  ;; change in the future.
  (unless (zerop (vc-git-command nil 1 nil "merge-base"
                                 "--is-ancestor" rev branch))
    (error "Revision %s does not exist on branch %s" rev branch)))

(defun vc-git-revision-published-p (rev)
  "Whether we think REV has been pushed such that it is public history.
Considers only the current branch.  Does not fetch."
  (let ((branch (vc-git-working-branch))
        (rev (vc-git--rev-parse rev)))
    (vc-git--assert-revision-on-branch rev branch)
    (and
     ;; BRANCH has an upstream.
     (with-temp-buffer
       (vc-git--out-ok "config" "--get"
                       (format "branch.%s.merge" branch)))
     ;; REV is not outgoing.
     (not (cl-member rev
                     (split-string
                      (with-output-to-string
                        (vc-git-command standard-output 0 nil "log"
                                        "--pretty=format:%H"
                                        "@{upstream}..HEAD")))
                     :test #'string-prefix-p)))))

(defun vc-git--assert-allowed-rewrite (rev)
  (when (and (not (and vc-allow-rewriting-published-history
                       (not (eq vc-allow-rewriting-published-history 'ask))))
             (vc-git-revision-published-p rev)
             (not (and (eq vc-allow-rewriting-published-history 'ask)
                       (yes-or-no-p
                        (format "\
Commit %s appears published; allow rewriting history?"
                                rev)))))
    (user-error "Will not rewrite likely-public history; \
see option `vc-allow-rewriting-published-history'")))

(defun vc-git-modify-change-comment (files rev comment)
  (vc-git--assert-allowed-rewrite rev)
  (when (zerop (vc-git--call nil nil "rev-parse" (format "%s^2" rev)))
    ;; This amend! approach doesn't work for merge commits.
    ;; Error out now instead of leaving an amend! commit hanging.
    (error "Cannot modify merge commit comments"))
  (let* ((args (delete "--amend"
                       (vc-git--log-edit-extract-headers comment)))
         (message (format "amend! %s\n\n%s" rev (pop args)))
         (msg-file
          ;; On MS-Windows, pass the message through a file, to work
          ;; around how command line arguments must be in the system
          ;; codepage, and therefore might not support non-ASCII.
          (and (eq system-type 'windows-nt)
               (let ((default-directory
                      (or (file-name-directory (or (car files)
                                                   default-directory))
                          default-directory)))
                 (make-nearby-temp-file "git-msg")))))
    (unwind-protect
        (progn
          (when (cl-intersection '("--author" "--date") args
                                 :test #'string=)
            ;; 'git rebase --autosquash' cannot alter authorship.
            ;; See the description of --fixup in git-commit(1).
            (error
"Author: and Date: not supported when modifying existing commits"))

          ;; Check that a rebase with --autosquash won't make changes
          ;; other than to REV's change comment.  With the prompt here
          ;; it's okay to assume the user knows what --autosquash is
          ;; because they've made some squash!/fixup!/amend! commits.
          (when
              (and (split-string
                    (with-output-to-string
                      (vc-git-command standard-output 0 nil
                                      "log" "--oneline" "-E"
                                      "--grep" "^(squash|fixup|amend)! "
                                      (format "%s~1.." rev))))
                   (not (yes-or-no-p "\
Rebase may --autosquash your other squash!/fixup!/amend!; proceed?")))
            (user-error "Aborted"))

          (when msg-file
            (let ((coding-system-for-write
                   (or coding-system-for-write
                       vc-git-commits-coding-system)))
              (write-region message nil msg-file)))
          ;; Regardless of the state of the index and working tree, this
          ;; will always create an empty commit, thanks to --only.
          (let ((coding-system-for-write
                 ;; On MS-Windows, we must encode command-line arguments in
                 ;; the system codepage.
                 (if (eq system-type 'windows-nt)
                     locale-coding-system
                   coding-system-for-write)))
            (apply #'vc-git-command nil 0 nil
                   "commit" "--only" "--allow-empty"
                   (nconc (if msg-file
                              (list "-F" (file-local-name msg-file))
                            (list "-m" message))
                          args))))
      (when (and msg-file (file-exists-p msg-file))
        (delete-file msg-file))))
  (with-environment-variables (("GIT_SEQUENCE_EDITOR" "true"))
    (vc-git-command nil 0 nil "rebase" "--autostash" "--autosquash" "-i"
                    (format "%s~1" rev))))

(defun vc-git-delete-revision (rev)
  "Rebase current branch to remove REV."
  (vc-git--assert-revision-on-branch rev (vc-git-working-branch))
  (with-temp-buffer
    (vc-git-command t 0 nil "log" "--merges" (format "%s~1.." rev))
    (unless (bobp)
      (error "There have been merges since %s; cannot delete revision"
             rev)))
  (unless (zerop (vc-git-command nil 1 nil "rebase"
                                 rev "--onto" (format "%s~1" rev)))
    ;; FIXME: Ideally we would leave some sort of conflict for the user
    ;; to resolve, instead of just giving up.  We would want C-x v v to
    ;; do 'git rebase --continue' like how it can currently be used to
    ;; conclude a merge after resolving conflicts.
    (vc-git-command nil 0 nil "rebase" "--abort")
    (error "Merge conflicts while trying to delete %s; aborting" rev)))

(defun vc-git-delete-revisions-from-end (rev)
  "Hard reset back to REV.
It is an error if REV is not on the current branch."
  (vc-git--assert-revision-on-branch rev (vc-git-working-branch))
  (vc-git-command nil 0 nil "reset" "--hard" rev))

(defun vc-git-uncommit-revisions-from-end (rev)
  "Mixed reset back to REV.
It is an error if REV is not on the current branch."
  (vc-git--assert-revision-on-branch rev (vc-git-working-branch))
  (vc-git-command nil 0 nil "reset" "--mixed" rev))

(defvar vc-git-extra-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [git-grep]
      '(menu-item "Git grep..." vc-git-grep
		  :help "Run the `git grep' command"))
    (define-key map [git-ds]
      '(menu-item "Delete Stash..." vc-git-stash-delete
                  :help "Delete a stash"))
    (define-key map [git-sn]
      '(menu-item "Stash a Snapshot" vc-git-stash-snapshot
		  :help "Stash the current state of the tree and keep the current state"))
    (define-key map [git-st]
      '(menu-item "Create Stash..." vc-git-stash
		  :help "Stash away changes"))
    (define-key map [git-ss]
      '(menu-item "Show Stash..." vc-git-stash-show
		  :help "Show stash contents"))
    map))

(defun vc-git-extra-menu () vc-git-extra-menu-map)

(defun vc-git-extra-status-menu () vc-git-extra-menu-map)

(defun vc-git-root (file)
  (vc-find-root file ".git"))

(defun vc-git-prepare-patch (rev)
  (with-current-buffer (generate-new-buffer " *vc-git-prepare-patch*")
    (vc-git-command t 0 nil "format-patch"
                    "--no-numbered" "--stdout" "-n1" rev)
    (condition-case _
        (let (subject body-start patch-start patch-end)
          (goto-char (point-min))
          (re-search-forward "^Subject: \\(.*\\)")
          (setq subject (match-string 1))
          (while (progn (forward-line 1)
                        (looking-at "[\s\t]\\(.*\\)"))
            (setq subject (format "%s %s" subject (match-string 1))))
          (goto-char (point-min))
          (re-search-forward "\n\n")
          (setq body-start (point))
          (if ;; If the user has added any of these to
              ;; `vc-git-diff-switches' then they expect to see the
              ;; diffstat in *vc-diff* buffers.
              (cl-intersection '("--stat"
                                 "--patch-with-stat"
                                 "--compact-summary")
                               (vc-switches 'git 'diff)
                               :test #'equal)
              (progn (re-search-forward "^---$")
                     (setq patch-start (pos-bol 2)))
            (re-search-forward "^diff --git a/")
            (setq patch-start (pos-bol)))
          (re-search-forward "^-- $")
          (setq patch-end (pos-bol))
          (list :subject subject
                :body-start body-start
                :patch-start patch-start
                :patch-end patch-end
                :buffer (current-buffer)))
      (search-failed (error "git-format-patch output parse failure")))))

;; grep-compute-defaults autoloads grep.
(declare-function grep-read-regexp "grep" ())
(declare-function grep-read-files "grep" (regexp))
(declare-function grep-expand-template "grep"
                 (template &optional regexp files dir excl more-opts))
(defvar compilation-environment)

;; Derived from `lgrep'.
;;;###autoload
(defun vc-git-grep (regexp &optional files dir)
  "Run git grep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.  As whitespace triggers
completion when entering a pattern, including it requires
quoting, e.g. `\\[quoted-insert]<space>'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-command'.

Collect output in a buffer.  While git grep runs asynchronously, you
can use \\[next-error] (`next-error'), or \\<grep-mode-map>\
\\[compile-goto-error] in the grep output buffer,
to go to the lines where grep found matches.

This command shares argument histories with \\[rgrep] and \\[grep]."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep"
				   nil nil 'grep-history)))
      (t (let* ((regexp (grep-read-regexp))
		(files
                 (mapconcat #'shell-quote-argument
                            (split-string (grep-read-files regexp)) " "))
		(dir (read-directory-name "In directory: "
					  nil default-directory t)))
	   (list regexp files dir))))))
  (require 'grep)
  (when (and (stringp regexp) (> (length regexp) 0))
    (unless (and dir (file-accessible-directory-p dir))
      (setq dir default-directory))
    (let ((command regexp))
      (if (null files)
	  (if (string= command "git grep")
	      (setq command nil))
	(setq dir (file-name-as-directory (expand-file-name dir)))
	(setq command
              (grep-expand-template vc-git-grep-template
                                    regexp files))
	(when command
	  (if (equal current-prefix-arg '(4))
	      (setq command
		    (read-from-minibuffer "Confirm: "
					  command nil nil 'grep-history))
	    (add-to-history 'grep-history command))))
      (when command
	(let ((default-directory dir)
	      (compilation-environment (cons "PAGER=" compilation-environment)))
	  ;; Setting process-setup-function makes exit-message-function work
	  ;; even when async processes aren't supported.
	  (compilation-start command 'grep-mode))
	(if (eq next-error-last-buffer (current-buffer))
	    (setq default-directory dir))))))

(declare-function vc-deduce-fileset "vc"
                  (&optional observer allow-unregistered
                             state-model-only-files))

(autoload 'vc-dir-marked-files "vc-dir")

(defun vc-git--deduce-files-for-stash ()
  ;; In *vc-dir*, if nothing is marked, act on the whole working tree
  ;; regardless of the position of point.  This preserves historical
  ;; behavior and is also probably more useful.
  (if (derived-mode-p 'vc-dir-mode)
      (vc-dir-marked-files)
    (cadr (vc-deduce-fileset))))

(defun vc-git-stash (name)
  "Create a stash named NAME.
In `vc-dir-mode', if there are files marked, stash the changes to those.
If no files are marked, stash all uncommitted changes to tracked files.
In other modes, call `vc-deduce-fileset' to determine files to stash."
  (interactive "sStash name: ")
  (let ((root (vc-git-root default-directory)))
    (when root
      (apply #'vc-git--call nil nil "stash" "push" "-m" name
             (vc-git--deduce-files-for-stash))
      (vc-resynch-buffer root t t))))

(defvar vc-git-stash-read-history nil
  "History for `vc-git-stash-read'.")

(cl-defun vc-git-stash-read (prompt &key default-most-recent)
  "Prompt the user, with PROMPT, to select a git stash.
PROMPT is passed to `format-prompt'.  If DEFAULT-MOST-RECENT is non-nil,
then the most recently pushed stash is the default selection."
  (if-let* ((stashes
             (split-string (vc-git--run-command-string nil
                                                       "stash" "list")
                           "\n" t)))
      (let* ((default (and default-most-recent (car stashes)))
             (prompt (format-prompt prompt
                                    (and default-most-recent
                                         "most recent, stash@{0}")))
             (stash (completing-read prompt stashes
                                     nil :require-match nil
                                     'vc-git-stash-read-history
                                     default)))
        (if (string-empty-p stash)
            (user-error "Not a stash")
          (string-match "^stash@{[[:digit:]]+}" stash)
          (match-string 0 stash)))
    (user-error "No stashes")))

(defun vc-git-stash-show (name)
  "Show the contents of stash NAME."
  (interactive (list (vc-git-stash-read "Show stash")))
  (vc-setup-buffer "*vc-git-stash*")
  (vc-git-command "*vc-git-stash*" 'async nil
                  "stash" "show" "--color=never" "-p" name)
  (set-buffer "*vc-git-stash*")
  (setq buffer-read-only t)
  (diff-mode)
  (pop-to-buffer (current-buffer)))

(defun vc-git-stash-apply (name)
  "Apply stash NAME."
  (interactive (list (vc-git-stash-read "Apply stash")))
  (vc-git-command "*vc-git-stash*" 0 nil "stash" "apply" "-q" name)
  (vc-resynch-buffer (vc-git-root default-directory) t t))

(defun vc-git-stash-pop (name)
  "Pop stash NAME."
  ;; Stashes are commonly popped off in reverse order, so pass non-nil
  ;; DEFAULT-MOST-RECENT to `vc-git-stash-read'.
  (interactive (list (vc-git-stash-read "Pop stash"
                                        :default-most-recent t)))
  (vc-git-command "*vc-git-stash*" 0 nil "stash" "pop" "-q" name)
  (vc-resynch-buffer (vc-git-root default-directory) t t))

(defun vc-git-stash-delete (name)
  "Delete stash NAME."
  (interactive (list (vc-git-stash-read "Delete stash")))
  (vc-git-command "*vc-git-stash*" 0 nil "stash" "drop" "-q" name)
  (vc-resynch-buffer (vc-git-root default-directory) t t))

(defun vc-git-stash-snapshot ()
  "Create a stash with the current uncommitted changes.
In `vc-dir-mode', if there are files marked, stash the changes to those.
If no files are marked, stash all uncommitted changes to tracked files.
In other modes, call `vc-deduce-fileset' to determine files to stash."
  (interactive)
  (apply #'vc-git--call nil nil "stash" "push" "-m"
	 (format-time-string "Snapshot on %Y-%m-%d at %H:%M")
         (vc-git--deduce-files-for-stash))
  (vc-git-command "*vc-git-stash*" 0 nil "stash" "apply" "-q" "stash@{0}")
  (vc-resynch-buffer (vc-git-root default-directory) t t))

(defun vc-git-stash-list ()
  (and-let* ((out (vc-git--run-command-string nil "stash" "list")))
    (split-string
     (replace-regexp-in-string
      "^stash@" "             " out)
     "\n"
     t)))

(defun vc-git-stash-get-at-point (point)
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (if (looking-at "^ +\\({[0-9]+}\\):")
        (match-string-no-properties 1)
      (error "Cannot find stash at point"))))

;; vc-git-stash-delete-at-point must be called from a vc-dir buffer.
(declare-function vc-dir-refresh "vc-dir" ())

(defun vc-git-stash-delete-at-point ()
  "Delete the stash at point."
  (interactive)
  (let ((stash (vc-git-stash-get-at-point (point))))
    (when (y-or-n-p (format "Remove stash %s ? " stash))
      (vc-git--run-command-string nil "stash" "drop" (format "stash@%s" stash))
      (vc-dir-refresh))))

(defun vc-git-stash-show-at-point ()
  "Show the stash at point."
  (interactive)
  (vc-git-stash-show (format "stash@%s" (vc-git-stash-get-at-point (point)))))

(defun vc-git-stash-apply-at-point ()
  "Apply the stash at point."
  (interactive)
  (let (vc-dir-buffers) ; Small optimization.
    (vc-git-stash-apply (format "stash@%s" (vc-git-stash-get-at-point (point)))))
  (vc-dir-refresh))

(defun vc-git-stash-pop-at-point ()
  "Pop the stash at point."
  (interactive)
  (let (vc-dir-buffers) ; Likewise.
    (vc-git-stash-pop (format "stash@%s" (vc-git-stash-get-at-point (point)))))
  (vc-dir-refresh))

(defun vc-git-stash-menu (e)
  (interactive "e")
  (vc-dir-at-event e (popup-menu vc-git-stash-menu-map e)))

(defun vc-git--worktrees ()
  "Return an alist of alists regarding this repository's worktrees.
The keys into the outer alist are the worktree root directories; so,
there is one inner alist for each worktree.  The keys and values of each
inner alist are the worktree attributes returned by `git worktree list';
see the \"LIST OUTPUT FORMAT\" section of the git-worktree(1) manual
page for the meanings of these attributes."
  (with-temp-buffer
    (vc-git-command nil 0 nil "worktree" "prune")
    (let ((have-worktree-list-porcelain-z
           ;; The -z option to 'worktree list --porcelain' appeared in 2.36
           (version<= "2.36" (vc-git--program-version))))
      (vc-git-command t 0 nil "worktree" "list" "--porcelain"
                      (and have-worktree-list-porcelain-z "-z"))
      (let (worktrees current-root current-rest)
        (goto-char (point-min))
        (while
            (re-search-forward
             (if have-worktree-list-porcelain-z
                 "\\=\\(\\([a-zA-Z]+\\)\\(?: \\([^\0]+\\)\\)?\\)?\0"
               "\\=\\(\\([a-zA-Z]+\\)\\(?: \\([^\n]+\\)\\)?\\)?\n")
             nil t)
          (if (match-string 1)
              (let ((k (intern (match-string 2)))
                    (v (or (match-string 3) t)))
                (cond ((and (not current-root) (eq k 'worktree))
                       (setq current-root (file-name-as-directory v)))
                      ((not (eq k 'worktree))
                       (push (cons k v) current-rest))
                      (t
                       (error "'git worktree' output parse error"))))
            (push (cons current-root current-rest) worktrees)
            (setq current-root nil current-rest nil)))
        (or worktrees
            (error "'git worktree' output parse error"))))))

(defun vc-git-known-other-working-trees ()
  "Implementation of `known-other-working-trees' backend function for Git."
  (cl-loop with root = (file-truename
                        (expand-file-name (vc-git-root default-directory)))
           for (worktree) in (vc-git--worktrees)
           unless (equal worktree root)
           collect (abbreviate-file-name worktree)))

(defun vc-git-add-working-tree (directory)
  "Implementation of `add-working-tree' backend function for Git."
  (letrec ((dir (expand-file-name directory))
           (vc-filter-command-function #'list) ; see `vc-read-revision'
           (revs (vc-git-revision-table nil))
           (table (lazy-completion-table table (lambda () revs)))
           (branch (completing-read (format-prompt "New or existing branch"
                                                   "latest revision, detached")
                                    table nil nil nil 'vc-revision-history))
           (args (cond ((string-empty-p branch)
                        (list "--detach" dir))
                       ((member branch revs)
                        (list dir branch))
                       (t
                        (list "-b" branch dir (vc-git--read-start-point))))))
    (apply #'vc-git-command nil 0 nil "worktree" "add" args)))

(defun vc-git-delete-working-tree (directory)
  "Implementation of `delete-working-tree' backend function for Git."
  ;; Avoid assuming we have 'git worktree remove' which older Git lacks.
  (delete-directory directory t t)
  (vc-git-command nil 0 nil "worktree" "prune"))

(defun vc-git-move-working-tree (from to)
  "Implementation of `move-working-tree' backend function for Git."
  (let ((v (vc-git--program-version)))
    (cond ((version<= "2.29" v)
           ;; 'git worktree move' can't move the main worktree,
           ;; but moving and then repairing can.
           (rename-file from (directory-file-name to) 1)
           (let ((default-directory to))
             (vc-git-command nil 0 nil "worktree" "repair")))
          ((version<= "2.17" v)
           ;; We lack 'git worktree repair' but have 'git worktree move'.
           (vc-git-command nil 0 nil "worktree" "move"
                           (expand-file-name from)
                           (expand-file-name to)))
          (t
           ;; We don't even have 'git worktree move'.
           (error "Your Git is too old to relocate other working trees")))))


;;; Internal commands

(defun vc-git-command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-git.el.
The difference to `vc-do-command' is that this function always invokes
`vc-git-program'."
  (let* ((coding-system-for-read
          (or coding-system-for-read vc-git-log-output-coding-system))
         ;; Commands which pass command line arguments which might
         ;; contain non-ASCII have to bind `coding-system-for-write' to
         ;; `locale-coding-system' when (eq system-type 'windows-nt)
         ;; because MS-Windows has the limitation that command line
         ;; arguments must be in the system codepage.  We do that only
         ;; within the commands which must do it, instead of implementing
         ;; it here, even though that means code repetition.  This is
         ;; because this let-binding has the disadvantage of overriding
         ;; any `coding-system-for-write' explicitly selected by the user
         ;; (e.g. with C-x RET c), or by enclosing function calls.  So we
         ;; want to do it only for commands which really require it.
	 (coding-system-for-write
          (or coding-system-for-write vc-git-commits-coding-system))
         (process-environment
          (append
           `("GIT_DIR"
             ,@(and vc-git-use-literal-pathspecs
                    '("GIT_LITERAL_PATHSPECS=1"))
             ;; Avoid repository locking during background operations
             ;; (bug#21559).
             ,@(and revert-buffer-in-progress
                    '("GIT_OPTIONAL_LOCKS=0")))
           process-environment))
         (file1 (and (not (cdr-safe file-or-list))
                     (or (car-safe file-or-list) file-or-list)))
         (file-list-is-rootdir (and file1
                                    (directory-name-p file1)
                                    (equal file1 (vc-git-root file1))))
         (default-directory (if file-list-is-rootdir
                                file1
                              default-directory)))
    (apply #'vc-do-command (or buffer "*vc*") okstatus vc-git-program
           ;; Three cases:
           ;; - operating on root directory and command is one where doing
           ;;   so requires passing "." to have the usual effect
           ;;   (e.g. 'git checkout --'   will do nothing;
           ;;         'git checkout -- .' will revert all files as desired)
           ;; - operating on root directory and command is one where we
           ;;   must pass no list of files to have the usual effect
           ;;   (e.g. 'git log' for root logs as discussed in bug#16897)
           ;; - not operating on root directory,
           ;;   pass FILE-OR-LIST along as normal.
           (cond ((and file-list-is-rootdir
                       (member (car flags) '("checkout")))
                  ".")
                 ((not file-list-is-rootdir)
                  file-or-list))
           (cons "--no-pager" flags))))

(defun vc-git--empty-db-p ()
  "Check if the git db is empty (no commit done yet)."
  (let (process-file-side-effects)
    (not (zerop (vc-git--call nil nil "rev-parse" "--verify" "HEAD")))))

(defun vc-git--call (infile buffer command &rest args)
  ;; We don't need to care the arguments.  If there is a file name, it
  ;; is always a relative one.  This works also for remote
  ;; directories.  We enable `inhibit-null-byte-detection', otherwise
  ;; Tramp's eol conversion might be confused.
  (let ((inhibit-null-byte-detection t)
	(coding-system-for-read
         (or coding-system-for-read vc-git-log-output-coding-system))
	(coding-system-for-write
         (or coding-system-for-write vc-git-commits-coding-system))
	(process-environment
	 (append
	  `("GIT_DIR"
            ,@(when vc-git-use-literal-pathspecs
                '("GIT_LITERAL_PATHSPECS=1"))
	    ;; Avoid repository locking during background operations
	    ;; (bug#21559).
	    ,@(when revert-buffer-in-progress
		'("GIT_OPTIONAL_LOCKS=0")))
	  process-environment)))
    (apply #'process-file vc-git-program infile buffer nil
           "--no-pager" command args)))

(defun vc-git--out-ok (command &rest args)
  "Run `git COMMAND ARGS...' and insert standard output in current buffer.
Return whether the process exited with status zero."
  (zerop (apply #'vc-git--call nil '(t nil) command args)))

(defun vc-git--out-str (command &rest args)
  "Run `git COMMAND ARGS...' and return standard output as a string.
The exit status is ignored."
  (with-output-to-string
    (with-current-buffer standard-output
      (apply #'vc-git--out-ok command args))))

(defun vc-git--out-match (args regexp group)
  "Run `git ARGS...' and return match for group number GROUP of REGEXP.
Return nil if the output does not match.  The exit status is ignored."
  (let ((out (apply #'vc-git--out-str args)))
    (when (string-match regexp out)
      (match-string group out))))

(defun vc-git--run-command-string (file &rest args)
  "Run a git command on FILE and return its output as string.
FILE can be nil."
  (let* ((ok t)
         (str (with-output-to-string
                (with-current-buffer standard-output
                  (unless (apply #'vc-git--out-ok
				 (if file
				     (append args (list (file-relative-name
							 file)))
				   args))
                    (setq ok nil))))))
    (and ok str)))

(defun vc-git-symbolic-commit (commit &optional force)
  "Translate revision string of COMMIT to a symbolic form.
If the optional argument FORCE is non-nil, the returned value is
allowed to include revision specifications like \"master~8\"
\(the 8th parent of the commit currently pointed to by the master
branch), otherwise such revision specifications are rejected, and
the function returns nil."
  (and commit
       (with-temp-buffer
         (and
          (vc-git--out-ok "name-rev" "--no-undefined" "--name-only" commit)
          (goto-char (point-min))
          (or force (not (looking-at "^.*[~^].*$" t)))
          (= (forward-line 2) 1)
          (bolp)
          (buffer-substring-no-properties (point-min)
                                          (1- (point-max)))))))

(defvar-keymap vc-dir-git-mode-map
  "z c" #'vc-git-stash
  "z s" #'vc-git-stash-snapshot
  "z p" #'vc-git-stash-pop
  "z d" #'vc-git-stash-delete)

(define-minor-mode vc-dir-git-mode
  "A minor mode for git-specific commands in `vc-dir-mode' buffers.
Also note that there are git stash commands available in the
\"Stash\" section at the head of the buffer."
  :lighter " Git")

(provide 'vc-git)

;;; vc-git.el ends here
