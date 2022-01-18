;;; vc-git.el --- VC backend for the git version control system -*- lexical-binding: t -*-

;; Copyright (C) 2006-2022 Free Software Foundation, Inc.

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
;; * checkin (files rev comment)                   OK
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
;; HISTORY FUNCTIONS
;; * print-log (files buffer &optional shortlog start-revision limit)   OK
;; * log-outgoing (buffer remote-location)         OK
;; * log-incoming (buffer remote-location)         OK
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
;; TAG SYSTEM
;; - create-tag (dir name branchp)                 OK
;; - retrieve-tag (dir name update)                OK
;; MISCELLANEOUS
;; - make-version-backups-p (file)                 NOT NEEDED
;; - previous-revision (file rev)                  OK
;; - next-revision (file rev)                      OK
;; - check-headers ()                              COULD BE SUPPORTED
;; - delete-file (file)                            OK
;; - rename-file (old new)                         OK
;; - find-file-hook ()                             OK
;; - conflicted-files                              OK
;; - repository-url (file-or-dir)                  OK

;;; Code:

(require 'cl-lib)
(require 'vc-dispatcher)
(eval-when-compile
  (require 'subr-x) ; for string-trim-right
  (require 'vc)
  (require 'vc-dir))

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
If nil, use the value of `vc-annotate-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "25.1")

(defcustom vc-git-log-switches nil
  "String or list of strings specifying switches for Git log under VC."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string))
  :version "28.1")

(defcustom vc-git-resolve-conflicts t
  "When non-nil, mark conflicted file as resolved upon saving.
That is performed after all conflict markers in it have been
removed.  If the value is `unstage-maybe', and no merge is in
progress, then after the last conflict is resolved, also clear
the staging area."
  :type '(choice (const :tag "Don't resolve" nil)
                 (const :tag "Resolve" t)
                 (const :tag "Resolve and maybe unstage all files"
                        unstage-maybe))
  :version "25.1")

(defcustom vc-git-program "git"
  "Name of the Git executable (excluding any arguments)."
  :version "24.1"
  :type 'string)

(defcustom vc-git-root-log-format
  '("%d%h..: %an %ad %s"
    ;; The first shy group matches the characters drawn by --graph.
    ;; We use numbered groups because `log-view-message-re' wants the
    ;; revision number to be group 1.
    "^\\(?:[*/\\| ]+ \\)?\\(?2: ([^)]+)\\)?\\(?1:[0-9a-z]+\\)..: \
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
  :type '(choice (const :tag "All" t)
                 (integer :tag "Limit"
                          :validate
                          (lambda (widget)
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
    (when dir
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

(defun vc-git--state-code (code)
  "Convert from a string to an added/deleted/modified state."
  (pcase (string-to-char code)
    (?M 'edited)
    (?A 'added)
    (?D 'removed)
    (?U 'edited)     ;; FIXME
    (?T 'edited)))   ;; FIXME

(defvar vc-git--program-version nil)

(defun vc-git--program-version ()
  (or vc-git--program-version
      (let ((version-string
             (vc-git--run-command-string nil "version")))
        (setq vc-git--program-version
              (if (and version-string
                       ;; Git for Windows appends ".windows.N" to the
                       ;; numerical version reported by Git.
                       (string-match
                        "git version \\([0-9.]+\\)\\(\\.windows\\.[0-9]+\\)?$"
                        version-string))
                  (match-string 1 version-string)
                "0")))))

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
       ;; I have only seen this with a file that is only present in the
       ;; index.  Let us call this `removed'.
       ("AD" 'removed)
       (_ (cond
           ((string-match-p "^[ RD]+$" code) 'removed)
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
  (let* ((rev (vc-working-revision file 'Git))
         (disp-rev (or (vc-git--symbolic-ref file)
                       (and rev (substring rev 0 7))))
         (def-ml (vc-default-mode-line-string 'Git file))
         (help-echo (get-text-property 0 'help-echo def-ml))
         (face   (get-text-property 0 'face def-ml)))
    (propertize (concat (substring def-ml 0 4) disp-rev)
                'face face
                'help-echo (concat help-echo "\nCurrent revision: " rev))))

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
       (setq next-stage (if files 'ls-files-up-to-date 'ls-files-conflict))
       (while (re-search-forward
               ":\\([0-7]\\{6\\}\\) \\([0-7]\\{6\\}\\) [0-9a-f]\\{40\\} [0-9a-f]\\{40\\} \\(\\([ADMUT]\\)\0\\([^\0]+\\)\\|\\([CR]\\)[0-9]*\0\\([^\0]+\\)\0\\([^\0]+\\)\\)\0"
               nil t 1)
         (let ((old-perm (string-to-number (match-string 1) 8))
               (new-perm (string-to-number (match-string 2) 8))
               (state (or (match-string 4) (match-string 6)))
               (name (or (match-string 5) (match-string 7)))
               (new-name (match-string 8)))
           (if new-name  ; Copy or rename.
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
              git-state name (vc-git--state-code state)
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
(declare-function vc-exec-after "vc-dispatcher" (code))
;; Follows vc-exec-after.
(declare-function vc-set-async-update "vc-dispatcher" (process-buffer))

(defun vc-git-dir-status-goto-stage (git-state)
  ;; TODO: Look into reimplementing this using `git status --porcelain=v2'.
  (let ((files (vc-git-dir-status-state->files git-state)))
    (erase-buffer)
    (pcase (vc-git-dir-status-state->stage git-state)
      ('update-index
       (if files
           (vc-git-command (current-buffer) 'async files "add" "--refresh" "--")
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
    (vc-run-delayed
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

(defvar vc-git-stash-shared-map
  (let ((map (make-sparse-keymap)))
    (define-key map "S" #'vc-git-stash-snapshot)
    (define-key map "C" #'vc-git-stash)
    map))

(defvar vc-git-stash-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map vc-git-stash-shared-map)
    ;; Turn off vc-dir marking
    (define-key map [mouse-2] #'ignore)

    (define-key map [down-mouse-3] #'vc-git-stash-menu)
    (define-key map "\C-k" #'vc-git-stash-delete-at-point)
    (define-key map "=" #'vc-git-stash-show-at-point)
    (define-key map "\C-m" #'vc-git-stash-show-at-point)
    (define-key map "A" #'vc-git-stash-apply-at-point)
    (define-key map "P" #'vc-git-stash-pop-at-point)
    map))

(defvar vc-git-stash-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map vc-git-stash-shared-map)
    (define-key map [mouse-2] #'push-button)
    (define-key map "\C-m" #'push-button)
    map))

(defconst vc-git-stash-shared-help
  "\\<vc-git-stash-shared-map>\\[vc-git-stash]: Create named stash\n\\[vc-git-stash-snapshot]: Snapshot stash")

(defconst vc-git-stash-list-help (concat "\\<vc-git-stash-map>mouse-3: Show stash menu\n\\[vc-git-stash-show-at-point], =: Show stash\n\\[vc-git-stash-apply-at-point]: Apply stash\n\\[vc-git-stash-pop-at-point]: Apply and remove stash (pop)\n\\[vc-git-stash-delete-at-point]: Delete stash\n"
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
		  :help "Snapshot stash"))
    (define-key map [cr]
      '(menu-item "Create Named Stash" vc-git-stash
		  :help "Create named stash"))
    (define-key map [de]
      '(menu-item "Delete Stash" vc-git-stash-delete-at-point
		  :help "Delete the current stash"))
    (define-key map [ap]
      '(menu-item "Apply Stash" vc-git-stash-apply-at-point
		  :help "Apply the current stash and keep it in the stash list"))
    (define-key map [po]
      '(menu-item "Apply and Remove Stash (Pop)" vc-git-stash-pop-at-point
		  :help "Apply the current stash and remove it"))
    (define-key map [sh]
      '(menu-item "Show Stash" vc-git-stash-show-at-point
		  :help "Show the contents of the current stash"))
    map))

(defun vc-git-dir-extra-headers (dir)
  (let ((str (with-output-to-string
               (with-current-buffer standard-output
                 (vc-git--out-ok "symbolic-ref" "HEAD"))))
	(stash-list (vc-git-stash-list))
        (default-directory dir)

	branch remote remote-url stash-button stash-string)
    (if (string-match "^\\(refs/heads/\\)?\\(.+\\)$" str)
	(progn
	  (setq branch (match-string 2 str))
	  (setq remote
		(with-output-to-string
		  (with-current-buffer standard-output
		    (vc-git--out-ok "config"
                                    (concat "branch." branch ".remote")))))
	  (when (string-match "\\([^\n]+\\)" remote)
	    (setq remote (match-string 1 remote)))
          (when (> (length remote) 0)
	    (setq remote-url (vc-git-repository-url dir remote))))
      (setq branch "not (detached HEAD)"))
    (when stash-list
      (let* ((len (length stash-list))
             (limit
              (if (integerp vc-git-show-stash)
                  (min vc-git-show-stash len)
                len))
             (shown-stashes (cl-subseq stash-list 0 limit))
             (hidden-stashes (cl-subseq stash-list limit))
             (all-hideable (or (eq vc-git-show-stash t)
                               (<= len vc-git-show-stash))))
        (setq stash-button (if all-hideable
                               (vc-git-make-stash-button nil limit limit)
                             (vc-git-make-stash-button t vc-git-show-stash len))
              stash-string
              (concat
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
                               'vc-git-hideable t))))))))
    (concat
     (propertize "Branch     : " 'face 'vc-dir-header)
     (propertize branch
		 'face 'vc-dir-header-value)
     (when remote-url
       (concat
	"\n"
	(propertize "Remote     : " 'face 'vc-dir-header)
	(propertize remote-url
		    'face 'vc-dir-header-value)))
     ;; For now just a heading, key bindings can be added later for various bisect actions
     (when (file-exists-p (expand-file-name ".git/BISECT_START" (vc-git-root dir)))
       (propertize  "\nBisect     : in progress" 'face 'vc-dir-status-warning))
     (when (file-exists-p (expand-file-name ".git/rebase-apply" (vc-git-root dir)))
       (propertize  "\nRebase     : in progress" 'face 'vc-dir-status-warning))
     (if stash-list
         (concat
          (propertize "\nStash      : " 'face 'vc-dir-header)
          stash-button
          stash-string)
       (concat
	(propertize "\nStash      : " 'face 'vc-dir-header)
	(propertize "Nothing stashed"
		    'help-echo vc-git-stash-shared-help
                    'keymap vc-git-stash-shared-map
		    'face 'vc-dir-header-value))))))

(defun vc-git-branches ()
  "Return the existing branches, as a list of strings.
The car of the list is the current branch."
  (with-temp-buffer
    (vc-git--call t "branch")
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
  (log-edit--toggle-amend
   (lambda ()
     (with-output-to-string
       (vc-git-command
        standard-output 1 nil
        "log" "--max-count=1" "--pretty=format:%B" "HEAD")))))

(defvar vc-git-log-edit-mode-map
  (let ((map (make-sparse-keymap "Git-Log-Edit")))
    (define-key map "\C-c\C-s" #'vc-git-log-edit-toggle-signoff)
    (define-key map "\C-c\C-n" #'vc-git-log-edit-toggle-no-verify)
    (define-key map "\C-c\C-e" #'vc-git-log-edit-toggle-amend)
    map))

(define-derived-mode vc-git-log-edit-mode log-edit-mode "Log-Edit/git"
  "Major mode for editing Git log messages.
It is based on `log-edit-mode', and has Git-specific extensions.")

(defun vc-git-checkin (files comment &optional _rev)
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
              (let ((default-directory (file-name-directory file1)))
                (make-nearby-temp-file "git-msg")))))
    (cl-flet ((boolean-arg-fn
               (argument)
               (lambda (value) (when (equal value "yes") (list argument)))))
      ;; When operating on the whole tree, better pass "-a" than ".", since "."
      ;; fails when we're committing a merge.
      (apply #'vc-git-command nil 0 (if only files)
             (nconc (if msg-file (list "commit" "-F"
                                       (file-local-name msg-file))
                      (list "commit" "-m"))
                    (let ((args
                           (log-edit-extract-headers
                            `(("Author" . "--author")
                              ("Date" . "--date")
                              ("Amend" . ,(boolean-arg-fn "--amend"))
                              ("No-Verify" . ,(boolean-arg-fn "--no-verify"))
                              ("Sign-Off" . ,(boolean-arg-fn "--signoff")))
                            comment)))
                      (when msg-file
                        (let ((coding-system-for-write
                               (or pcsw vc-git-commits-coding-system)))
                          (write-region (car args) nil msg-file))
                        (setq args (cdr args)))
                      args)
		    (if only (list "--only" "--") '("-a")))))
    (if (and msg-file (file-exists-p msg-file)) (delete-file msg-file))))

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
    (vc-git-command
     buffer 0
     nil
     "cat-file" "blob" (concat (if rev rev "HEAD") ":" fullname))))

(defun vc-git-find-ignore-file (file)
  "Return the git ignore file that controls FILE."
  (expand-file-name ".gitignore"
		    (vc-git-root file)))

(defun vc-git-checkout (file &optional rev)
  (vc-git-command nil 0 file "checkout" (or rev "HEAD")))

(defun vc-git-revert (file &optional contents-done)
  "Revert FILE to the version stored in the git repository."
  (if contents-done
      (vc-git-command nil 0 file "update-index" "--")
    (vc-git-command nil 0 file "reset" "-q" "--")
    (vc-git-command nil nil file "checkout" "-q" "--")))

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
  (let* ((root (vc-git-root default-directory))
	 (buffer (format "*vc-git : %s*" (expand-file-name root)))
	 (git-program vc-git-program)
	 args)
    ;; If necessary, prompt for the exact command.
    ;; TODO if pushing, prompt if no default push location - cf bzr.
    (when prompt
      (setq args (split-string
		  (read-shell-command
                   (format "Git %s command: " command)
                   (format "%s %s" git-program command)
                   'vc-git-history)
		  " " t))
      (setq git-program (car  args)
	    command     (cadr args)
	    args        (cddr args)))
    (setq args (nconc args extra-args))
    (require 'vc-dispatcher)
    (apply #'vc-do-async-command buffer root git-program command args)
    (with-current-buffer buffer
      (vc-run-delayed
        (vc-compilation-mode 'git)
        (setq-local compile-command
                    (concat git-program " " command " "
                            (mapconcat #'identity args " ")))
        (setq-local compilation-directory root)
        ;; Either set `compilation-buffer-name-function' locally to nil
        ;; or use `compilation-arguments' to set `name-function'.
        ;; See `compilation-buffer-name'.
        (setq-local compilation-arguments
                    (list compile-command nil
                          (lambda (_name-of-mode) buffer)
                          nil))))
    (vc-set-async-update buffer)))

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
					 (expand-file-name ".git/FETCH_HEAD"
							   root))))
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
      (vc-git-command (current-buffer) 0 nil "remote" "get-url"
                      (or remote-name "origin"))
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
               ;; Doing a merge, so bug#20292 doesn't apply.
               (file-exists-p (expand-file-name ".git/MERGE_HEAD"
                                                (vc-git-root buffer-file-name)))
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
    (when vc-git-resolve-conflicts
      (add-hook 'after-save-hook #'vc-git-resolve-when-done nil 'local))
    (vc-message-unresolved-conflicts buffer-file-name)))

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
  "If true, follow renames in Git logs for a single file."
  :type 'boolean
  :version "26.1")

(autoload 'vc-switches "vc")

(defun vc-git-print-log (files buffer &optional shortlog start-revision limit)
  "Print commit log associated with FILES into specified BUFFER.
If SHORTLOG is non-nil, use a short format based on `vc-git-root-log-format'.
\(This requires at least Git version 1.5.6, for the --graph option.)
If START-REVISION is non-nil, it is the newest revision to show.
If LIMIT is a number, show no more than this many entries.
If LIMIT is a revision string, use it as an end-revision."
  (let ((coding-system-for-read
         (or coding-system-for-read vc-git-log-output-coding-system)))
    ;; `vc-do-command' creates the buffer, but we need it before running
    ;; the command.
    (vc-setup-buffer buffer)
    ;; If the buffer exists from a previous invocation it might be
    ;; read-only.
    (let ((inhibit-read-only t))
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
                vc-git-log-switches
                (when (numberp limit)
                  (list "-n" (format "%s" limit)))
		(when start-revision
                  (if (and limit (not (numberp limit)))
                      (list (concat start-revision ".." (if (equal limit "")
                                                            "HEAD"
                                                          limit)))
                    (list start-revision)))
                (when (eq vc-log-view-type 'with-diff)
                  (list "-p"))
		'("--")))))))

(defun vc-git-log-outgoing (buffer remote-location)
  (vc-setup-buffer buffer)
  (vc-git-command
   buffer 'async nil
   "log"
   "--no-color" "--graph" "--decorate" "--date=short"
   (format "--pretty=tformat:%s" (car vc-git-root-log-format))
   "--abbrev-commit"
   (concat (if (string= remote-location "")
	       "@{upstream}"
	     remote-location)
	   "..HEAD")))

(defun vc-git-log-incoming (buffer remote-location)
  (vc-setup-buffer buffer)
  (vc-git-command nil 0 nil "fetch")
  (vc-git-command
   buffer 'async nil
   "log"
   "--no-color" "--graph" "--decorate" "--date=short"
   (format "--pretty=tformat:%s" (car vc-git-root-log-format))
   "--abbrev-commit"
   (concat "HEAD.." (if (string= remote-location "")
			"@{upstream}"
		      remote-location))))

(defun vc-git-log-search (buffer pattern)
  "Search the log of changes for PATTERN and output results into BUFFER.

PATTERN is a basic regular expression by default in Git.

Display all entries that match log messages in long format.
With a prefix argument, ask for a command to run that will output
log entries."
  (let ((args `("log" "--no-color" "-i"
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
	  '(("^Author:[ \t]+\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)"
	     (1 'change-log-email))
	    ;; Handle the case:
	    ;; user: FirstName LastName <foo@bar>
	    ("^Author:[ \t]+\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]"
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
	    ("^\\(?:Date:   \\|AuthorDate: \\)\\(.+\\)" (1 'change-log-date))
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
    (apply #'vc-git-command t nil nil (list "log" revision "-1"  "--no-color" "--"))
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
    (vc-call-backend 'git 'diff (list file) "HEAD" nil (current-buffer))
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
(defvar font-lock-keywords)
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

(defun vc-git-diff (files &optional rev1 rev2 buffer _async)
  "Get a difference report using Git between two revisions of FILES."
  (let (process-file-side-effects
        (command "diff-tree"))
    (vc-git--asciify-coding-system)
    (if rev2
        ;; Diffing against the empty tree.
        (unless rev1 (setq rev1 "4b825dc642cb6eb9a060e54bf8d69288fbee4904"))
      (setq command "diff-index")
      (unless rev1 (setq rev1 "HEAD")))
    (if vc-git-diff-switches
        (apply #'vc-git-command (or buffer "*vc-diff*")
	       1 ; bug#21969
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
    table))

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

(defun vc-git-annotate-time ()
  (and (re-search-forward "^[0-9a-f^]+[^()]+(.*?\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\) \\(:?\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) \\([-+0-9]+\\)\\)? *[0-9]+) " nil t)
       (vc-annotate-convert-time
        (apply #'encode-time (mapcar (lambda (match)
                                       (if (match-beginning match)
                                           (string-to-number (match-string match))
                                         0))
                                     '(6 5 4 3 2 1 7))))))

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

;;; TAG SYSTEM

(defun vc-git-create-tag (dir name branchp)
  (let ((default-directory dir))
    (and (vc-git-command nil 0 nil "update-index" "--refresh")
         (if branchp
             (vc-git-command nil 0 nil "checkout" "-b" name)
           (vc-git-command nil 0 nil "tag" name)))))

(defun vc-git-retrieve-tag (dir name _update)
  (let ((default-directory dir))
    (vc-git-command nil 0 nil "checkout" name)))


;;; MISCELLANEOUS

(defun vc-git-previous-revision (file rev)
  "Git-specific version of `vc-previous-revision'."
  (if file
      (let* ((fname (file-relative-name file))
             (prev-rev (with-temp-buffer
                         (and
                          (vc-git--out-ok "rev-list" "-2" rev "--" fname)
                          (goto-char (point-max))
                          (bolp)
                          (zerop (forward-line -1))
                          (not (bobp))
                          (buffer-substring-no-properties
                           (point)
                           (1- (point-max)))))))
        (or (vc-git-symbolic-commit prev-rev) prev-rev))
    ;; We used to use "^" here, but that fails on MS-Windows if git is
    ;; invoked via a batch file, in which case cmd.exe strips the "^"
    ;; because it is a special character for cmd which process-file
    ;; does not (and cannot) quote.
    (vc-git--rev-parse (concat rev "~1"))))

(defun vc-git--rev-parse (rev)
  (with-temp-buffer
    (and
     (vc-git--out-ok "rev-parse" rev)
     (buffer-substring-no-properties (point-min) (+ (point-min) 40)))))

(defun vc-git-next-revision (file rev)
  "Git-specific version of `vc-next-revision'."
  (let* ((default-directory (vc-git-root file))
         (file (file-relative-name file))
         (current-rev
          (with-temp-buffer
            (and
             (vc-git--out-ok "rev-list" "-1" rev "--" file)
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
                  (vc-git--out-ok "rev-list" "HEAD" "--" file)
                  (goto-char (point-min))
                  (search-forward current-rev nil t)
                  (zerop (forward-line -1))
                  (buffer-substring-no-properties
                   (point)
                   (progn (forward-line 1) (1- (point)))))))))
    (or (vc-git-symbolic-commit next-rev) next-rev)))

(defun vc-git-delete-file (file)
  (vc-git-command nil 0 file "rm" "-f" "--"))

(defun vc-git-rename-file (old new)
  (vc-git-command nil 0 (list old new) "mv" "-f" "--"))

(defun vc-git-mark-resolved (files)
  (vc-git-command nil 0 files "add"))

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

;; grep-compute-defaults autoloads grep.
(declare-function grep-read-regexp "grep" ())
(declare-function grep-read-files "grep" (regexp))
(declare-function grep-expand-template "grep"
                 (template &optional regexp files dir excl))
(defvar compilation-environment)

;; Derived from `lgrep'.
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

(autoload 'vc-dir-marked-files "vc-dir")

(defun vc-git-stash (name)
  "Create a stash given the name NAME."
  (interactive "sStash name: ")
  (let ((root (vc-git-root default-directory)))
    (when root
      (apply #'vc-git--call nil "stash" "push" "-m" name
             (when (derived-mode-p 'vc-dir-mode)
               (vc-dir-marked-files)))
      (vc-resynch-buffer root t t))))

(defvar vc-git-stash-read-history nil
  "History for `vc-git-stash-read'.")

(defun vc-git-stash-read (prompt)
  "Read a Git stash.  PROMPT is a string to prompt with."
  (let ((stash (completing-read
                 prompt
                 (split-string
                  (or (vc-git--run-command-string nil "stash" "list") "") "\n")
                 nil :require-match nil 'vc-git-stash-read-history)))
    (if (string-equal stash "")
        (user-error "Not a stash")
      (string-match "^stash@{[[:digit:]]+}" stash)
      (match-string 0 stash))))

(defun vc-git-stash-show (name)
  "Show the contents of stash NAME."
  (interactive (list (vc-git-stash-read "Show stash: ")))
  (vc-setup-buffer "*vc-git-stash*")
  (vc-git-command "*vc-git-stash*" 'async nil "stash" "show" "-p" name)
  (set-buffer "*vc-git-stash*")
  (diff-mode)
  (setq buffer-read-only t)
  (pop-to-buffer (current-buffer)))

(defun vc-git-stash-apply (name)
  "Apply stash NAME."
  (interactive (list (vc-git-stash-read "Apply stash: ")))
  (vc-git-command "*vc-git-stash*" 0 nil "stash" "apply" "-q" name)
  (vc-resynch-buffer (vc-git-root default-directory) t t))

(defun vc-git-stash-pop (name)
  "Pop stash NAME."
  (interactive (list (vc-git-stash-read "Pop stash: ")))
  (vc-git-command "*vc-git-stash*" 0 nil "stash" "pop" "-q" name)
  (vc-resynch-buffer (vc-git-root default-directory) t t))

(defun vc-git-stash-delete (name)
  "Delete stash NAME."
  (interactive (list (vc-git-stash-read "Delete stash: ")))
  (vc-git-command "*vc-git-stash*" 0 nil "stash" "drop" "-q" name)
  (vc-resynch-buffer (vc-git-root default-directory) t t))

(defun vc-git-stash-snapshot ()
  "Create a stash with the current tree state."
  (interactive)
  (vc-git--call nil "stash" "save"
		(format-time-string "Snapshot on %Y-%m-%d at %H:%M"))
  (vc-git-command "*vc-git-stash*" 0 nil "stash" "apply" "-q" "stash@{0}")
  (vc-resynch-buffer (vc-git-root default-directory) t t))

(defun vc-git-stash-list ()
  (when-let ((out (vc-git--run-command-string nil "stash" "list")))
    (delete
     ""
     (split-string
      (replace-regexp-in-string
       "^stash@" "             " out)
      "\n"))))

(defun vc-git-stash-get-at-point (point)
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (if (looking-at "^ +\\({[0-9]+}\\):")
	(match-string 1)
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


;;; Internal commands

(defun vc-git-command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-git.el.
The difference to vc-do-command is that this function always invokes
`vc-git-program'."
  (let ((coding-system-for-read
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
            ,@(when revert-buffer-in-progress-p
                '("GIT_OPTIONAL_LOCKS=0")))
          process-environment)))
    (apply #'vc-do-command (or buffer "*vc*") okstatus vc-git-program
           ;; https://debbugs.gnu.org/16897
           (unless (vc-git--file-list-is-rootdir file-or-list)
             file-or-list)
           (cons "--no-pager" flags))))

(defun vc-git--file-list-is-rootdir (file-or-list)
  (and (not (cdr-safe file-or-list))
       (let ((file (or (car-safe file-or-list)
                       file-or-list)))
         (and file
              (eq ?/ (aref file (1- (length file))))
              (equal file (vc-git-root file))))))

(defun vc-git--empty-db-p ()
  "Check if the git db is empty (no commit done yet)."
  (let (process-file-side-effects)
    (not (eq 0 (vc-git--call nil "rev-parse" "--verify" "HEAD")))))

(defun vc-git--call (buffer command &rest args)
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
	    ,@(when revert-buffer-in-progress-p
		'("GIT_OPTIONAL_LOCKS=0")))
	  process-environment)))
    (apply #'process-file vc-git-program nil buffer nil "--no-pager" command args)))

(defun vc-git--out-ok (command &rest args)
  (zerop (apply #'vc-git--call '(t nil) command args)))

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

(defun vc-git-symbolic-commit (commit)
  "Translate COMMIT string into symbolic form.
Returns nil if not possible."
  (and commit
       (let ((name (with-temp-buffer
                     (and
                      (vc-git--out-ok "name-rev" "--name-only" commit)
                      (goto-char (point-min))
                      (= (forward-line 2) 1)
                      (bolp)
                      (buffer-substring-no-properties (point-min)
                                                      (1- (point-max)))))))
         (and name (not (string= name "undefined")) name))))

(provide 'vc-git)

;;; vc-git.el ends here
