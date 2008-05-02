;;; vc-git.el --- VC backend for the git version control system

;; Copyright (C) 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Alexandre Julliard <julliard@winehq.org>
;; Keywords: tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains a VC backend for the git version control
;; system.
;;

;;; Installation:

;; To install: put this file on the load-path and add Git to the list
;; of supported backends in `vc-handled-backends'; the following line,
;; placed in your ~/.emacs, will accomplish this:
;;
;;     (add-to-list 'vc-handled-backends 'Git)

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
;; STATE-QUERYING FUNCTIONS
;; * registered (file)				   OK
;; * state (file)				   OK
;; - state-heuristic (file)			   NOT NEEDED
;; - dir-state (dir)				   OK
;; * working-revision (file)			   OK
;; - latest-on-branch-p (file)			   NOT NEEDED
;; * checkout-model (files)			   OK
;; - workfile-unchanged-p (file)		   OK
;; - mode-line-string (file)			   OK
;; - prettify-state-info (file)			   OK
;; STATE-CHANGING FUNCTIONS
;; * create-repo ()				   OK
;; * register (files &optional rev comment)	   OK
;; - init-revision (file)			   NOT NEEDED
;; - responsible-p (file)			   OK
;; - could-register (file)			   NOT NEEDED, DEFAULT IS GOOD
;; - receive-file (file rev)			   NOT NEEDED
;; - unregister (file)				   OK
;; * checkin (files rev comment)		   OK
;; * find-revision (file rev buffer)		   OK
;; * checkout (file &optional editable rev)	   OK
;; * revert (file &optional contents-done)	   OK
;; - rollback (files)				   COULD BE SUPPORTED
;; - merge (file rev1 rev2)			   It would be possible to merge changes into
;;                                                 a single file, but when committing they
;;                                                 wouldn't be identified as a merge by git,
;;                                                 so it's probably not a good idea.
;; - merge-news (file)				   see `merge'
;; - steal-lock (file &optional revision)	   NOT NEEDED
;; HISTORY FUNCTIONS
;; * print-log (files &optional buffer)		   OK
;; - log-view-mode ()				   OK
;; - show-log-entry (revision)			   OK
;; - wash-log (file)				   COULD BE SUPPORTED
;; - logentry-check ()				   NOT NEEDED
;; - comment-history (file)			   ??
;; - update-changelog (files)			   COULD BE SUPPORTED
;; * diff (file &optional rev1 rev2 buffer)	   OK
;; - revision-completion-table (files)		   OK
;; - annotate-command (file buf &optional rev)	   OK
;; - annotate-time ()				   OK
;; - annotate-current-time ()			   NOT NEEDED
;; - annotate-extract-revision-at-line ()	   OK
;; SNAPSHOT SYSTEM
;; - create-snapshot (dir name branchp)		   OK
;; - assign-name (file name)			   NOT NEEDED
;; - retrieve-snapshot (dir name update)	   OK, needs to update buffers
;; MISCELLANEOUS
;; - make-version-backups-p (file)		   NOT NEEDED
;; - repository-hostname (dirname)		   NOT NEEDED
;; - previous-revision (file rev)		   OK
;; - next-revision (file rev)			   OK
;; - check-headers ()				   COULD BE SUPPORTED
;; - clear-headers ()				   NOT NEEDED
;; - delete-file (file)				   OK
;; - rename-file (old new)			   OK
;; - find-file-hook ()				   NOT NEEDED
;; - find-file-not-found-hook ()                   NOT NEEDED

(eval-when-compile
  (require 'cl)
  (require 'vc)
  (require 'grep))

(defvar git-commits-coding-system 'utf-8
  "Default coding system for git commits.")

;;; BACKEND PROPERTIES

(defun vc-git-revision-granularity () 'repository)
(defun vc-git-checkout-model (files) 'implicit)

;;; STATE-QUERYING FUNCTIONS

;;;###autoload (defun vc-git-registered (file)
;;;###autoload   "Return non-nil if FILE is registered with git."
;;;###autoload   (if (vc-find-root file ".git")       ; short cut
;;;###autoload       (progn
;;;###autoload         (load "vc-git")
;;;###autoload         (vc-git-registered file))))

(defun vc-git-registered (file)
  "Check whether FILE is registered with git."
  (when (vc-git-root file)
    (with-temp-buffer
      (let* ((dir (file-name-directory file))
	     (name (file-relative-name file dir)))
	(and (ignore-errors
               (when dir (cd dir))
               (vc-git--out-ok "ls-files" "-c" "-z" "--" name))
	     (let ((str (buffer-string)))
	       (and (> (length str) (length name))
		    (string= (substring str 0 (1+ (length name)))
                             (concat name "\0")))))))))

(defun vc-git--state-code (code)
  "Convert from a string to a added/deleted/modified state."
  (case (string-to-char code)
    (?M 'edited)
    (?A 'added)
    (?D 'removed)
    (?U 'edited)     ;; FIXME
    (?T 'edited)))   ;; FIXME

(defun vc-git-state (file)
  "Git-specific version of `vc-state'."
  ;; FIXME: This can't set 'ignored yet
  (if (not (vc-git-registered file))
      'unregistered
    (vc-git--call nil "add" "--refresh" "--" (file-relative-name file))
    (let ((diff (vc-git--run-command-string file "diff-index" "-z" "HEAD" "--")))
      (if (and diff (string-match ":[0-7]\\{6\\} [0-7]\\{6\\} [0-9a-f]\\{40\\} [0-9a-f]\\{40\\} \\([ADMUT]\\)\0[^\0]+\0"
				  diff))
	  (vc-git--state-code (match-string 1 diff))
	(if (vc-git--empty-db-p) 'added 'up-to-date)))))

(defun vc-git--ls-files-state (state &rest args)
  "Set state to STATE on all files found with git-ls-files ARGS."
  (with-temp-buffer
    (apply 'vc-git-command (current-buffer) nil nil "ls-files" "-z" args)
    (goto-char (point-min))
    (let ((start (point)))
      (while (search-forward "\0" nil t)
	(let ((file (expand-file-name
		     (buffer-substring-no-properties start (1- (point))))))
	  (vc-file-setprop file 'vc-backend (if state 'Git 'none))
	  (vc-file-setprop file 'vc-state state))
	(setq start (point))))))

(defun vc-git-dir-state (dir)
  "Git-specific version of `dir-state'."
  (vc-git--ls-files-state 'up-to-date "-c")
  (vc-git--ls-files-state 'edited "-m")
  (vc-git--ls-files-state 'removed "-d")
  (vc-git--ls-files-state 'ignored "-o" "-i" "--exclude-standard")
  (vc-git--ls-files-state nil "-o" "--exclude-standard"))

(defun vc-git-working-revision (file)
  "Git-specific version of `vc-working-revision'."
  (let ((str (with-output-to-string
               (with-current-buffer standard-output
                 (vc-git--out-ok "symbolic-ref" "HEAD")))))
    (if (string-match "^\\(refs/heads/\\)?\\(.+\\)$" str)
        (match-string 2 str)
      str)))

(defun vc-git-workfile-unchanged-p (file)
  (eq 'up-to-date (vc-git-state file)))

(defun vc-git-mode-line-string (file)
  "Return string for placement into the modeline for FILE."
  (let* ((branch (vc-git-working-revision file))
         (def-ml (vc-default-mode-line-string 'Git file))
         (help-echo (get-text-property 0 'help-echo def-ml)))
    (if (zerop (length branch))
        (propertize
         (concat def-ml "!")
         'help-echo (concat help-echo "\nNo current branch (detached HEAD)"))
      (propertize def-ml
                  'help-echo (concat help-echo "\nCurrent branch: " branch)))))

(defstruct (vc-git-extra-fileinfo
            (:copier nil)
            (:constructor vc-git-create-extra-fileinfo (old-perm new-perm &optional rename-state orig-name))
            (:conc-name vc-git-extra-fileinfo->))
  old-perm new-perm   ;; permission flags
  rename-state        ;; rename or copy state
  orig-name)          ;; original name for renames or copies

(defun vc-git-escape-file-name (name)
  "Escape a file name if necessary."
  (if (string-match "[\n\t\"\\]" name)
      (concat "\""
              (mapconcat (lambda (c)
                   (case c
                     (?\n "\\n")
                     (?\t "\\t")
                     (?\\ "\\\\")
                     (?\" "\\\"")
                     (t (char-to-string c))))
                 name "")
              "\"")
    name))

(defun vc-git-file-type-as-string (old-perm new-perm)
  "Return a string describing the file type based on its permissions."
  (let* ((old-type (lsh (or old-perm 0) -9))
	 (new-type (lsh (or new-perm 0) -9))
	 (str (case new-type
		(?\100  ;; file
		 (case old-type
		   (?\100 nil)
		   (?\120 "   (type change symlink -> file)")
		   (?\160 "   (type change subproject -> file)")))
		 (?\120  ;; symlink
		  (case old-type
		    (?\100 "   (type change file -> symlink)")
		    (?\160 "   (type change subproject -> symlink)")
		    (t "   (symlink)")))
		  (?\160  ;; subproject
		   (case old-type
		     (?\100 "   (type change file -> subproject)")
		     (?\120 "   (type change symlink -> subproject)")
		     (t "   (subproject)")))
                  (?\110 nil)  ;; directory (internal, not a real git state)
		  (?\000  ;; deleted or unknown
		   (case old-type
		     (?\120 "   (symlink)")
		     (?\160 "   (subproject)")))
		  (t (format "   (unknown type %o)" new-type)))))
    (cond (str (propertize str 'face 'font-lock-comment-face))
          ((eq new-type ?\110) "/")
          (t ""))))

(defun vc-git-rename-as-string (state extra)
  "Return a string describing the copy or rename associated with INFO, or an empty string if none."
  (let ((rename-state (when extra 
			(vc-git-extra-fileinfo->rename-state extra))))
    (if rename-state
        (propertize
         (concat "   ("
                 (if (eq rename-state 'copy) "copied from "
                   (if (eq state 'added) "renamed from "
                     "renamed to "))
                 (vc-git-escape-file-name (vc-git-extra-fileinfo->orig-name extra))
                 ")") 'face 'font-lock-comment-face)
      "")))

(defun vc-git-permissions-as-string (old-perm new-perm)
  "Format a permission change as string."
  (propertize
   (if (or (not old-perm)
           (not new-perm)
           (eq 0 (logand ?\111 (logxor old-perm new-perm))))
       "  "
     (if (eq 0 (logand ?\111 old-perm)) "+x" "-x"))
  'face 'font-lock-type-face))

(defun vc-git-status-printer (info)
  "Pretty-printer for the vc-dir-fileinfo structure."
  (let* ((state (if (vc-dir-fileinfo->directory info)
		    'DIRECTORY
		  (vc-dir-fileinfo->state info)))
         (extra (vc-dir-fileinfo->extra info))
         (old-perm (when extra (vc-git-extra-fileinfo->old-perm extra)))
         (new-perm (when extra (vc-git-extra-fileinfo->new-perm extra))))
    (insert
     "  "
     (propertize (format "%c" (if (vc-dir-fileinfo->marked info) ?* ? ))
                 'face 'font-lock-type-face)
     "  "
     (propertize
      (format "%-12s" state)
      'face (cond ((eq state 'up-to-date) 'font-lock-builtin-face)
		  ((eq state 'missing) 'font-lock-warning-face)
		  (t 'font-lock-variable-name-face))
      'mouse-face 'highlight)
     "  " (vc-git-permissions-as-string old-perm new-perm)
     "     "
     (propertize (vc-git-escape-file-name (vc-dir-fileinfo->name info))
                 'face 'font-lock-function-name-face
                 'mouse-face 'highlight)
     (vc-git-file-type-as-string old-perm new-perm)
     (vc-git-rename-as-string state extra))))

(defun vc-git-after-dir-status-stage (stage files update-function)
  "Process sentinel for the various dir-status stages."
  (let (remaining next-stage result)
    (goto-char (point-min))
    (case stage
      ('update-index
       (setq next-stage (if (vc-git--empty-db-p) 'ls-files-added
                          (if files 'ls-files-up-to-date 'diff-index))))
      ('ls-files-added
       (setq next-stage 'ls-files-unknown)
       (while (re-search-forward "\\([0-7]\\{6\\}\\) [0-9a-f]\\{40\\} 0\t\\([^\0]+\\)\0" nil t)
         (let ((new-perm (string-to-number (match-string 1) 8))
               (name (match-string 2)))
           (push (list name 'added (vc-git-create-extra-fileinfo 0 new-perm)) result))))
      ('ls-files-up-to-date
       (setq next-stage 'diff-index)
       (while (re-search-forward "\\([0-7]\\{6\\}\\) [0-9a-f]\\{40\\} 0\t\\([^\0]+\\)\0" nil t)
         (let ((perm (string-to-number (match-string 1) 8))
               (name (match-string 2)))
           (push (list name 'up-to-date (vc-git-create-extra-fileinfo perm perm)) result))))
      ('ls-files-unknown
       (when files (setq next-stage 'ls-files-ignored))
       (while (re-search-forward "\\([^\0]*?\\)\0" nil t 1)
         (push (list (match-string 1) 'unregistered (vc-git-create-extra-fileinfo 0 0)) result)))
      ('ls-files-ignored
       (while (re-search-forward "\\([^\0]*?\\)\0" nil t 1)
         (push (list (match-string 1) 'ignored (vc-git-create-extra-fileinfo 0 0)) result)))
      ('diff-index
       (setq next-stage 'ls-files-unknown)
       (while (re-search-forward
               ":\\([0-7]\\{6\\}\\) \\([0-7]\\{6\\}\\) [0-9a-f]\\{40\\} [0-9a-f]\\{40\\} \\(\\([ADMUT]\\)\0\\([^\0]+\\)\\|\\([CR]\\)[0-9]*\0\\([^\0]+\\)\0\\([^\0]+\\)\\)\0"
               nil t 1)
         (let ((old-perm (string-to-number (match-string 1) 8))
               (new-perm (string-to-number (match-string 2) 8))
               (state (or (match-string 4) (match-string 6)))
               (name (or (match-string 5) (match-string 7)))
               (new-name (match-string 8)))
           (if new-name  ; copy or rename
               (if (eq ?C (string-to-char state))
                   (push (list new-name 'added (vc-git-create-extra-fileinfo old-perm new-perm 'copy name)) result)
                 (push (list name 'removed (vc-git-create-extra-fileinfo 0 0 'rename new-name)) result)
                 (push (list new-name 'added (vc-git-create-extra-fileinfo old-perm new-perm 'rename name)) result))
             (push (list name (vc-git--state-code state) (vc-git-create-extra-fileinfo old-perm new-perm)) result))))))
    (when result
      (setq result (nreverse result))
      (when files
        (dolist (entry result) (setq files (delete (car entry) files)))
        (unless files (setq next-stage nil))))
    (when (or result (not next-stage)) (funcall update-function result next-stage))
    (when next-stage (vc-git-dir-status-goto-stage next-stage files update-function))))

(defun vc-git-dir-status-goto-stage (stage files update-function)
  (erase-buffer)
  (case stage
    ('update-index
     (if files
         (vc-git-command (current-buffer) 'async files "add" "--refresh" "--")
       (vc-git-command (current-buffer) 'async nil "update-index" "--refresh")))
    ('ls-files-added
     (vc-git-command (current-buffer) 'async files "ls-files" "-z" "-c" "-s" "--"))
    ('ls-files-up-to-date
     (vc-git-command (current-buffer) 'async files "ls-files" "-z" "-c" "-s" "--"))
    ('ls-files-unknown
     (vc-git-command (current-buffer) 'async files "ls-files" "-z" "-o"
                     "--directory" "--no-empty-directory" "--exclude-standard" "--"))
    ('ls-files-ignored
     (vc-git-command (current-buffer) 'async files "ls-files" "-z" "-o" "-i"
                     "--directory" "--no-empty-directory" "--exclude-standard" "--"))
    ('diff-index
     (vc-git-command (current-buffer) 'async files "diff-index" "-z" "-M" "HEAD" "--")))
  (vc-exec-after
   `(vc-git-after-dir-status-stage (quote ,stage) (quote ,files) (quote ,update-function))))

(defun vc-git-dir-status (dir update-function)
  "Return a list of (FILE STATE EXTRA) entries for DIR."
  ;; Further things that would have to be fixed later:
  ;; - how to handle unregistered directories
  ;; - how to support vc-dir on a subdir of the project tree
  (vc-git-dir-status-goto-stage 'update-index nil update-function))

(defun vc-git-dir-status-files (dir files default-state update-function)
  "Return a list of (FILE STATE EXTRA) entries for FILES in DIR."
  (vc-git-dir-status-goto-stage 'update-index files update-function))

(defun vc-git-status-extra-headers (dir)
  (let ((str (with-output-to-string
               (with-current-buffer standard-output
                 (vc-git--out-ok "symbolic-ref" "HEAD")))))
    (concat
     (propertize "Branch     : " 'face 'font-lock-type-face)
     (propertize 
      (if (string-match "^\\(refs/heads/\\)?\\(.+\\)$" str)
	  (match-string 2 str)
	"not (detached HEAD)")
       'face 'font-lock-variable-name-face))))

;;; STATE-CHANGING FUNCTIONS

(defun vc-git-create-repo ()
  "Create a new Git repository."
  (vc-git-command nil 0 nil "init"))

(defun vc-git-register (files &optional rev comment)
  "Register FILE into the git version-control system."
  (vc-git-command nil 0 files "update-index" "--add" "--"))

(defalias 'vc-git-responsible-p 'vc-git-root)

(defun vc-git-unregister (file)
  (vc-git-command nil 0 file "rm" "-f" "--cached" "--"))


(defun vc-git-checkin (files rev comment)
  (let ((coding-system-for-write git-commits-coding-system))
    (vc-git-command nil 0 files "commit" "-m" comment "--only" "--")))

(defun vc-git-find-revision (file rev buffer)
  (let ((coding-system-for-read 'binary)
        (coding-system-for-write 'binary)
	(fullname (substring
		   (vc-git--run-command-string
		    file "ls-files" "-z" "--full-name" "--")
		   0 -1)))
    (vc-git-command
     buffer 0
     (concat (if rev rev "HEAD") ":" fullname) "cat-file" "blob")))

(defun vc-git-checkout (file &optional editable rev)
  (vc-git-command nil 0 file "checkout" (or rev "HEAD")))

(defun vc-git-revert (file &optional contents-done)
  "Revert FILE to the version stored in the git repository."
  (if contents-done
      (vc-git-command nil 0 file "update-index" "--")
    (vc-git-command nil 0 file "checkout" "HEAD")))

;;; HISTORY FUNCTIONS

(defun vc-git-print-log (files &optional buffer)
  "Get change log associated with FILES."
  (let ((coding-system-for-read git-commits-coding-system)
	;; Support both the old print-log interface that passes a
	;; single file, and the new one that passes a file list.
	(flist (if (listp files) files (list files))))
    ;; `vc-do-command' creates the buffer, but we need it before running
    ;; the command.
    (vc-setup-buffer buffer)
    ;; If the buffer exists from a previous invocation it might be
    ;; read-only.
    (let ((inhibit-read-only t))
      ;; XXX `log-view-mode' needs to have something to identify where
      ;; the log for each individual file starts. It seems that by
      ;; default git does not output this info. So loop here and call
      ;; "git rev-list" on each file separately to make sure that each
      ;; file gets a "File:" header before the corresponding
      ;; log. Maybe there is a way to do this with one command...
      (dolist (file flist)
	(with-current-buffer
	    buffer
	  (insert "File: " (file-name-nondirectory file) "\n"))
	(vc-git-command buffer 'async (file-relative-name file)
			"rev-list" "--pretty" "HEAD" "--")))))

(defvar log-view-message-re)
(defvar log-view-file-re)
(defvar log-view-font-lock-keywords)

(define-derived-mode vc-git-log-view-mode log-view-mode "Git-Log-View"
  (require 'add-log) ;; we need the faces add-log
  ;; Don't have file markers, so use impossible regexp.
  (set (make-local-variable 'log-view-file-re) "^File:[ \t]+\\(.+\\)")
  (set (make-local-variable 'log-view-message-re)
       "^commit *\\([0-9a-z]+\\)")
  (set (make-local-variable 'log-view-font-lock-keywords)
       (append
        `((,log-view-message-re  (1 'change-log-acknowledgement))
          (,log-view-file-re (1 'change-log-file-face)))
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
           (1 'change-log-acknowledgement)
           (2 'change-log-acknowledgement))
          ("^Date:   \\(.+\\)" (1 'change-log-date))
          ("^summary:[ \t]+\\(.+\\)" (1 'log-view-message))))))

(defun vc-git-show-log-entry (revision)
  "Move to the log entry for REVISION.
REVISION may have the form BRANCH, BRANCH~N,
or BRANCH^ (where \"^\" can be repeated)."
  (goto-char (point-min))
  (search-forward "\ncommit" nil t
                  (cond ((string-match "~\\([0-9]\\)$" revision)
                         (1+ (string-to-number (match-string 1 revision))))
                        ((string-match "\\^+$" revision)
                         (1+ (length (match-string 0 revision))))
                        (t nil)))
  (beginning-of-line))

(defun vc-git-diff (files &optional rev1 rev2 buffer)
  (let ((buf (or buffer "*vc-diff*")))
    (if (and rev1 rev2)
        (vc-git-command buf 1 files "diff-tree" "--exit-code" "-p"
                        rev1 rev2 "--")
      (vc-git-command buf 1 files "diff-index" "--exit-code" "-p"
                      (or rev1 "HEAD") "--"))))

(defun vc-git-revision-table (files)
  ;; What about `files'?!?  --Stef
  (let ((table (list "HEAD")))
    (with-temp-buffer
      (vc-git-command t nil nil "for-each-ref" "--format=%(refname)")
      (goto-char (point-min))
      (while (re-search-forward "^refs/\\(heads\\|tags\\)/\\(.*\\)$" nil t)
        (push (match-string 2) table)))
    table))

(defun vc-git-revision-completion-table (files)
  (lexical-let ((files files)
                table)
    (setq table (lazy-completion-table
                 table (lambda () (vc-git-revision-table files))))
    table))

(defun vc-git-annotate-command (file buf &optional rev)
  ;; FIXME: rev is ignored
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" (if rev (concat "-r" rev)))))

(defun vc-git-annotate-time ()
  (and (re-search-forward "[0-9a-f]+[^()]+(.* \\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) \\([-+0-9]+\\) +[0-9]+) " nil t)
       (vc-annotate-convert-time
        (apply #'encode-time (mapcar (lambda (match)
                                       (string-to-number (match-string match)))
                                     '(6 5 4 3 2 1 7))))))

(defun vc-git-annotate-extract-revision-at-line ()
  (save-excursion
    (move-beginning-of-line 1)
    (and (looking-at "[0-9a-f^][0-9a-f]+")
         (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))

;;; SNAPSHOT SYSTEM

(defun vc-git-create-snapshot (dir name branchp)
  (let ((default-directory dir))
    (and (vc-git-command nil 0 nil "update-index" "--refresh")
         (if branchp
             (vc-git-command nil 0 nil "checkout" "-b" name)
           (vc-git-command nil 0 nil "tag" name)))))

(defun vc-git-retrieve-snapshot (dir name update)
  (let ((default-directory dir))
    (vc-git-command nil 0 nil "checkout" name)
    ;; FIXME: update buffers if `update' is true
    ))


;;; MISCELLANEOUS

(defun vc-git-previous-revision (file rev)
  "Git-specific version of `vc-previous-revision'."
  (let ((default-directory (file-name-directory (expand-file-name file)))
	(file (file-name-nondirectory file)))
    (vc-git-symbolic-commit
     (with-temp-buffer
       (and
	(vc-git--out-ok "rev-list" "-2" rev "--" file)
	(goto-char (point-max))
	(bolp)
	(zerop (forward-line -1))
	(not (bobp))
	(buffer-substring-no-properties
         (point)
         (1- (point-max))))))))

(defun vc-git-next-revision (file rev)
  "Git-specific version of `vc-next-revision'."
  (let* ((default-directory (file-name-directory
			     (expand-file-name file)))
         (file (file-name-nondirectory file))
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
              (1- (point-max)))))))
    (and current-rev
	 (vc-git-symbolic-commit
	  (with-temp-buffer
	    (and
	     (vc-git--out-ok "rev-list" "HEAD" "--" file)
	     (goto-char (point-min))
	     (search-forward current-rev nil t)
	     (zerop (forward-line -1))
	     (buffer-substring-no-properties
	      (point)
	      (progn (forward-line 1) (1- (point))))))))))

(defun vc-git-delete-file (file)
  (vc-git-command nil 0 file "rm" "-f" "--"))

(defun vc-git-rename-file (old new)
  (vc-git-command nil 0 (list old new) "mv" "-f" "--"))

(defvar vc-git-extra-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [git-grep]
      '(menu-item "Git grep..." vc-git-grep
		  :help "Run the `git grep' command"))
    map))

(defun vc-git-extra-menu () vc-git-extra-menu-map)

(defun vc-git-extra-status-menu () vc-git-extra-menu-map)

;; Derived from `lgrep'.
(defun vc-git-grep (regexp &optional files dir)
  "Run git grep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-command'.

Collect output in a buffer.  While git grep runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the grep output buffer,
to go to the lines where grep found matches.

This command shares argument histories with \\[rgrep] and \\[grep]."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep"
				   nil nil 'grep-history)
	     nil))
      (t (let* ((regexp (grep-read-regexp))
		(files (grep-read-files regexp))
		(dir (read-directory-name "In directory: "
					  nil default-directory t)))
	   (list regexp files dir))))))
  (require 'grep)
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((command regexp))
      (if (null files)
	  (if (string= command "git grep")
	      (setq command nil))
	(setq dir (file-name-as-directory (expand-file-name dir)))
	(setq command
	      (grep-expand-template "git grep -n -e <R> -- <F>" regexp files))
	(when command
	  (if (equal current-prefix-arg '(4))
	      (setq command
		    (read-from-minibuffer "Confirm: "
					  command nil nil 'grep-history))
	    (add-to-history 'grep-history command))))
      (when command
	(let ((default-directory dir)
	      (compilation-environment '("PAGER=")))
	  ;; Setting process-setup-function makes exit-message-function work
	  ;; even when async processes aren't supported.
	  (compilation-start command 'grep-mode))
	(if (eq next-error-last-buffer (current-buffer))
	    (setq default-directory dir))))))

;;; Internal commands

(defun vc-git-root (file)
  (vc-find-root file ".git"))

(defun vc-git-command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-git.el.
The difference to vc-do-command is that this function always invokes `git'."
  (apply 'vc-do-command buffer okstatus "git" file-or-list flags))

(defun vc-git--empty-db-p ()
  "Check if the git db is empty (no commit done yet)."
  (not (eq 0 (vc-git--call nil "rev-parse" "--verify" "HEAD"))))

(defun vc-git--call (buffer command &rest args)
  ;; We don't need to care the arguments.  If there is a file name, it
  ;; is always a relative one.  This works also for remote
  ;; directories.
  (apply 'process-file "git" nil buffer nil command args))

(defun vc-git--out-ok (command &rest args)
  (zerop (apply 'vc-git--call '(t nil) command args)))

(defun vc-git--run-command-string (file &rest args)
  "Run a git command on FILE and return its output as string."
  (let* ((ok t)
         (str (with-output-to-string
                (with-current-buffer standard-output
                  (unless (apply 'vc-git--out-ok
                                 (append args (list (file-relative-name
                                                     file))))
                    (setq ok nil))))))
    (and ok str)))

(defun vc-git-symbolic-commit (commit)
  "Translate COMMIT string into symbolic form.
Returns nil if not possible."
  (and commit
       (with-temp-buffer
	 (and
	  (vc-git--out-ok "name-rev" "--name-only" "--tags" commit)
	  (goto-char (point-min))
	  (= (forward-line 2) 1)
	  (bolp)
	  (buffer-substring-no-properties (point-min) (1- (point-max)))))))

(provide 'vc-git)

;; arch-tag: bd10664a-0e5b-48f5-a877-6c17b135be12
;;; vc-git.el ends here
