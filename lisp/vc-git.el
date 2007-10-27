;;; vc-git.el --- VC backend for the git version control system

;; Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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
;; * workfile-version (file)			   OK
;; - latest-on-branch-p (file)			   NOT NEEDED
;; * checkout-model (file)			   OK
;; - workfile-unchanged-p (file)		   OK
;; - mode-line-string (file)			   OK
;; - dired-state-info (file)			   OK
;; STATE-CHANGING FUNCTIONS
;; * create-repo ()				   OK
;; * register (files &optional rev comment)	   OK
;; - init-version (file)			   NOT NEEDED
;; - responsible-p (file)			   OK
;; - could-register (file)			   NOT NEEDED, DEFAULT IS GOOD
;; - receive-file (file rev)			   NOT NEEDED
;; - unregister (file)				   OK
;; * checkin (files rev comment)		   OK
;; * find-version (file rev buffer)		   OK
;; * checkout (file &optional editable rev)	   OK
;; * revert (file &optional contents-done)	   OK
;; - rollback (files)				   COULD BE SUPPORTED
;; - merge (file rev1 rev2)			   It would be possible to merge changes into
;;                                                 a single file, but when committing they
;;                                                 wouldn't be identified as a merge by git,
;;                                                 so it's probably not a good idea.
;; - merge-news (file)				   see `merge'
;; - steal-lock (file &optional version)	   NOT NEEDED
;; HISTORY FUNCTIONS
;; * print-log (files &optional buffer)		   OK
;; - log-view-mode ()				   OK
;; - show-log-entry (version)			   NOT NEEDED, DEFAULT IS GOOD
;; - wash-log (file)				   COULD BE SUPPORTED
;; - logentry-check ()				   NOT NEEDED
;; - comment-history (file)			   ??
;; - update-changelog (files)			   COULD BE SUPPORTED
;; * diff (file &optional rev1 rev2 buffer)	   OK
;; - revision-completion-table (file)		   NEEDED?
;; - diff-tree (dir &optional rev1 rev2)	   OK
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
;; - previous-version (file rev)		   OK
;; - next-version (file rev)			   OK
;; - check-headers ()				   COULD BE SUPPORTED
;; - clear-headers ()				   NOT NEEDED
;; - delete-file (file)				   OK
;; - rename-file (old new)			   OK
;; - find-file-hook ()				   NOT NEEDED
;; - find-file-not-found-hook ()                   NOT NEEDED

(eval-when-compile (require 'cl) (require 'vc))

(defvar git-commits-coding-system 'utf-8
  "Default coding system for git commits.")

;;; BACKEND PROPERTIES

(defun vc-git-revision-granularity ()
     'repository)

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
	      (eq 0 (call-process "git" nil '(t nil) nil "ls-files" "-c" "-z" "--" name)))
	     (let ((str (buffer-string)))
	       (and (> (length str) (length name))
		    (string= (substring str 0 (1+ (length name))) (concat name "\0")))))))))

(defun vc-git-state (file)
  "Git-specific version of `vc-state'."
  (let ((diff (vc-git--run-command-string file "diff-index" "-z" "HEAD" "--")))
    (if (and diff (string-match ":[0-7]\\{6\\} [0-7]\\{6\\} [0-9a-f]\\{40\\} [0-9a-f]\\{40\\} [ADMU]\0[^\0]+\0" diff))
        'edited
      'up-to-date)))

(defun vc-git-dir-state (dir)
  (with-temp-buffer
    (vc-git-command (current-buffer) nil nil "ls-files" "-t")
    (goto-char (point-min))
    (let ((status-char nil)
	  (file nil))
      (while (not (eobp))
	(setq status-char (char-after))
	(setq file
	      (expand-file-name
	       (buffer-substring-no-properties (+ (point) 2) (line-end-position))))
	(cond
	 ;; The rest of the possible states in "git ls-files -t" output:
         ;;      R              removed/deleted
	 ;;      K              to be killed
	 ;; should not show up in vc-dired, so don't deal with them
	 ;; here.
	 ((eq status-char ?H)
	  (vc-file-setprop file 'vc-state 'up-to-date))
	 ((eq status-char ?M)
	  (vc-file-setprop file 'vc-state 'edited))
	 ((eq status-char ?C)
	  (vc-file-setprop file 'vc-state 'edited))
	 ((eq status-char ??)
	  (vc-file-setprop file 'vc-backend 'none)
	  (vc-file-setprop file 'vc-state 'nil)))
	(forward-line)))))

(defun vc-git-workfile-version (file)
  "Git-specific version of `vc-workfile-version'."
  (let ((str (with-output-to-string
               (with-current-buffer standard-output
                 (call-process "git" nil '(t nil) nil "symbolic-ref" "HEAD")))))
    (if (string-match "^\\(refs/heads/\\)?\\(.+\\)$" str)
        (match-string 2 str)
      str)))

(defun vc-git-checkout-model (file)
  'implicit)

(defun vc-git-workfile-unchanged-p (file)
  ;; The reason this does not use the result of vc-git-state is that
  ;; git-diff-index (used by vc-git-state) doesn't refresh the cached
  ;; stat info, so if the file has been modified it will always show
  ;; up as modified in vc-git-state, even if the change has been
  ;; undone, until git-update-index --refresh is run.

  ;; OTOH the vc-git-workfile-unchanged-p implementation checks the
  ;; actual content, so it will detect the case of a file reverted
  ;; back to its original state.

  ;; The ideal implementation would be to refresh the stat cache and
  ;; then call vc-git-state, but at the moment there's no git command
  ;; to refresh a single file, so this will have to be added first.
  (let ((sha1 (vc-git--run-command-string file "hash-object" "--"))
        (head (vc-git--run-command-string file "ls-tree" "-z" "HEAD" "--")))
    (and head
         (string-match "[0-7]\\{6\\} blob \\([0-9a-f]\\{40\\}\\)\t[^\0]+\0" head)
         (string= (car (split-string sha1 "\n")) (match-string 1 head)))))

(defun vc-git-mode-line-string (file)
  "Return string for placement into the modeline for FILE."
  (let* ((branch (vc-git-workfile-version file))
         (def-ml (vc-default-mode-line-string 'Git file))
         (help-echo (get-text-property 0 'help-echo def-ml)))
    (if (zerop (length branch))
        (propertize
         (concat def-ml "!")
         'help-echo (concat help-echo "\nNo current branch (detached HEAD)"))
      (propertize def-ml
                  'help-echo (concat help-echo "\nCurrent branch: " branch)))))

(defun vc-git-dired-state-info (file)
  "Git-specific version of `vc-dired-state-info'."
  (let ((git-state (vc-state file)))
    (if (eq git-state 'edited)
	"(modified)"
      ;; fall back to the default VC representation
      (vc-default-dired-state-info 'Git file))))

;;; STATE-CHANGING FUNCTIONS

(defun vc-git-create-repo ()
  "Create a new Git repository."
  (vc-git-command "init" nil 0 nil))

(defun vc-git-register (files &optional rev comment)
  "Register FILE into the git version-control system."
  (vc-git-command nil 0 files "update-index" "--add" "--"))

(defalias 'vc-git-responsible-p 'vc-git-root)

(defun vc-git-unregister (file)
  (vc-git-command nil 0 file "rm" "-f" "--cached" "--"))


(defun vc-git-checkin (files rev comment)
  (let ((coding-system-for-write git-commits-coding-system))
    (vc-git-command nil 0 files "commit" "-m" comment "--only" "--")))

(defun vc-git-find-version (file rev buffer)
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
	   ("^Date:   \\(.+\\)" (1 'change-log-date))
	   ("^summary:[ \t]+\\(.+\\)" (1 'log-view-message))))))

(defun vc-git-diff (files &optional rev1 rev2 buffer)
  (let ((buf (or buffer "*vc-diff*")))
    (if (and rev1 rev2)
        (vc-git-command buf 1 files "diff-tree" "--exit-code" "-p" rev1 rev2 "--")
      (vc-git-command buf 1 files "diff-index" "--exit-code" "-p" (or rev1 "HEAD") "--"))))

(defun vc-git-revision-table (file)
  (let ((table (list "HEAD")))
    (with-temp-buffer
      (vc-git-command t nil nil "for-each-ref" "--format=%(refname)")
      (goto-char (point-min))
      (while (re-search-forward "^refs/\\(heads\\|tags\\)/\\(.*\\)$" nil t)
        (push (match-string 2) table)))
    table))

(defun vc-git-revision-completion-table (file)
  (lexical-let ((file file)
                table)
    (setq table (lazy-completion-table
                 table (lambda () (vc-git-revision-table file))))
    table))

(defun vc-git-diff-tree (dir &optional rev1 rev2)
  (vc-git-diff dir rev1 rev2))

(defun vc-git-annotate-command (file buf &optional rev)
  ;; FIXME: rev is ignored
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" (if rev (concat "-r" rev)))))

(defun vc-git-annotate-time ()
  (and (re-search-forward "[0-9a-f]+ (.* \\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) \\([-+0-9]+\\) +[0-9]+)" nil t)
       (vc-annotate-convert-time
        (apply #'encode-time (mapcar (lambda (match) (string-to-number (match-string match))) '(6 5 4 3 2 1 7))))))

(defun vc-git-annotate-extract-revision-at-line ()
 (save-excursion
   (move-beginning-of-line 1)
   (and (looking-at "[0-9a-f]+")
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

(defun vc-git-previous-version (file rev)
  "Git-specific version of `vc-previous-version'."
  (let ((default-directory (file-name-directory (expand-file-name file)))
	(file (file-name-nondirectory file)))
    (vc-git-symbolic-commit
     (with-temp-buffer
       (and
	(zerop
	 (call-process "git" nil '(t nil) nil "rev-list"
		       "-2" rev "--" file))
	(goto-char (point-max))
	(bolp)
	(zerop (forward-line -1))
	(not (bobp))
	(buffer-substring-no-properties
	   (point)
	   (1- (point-max))))))))

(defun vc-git-next-version (file rev)
  "Git-specific version of `vc-next-version'."
  (let* ((default-directory (file-name-directory
			     (expand-file-name file)))
	(file (file-name-nondirectory file))
	(current-rev
	 (with-temp-buffer
	   (and
	    (zerop
	     (call-process "git" nil '(t nil) nil "rev-list"
			   "-1" rev "--" file))
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
	     (zerop
	      (call-process "git" nil '(t nil) nil "rev-list"
			    "HEAD" "--" file))
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


;;; Internal commands

(defun vc-git-root (file)
  (vc-find-root file ".git"))

(defun vc-git-command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-git.el.
The difference to vc-do-command is that this function always invokes `git'."
  (apply 'vc-do-command buffer okstatus "git" file-or-list flags))

(defun vc-git--run-command-string (file &rest args)
  "Run a git command on FILE and return its output as string."
  (let* ((ok t)
         (str (with-output-to-string
                (with-current-buffer standard-output
                  (unless (eq 0 (apply #'call-process "git" nil '(t nil) nil
                                       (append args (list (file-relative-name file)))))
                    (setq ok nil))))))
    (and ok str)))

(defun vc-git-symbolic-commit (commit)
  "Translate COMMIT string into symbolic form.
Returns nil if not possible."
  (and commit
       (with-temp-buffer
	 (and
	  (zerop
	   (call-process "git" nil '(t nil) nil "name-rev"
			 "--name-only" "--tags"
			 commit))
	  (goto-char (point-min))
	  (= (forward-line 2) 1)
	  (bolp)
	  (buffer-substring-no-properties (point-min) (1- (point-max)))))))

(provide 'vc-git)

;; arch-tag: bd10664a-0e5b-48f5-a877-6c17b135be12
;;; vc-git.el ends here
