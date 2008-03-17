;;; vc-bzr.el --- VC backend for the bzr revision control system

;; Copyright (C) 2006, 2007, 2008  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>, Riccardo Murri <riccardo.murri@gmail.com>
;; Keywords: tools
;; Created: Sept 2006
;; Version: 2008-01-04 (Bzr revno 25)
;; URL: http://launchpad.net/vc-bzr

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; See <URL:http://bazaar-vcs.org/> concerning bzr.  See
;; <URL:http://launchpad.net/vc-bzr> for alternate development
;; branches of `vc-bzr'.

;; Load this library to register bzr support in VC.  

;; Known bugs
;; ==========

;; When edititing a symlink and *both* the symlink and its target
;; are bzr-versioned, `vc-bzr` presently runs `bzr status` on the
;; symlink, thereby not detecting whether the actual contents
;; (that is, the target contents) are changed.  
;; See https://bugs.launchpad.net/vc-bzr/+bug/116607

;; For an up-to-date list of bugs, please see:
;;   https://bugs.launchpad.net/vc-bzr/+bugs


;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'vc))                        ; for vc-exec-after

;; Clear up the cache to force vc-call to check again and discover
;; new functions when we reload this file.
(put 'Bzr 'vc-functions nil)

(defgroup vc-bzr nil
  "VC bzr backend."
  :version "22.2"
  :group 'vc)

(defcustom vc-bzr-program "bzr"
  "Name of the bzr command (excluding any arguments)."
  :group 'vc-bzr
  :type 'string)

(defcustom vc-bzr-diff-switches nil
  "String/list of strings specifying extra switches for bzr diff under VC."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string))
  :group 'vc-bzr)

(defcustom vc-bzr-log-switches nil
  "String/list of strings specifying extra switches for `bzr log' under VC."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string))
  :group 'vc-bzr)

;; since v0.9, bzr supports removing the progress indicators
;; by setting environment variable BZR_PROGRESS_BAR to "none".
(defun vc-bzr-command (bzr-command buffer okstatus file-or-list &rest args)
  "Wrapper round `vc-do-command' using `vc-bzr-program' as COMMAND.
Invoke the bzr command adding `BZR_PROGRESS_BAR=none' and
`LC_MESSAGES=C' to the environment."
  (let ((process-environment
         (list* "BZR_PROGRESS_BAR=none" ; Suppress progress output (bzr >=0.9)
                "LC_MESSAGES=C"         ; Force English output
                process-environment)))
    (apply 'vc-do-command buffer okstatus vc-bzr-program
           file-or-list bzr-command args)))


;;;###autoload
(defconst vc-bzr-admin-dirname ".bzr"
  "Name of the directory containing Bzr repository status files.")
;;;###autoload
(defconst vc-bzr-admin-checkout-format-file
  (concat vc-bzr-admin-dirname "/checkout/format"))
(defconst vc-bzr-admin-dirstate
  (concat vc-bzr-admin-dirname "/checkout/dirstate"))
(defconst vc-bzr-admin-branch-format-file
  (concat vc-bzr-admin-dirname "/branch/format"))
(defconst vc-bzr-admin-revhistory
  (concat vc-bzr-admin-dirname "/branch/revision-history"))
(defconst vc-bzr-admin-lastrev
  (concat vc-bzr-admin-dirname "/branch/last-revision"))

;;;###autoload (defun vc-bzr-registered (file)
;;;###autoload   (if (vc-find-root file vc-bzr-admin-checkout-format-file)
;;;###autoload       (progn
;;;###autoload         (load "vc-bzr")
;;;###autoload         (vc-bzr-registered file))))

(defun vc-bzr-root (file)
  "Return the root directory of the bzr repository containing FILE."
  ;; Cache technique copied from vc-arch.el.
  (or (vc-file-getprop file 'bzr-root)
      (let ((root (vc-find-root file vc-bzr-admin-checkout-format-file)))
	(when root (vc-file-setprop file 'bzr-root root)))))

(defun vc-bzr-registered (file)
  "Return non-nil if FILE is registered with bzr.

For speed, this function tries first to parse Bzr internal file
`checkout/dirstate', but it may fail if Bzr internal file format
has changed.  As a safeguard, the `checkout/dirstate' file is
only parsed if it contains the string `#bazaar dirstate flat
format 3' in the first line.

If the `checkout/dirstate' file cannot be parsed, fall back to
running `vc-bzr-state'."
  (lexical-let ((root (vc-bzr-root file)))
    (when root    ; Short cut.
      ;; This looks at internal files.  May break if they change
      ;; their format.
      (lexical-let ((dirstate (expand-file-name vc-bzr-admin-dirstate root)))
        (if (not (file-readable-p dirstate))
            (vc-bzr-state file)         ; Expensive.
          (with-temp-buffer
            (insert-file-contents dirstate)
            (goto-char (point-min))
            (if (not (looking-at "#bazaar dirstate flat format 3"))
                (vc-bzr-state file)     ; Some other unknown format?
              (let* ((relfile (file-relative-name file root))
                     (reldir (file-name-directory relfile)))
                (re-search-forward
                 (concat "^\0"
                         (if reldir (regexp-quote (directory-file-name reldir)))
                         "\0"
                         (regexp-quote (file-name-nondirectory relfile))
                         "\0")
                 nil t)))))))))

(defconst vc-bzr-state-words
  "added\\|ignored\\|kind changed\\|modified\\|removed\\|renamed\\|unknown"
  "Regexp matching file status words as reported in `bzr' output.")

(defun vc-bzr-file-name-relative (filename)
  "Return file name FILENAME stripped of the initial Bzr repository path."
  (lexical-let*
      ((filename* (expand-file-name filename))
       (rootdir (vc-bzr-root filename*)))
    (when rootdir 
         (file-relative-name filename* rootdir))))

(defun vc-bzr-status (file)
  "Return FILE status according to Bzr.
Return value is a cons (STATUS . WARNING), where WARNING is a
string or nil, and STATUS is one of the symbols: `added',
`ignored', `kindchanged', `modified', `removed', `renamed', `unknown',
which directly correspond to `bzr status' output, or 'unchanged
for files whose copy in the working tree is identical to the one
in the branch repository, or nil for files that are not
registered with Bzr.

If any error occurred in running `bzr status', then return nil."
  (with-temp-buffer
    (let ((ret (condition-case nil
                   (vc-bzr-command "status" t 0 file)
                 (file-error nil)))     ; vc-bzr-program not found.
          (status 'unchanged))
          ;; the only secure status indication in `bzr status' output
          ;; is a couple of lines following the pattern::
          ;;   | <status>:
          ;;   |   <file name>
          ;; if the file is up-to-date, we get no status report from `bzr',
          ;; so if the regexp search for the above pattern fails, we consider
          ;; the file to be up-to-date.
          (goto-char (point-min))
          (when (re-search-forward
                 ;; bzr prints paths relative to the repository root.
                 (concat "^\\(" vc-bzr-state-words "\\):[ \t\n]+"
                         (regexp-quote (vc-bzr-file-name-relative file))
                         ;; Bzr appends a '/' to directory names and
                         ;; '*' to executable files
                         (if (file-directory-p file) "/?" "\\*?")
                         "[ \t\n]*$")
                 nil t)
            (lexical-let ((statusword (match-string 1)))
              ;; Erase the status text that matched.
              (delete-region (match-beginning 0) (match-end 0))
              (setq status
                    (intern (replace-regexp-in-string " " "" statusword)))))
          (when status
            (goto-char (point-min))
            (skip-chars-forward " \n\t") ;Throw away spaces.
            (cons status
                  ;; "bzr" will output warnings and informational messages to
                  ;; stderr; due to Emacs' `vc-do-command' (and, it seems,
                  ;; `start-process' itself) limitations, we cannot catch stderr
                  ;; and stdout into different buffers.  So, if there's anything
                  ;; left in the buffer after removing the above status
                  ;; keywords, let us just presume that any other message from
                  ;; "bzr" is a user warning, and display it.
                  (unless (eobp) (buffer-substring (point) (point-max))))))))

(defun vc-bzr-state (file)
  (lexical-let ((result (vc-bzr-status file)))
    (when (consp result)
      (if (cdr result)
          (message "Warnings in `bzr' output: %s" (cdr result)))
      (cdr (assq (car result)
                 '((added . edited)
                   (kindchanged . edited)
                   (renamed . edited)
                   (modified . edited)
                   (removed . edited)
                   (ignored . nil)
                   (unknown . nil)
                   (unchanged . up-to-date)))))))

(defun vc-bzr-resolve-when-done ()
  "Call \"bzr resolve\" if the conflict markers have been removed."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^<<<<<<< " nil t)
      (vc-bzr-command "resolve" nil 0 buffer-file-name)
      ;; Remove the hook so that it is not called multiple times.
      (remove-hook 'after-save-hook 'vc-bzr-resolve-when-done t))))

(defun vc-bzr-find-file-hook ()
  (when (and buffer-file-name
             ;; FIXME: We should check that "bzr status" says "conflict".
             (file-exists-p (concat buffer-file-name ".BASE"))
             (file-exists-p (concat buffer-file-name ".OTHER"))
             (file-exists-p (concat buffer-file-name ".THIS"))
             ;; If "bzr status" says there's a conflict but there are no
             ;; conflict markers, it's not clear what we should do.
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "^<<<<<<< " nil t)))
    ;; TODO: the merge algorithm used in `bzr merge' is nicely configurable,
    ;; but the one in `bzr pull' isn't, so it would be good to provide an
    ;; elisp function to remerge from the .BASE/OTHER/THIS files.
    (smerge-start-session)
    (add-hook 'after-save-hook 'vc-bzr-resolve-when-done nil t)
    (message "There are unresolved conflicts in this file")))

(defun vc-bzr-workfile-unchanged-p (file)
  (eq 'unchanged (car (vc-bzr-status file))))

(defun vc-bzr-working-revision (file)
  (lexical-let*
      ((rootdir (vc-bzr-root file))
       (branch-format-file (expand-file-name vc-bzr-admin-branch-format-file
                                             rootdir))
       (revhistory-file (expand-file-name vc-bzr-admin-revhistory rootdir))
       (lastrev-file (expand-file-name vc-bzr-admin-lastrev rootdir)))
    ;; This looks at internal files to avoid forking a bzr process.
    ;; May break if they change their format.
    (if (file-exists-p branch-format-file)
        (with-temp-buffer
          (insert-file-contents branch-format-file) 
          (goto-char (point-min))
          (cond
           ((or
             (looking-at "Bazaar-NG branch, format 0.0.4")
             (looking-at "Bazaar-NG branch format 5"))
            ;; count lines in .bzr/branch/revision-history
            (insert-file-contents revhistory-file) 
            (number-to-string (count-lines (line-end-position) (point-max))))
           ((looking-at "Bazaar Branch Format 6 (bzr 0.15)")
            ;; revno is the first number in .bzr/branch/last-revision
            (insert-file-contents lastrev-file) 
            (if (re-search-forward "[0-9]+" nil t)
                (buffer-substring (match-beginning 0) (match-end 0))))))
      ;; fallback to calling "bzr revno"
      (lexical-let*
          ((result (vc-bzr-command-discarding-stderr
                    vc-bzr-program "revno" file))
           (exitcode (car result))
           (output (cdr result)))
        (cond
         ((eq exitcode 0) (substring output 0 -1))
         (t nil))))))

(defun vc-bzr-checkout-model (file)
  'implicit)

(defun vc-bzr-create-repo ()
  "Create a new Bzr repository."
  (vc-bzr-command "init" nil 0 nil))

(defun vc-bzr-init-revision (&optional file)
  "Always return nil, as Bzr cannot register explicit versions."
  nil)

(defun vc-bzr-register (files &optional rev comment)
  "Register FILE under bzr.
Signal an error unless REV is nil.
COMMENT is ignored."
  (if rev (error "Can't register explicit revision with bzr"))
  (vc-bzr-command "add" nil 0 files))

;; Could run `bzr status' in the directory and see if it succeeds, but
;; that's relatively expensive.
(defalias 'vc-bzr-responsible-p 'vc-bzr-root
  "Return non-nil if FILE is (potentially) controlled by bzr.
The criterion is that there is a `.bzr' directory in the same
or a superior directory.")

(defun vc-bzr-could-register (file)
  "Return non-nil if FILE could be registered under bzr."
  (and (vc-bzr-responsible-p file)      ; shortcut
       (condition-case ()
           (with-temp-buffer
             (vc-bzr-command "add" t 0 file "--dry-run")
             ;; The command succeeds with no output if file is
             ;; registered (in bzr 0.8).
             (goto-char (point-min))
             (looking-at "added "))
         (error))))

(defun vc-bzr-unregister (file)
  "Unregister FILE from bzr."
  (vc-bzr-command "remove" nil 0 file "--keep"))

(defun vc-bzr-checkin (files rev comment)
  "Check FILE in to bzr with log message COMMENT.
REV non-nil gets an error."
  (if rev (error "Can't check in a specific revision with bzr"))
  (vc-bzr-command "commit" nil 0 files "-m" comment))

(defun vc-bzr-find-version (file rev buffer)
  "Fetch version REV of file FILE and put it into BUFFER."
    (with-current-buffer buffer
      (if (and rev (stringp rev) (not (string= rev "")))
          (vc-bzr-command "cat" t 0 file "-r" rev)
        (vc-bzr-command "cat" t 0 file))))

(defun vc-bzr-checkout (file &optional editable rev destfile)
  "Checkout revision REV of FILE from bzr to DESTFILE.
EDITABLE is ignored."
  (unless destfile
    (setq destfile (vc-version-backup-file-name file rev)))
  (let ((coding-system-for-read 'binary)
        (coding-system-for-write 'binary))
    (with-temp-file destfile
      (if (and rev (stringp rev) (not (string= rev "")))
          (vc-bzr-command "cat" t 0 file "-r" rev)
        (vc-bzr-command "cat" t 0 file)))))

(defun vc-bzr-revert (file &optional contents-done)
  (unless contents-done
    (with-temp-buffer (vc-bzr-command "revert" t 0 file))))

(defvar log-view-message-re)
(defvar log-view-file-re)
(defvar log-view-font-lock-keywords)
(defvar log-view-current-tag-function)

(define-derived-mode vc-bzr-log-view-mode log-view-mode "Bzr-Log-View"
  (remove-hook 'log-view-mode-hook 'vc-bzr-log-view-mode) ;Deactivate the hack.
  (require 'add-log)
  (set (make-local-variable 'log-view-file-re) "^Working file:[ \t]+\\(.+\\)")
  (set (make-local-variable 'log-view-message-re)
       "^ *-+\n *\\(?:revno: \\([0-9.]+\\)\\|merged: .+\\)")
  (set (make-local-variable 'log-view-font-lock-keywords)
       ;; log-view-font-lock-keywords is careful to use the buffer-local
       ;; value of log-view-message-re only since Emacs-23.
       (append `((,log-view-message-re . 'log-view-message-face))
               ;; log-view-font-lock-keywords
               '(("^ *committer: \
\\([^<(]+?\\)[  ]*[(<]\\([[:alnum:]_.+-]+@[[:alnum:]_.-]+\\)[>)]"
                  (1 'change-log-name)
                  (2 'change-log-email))
                 ("^ *timestamp: \\(.*\\)" (1 'change-log-date-face))))))

(defun vc-bzr-print-log (files &optional buffer) ; get buffer arg in Emacs 22
  "Get bzr change log for FILES into specified BUFFER."
  ;; `vc-do-command' creates the buffer, but we need it before running
  ;; the command.
  (vc-setup-buffer buffer)
  ;; If the buffer exists from a previous invocation it might be
  ;; read-only.
  (let ((inhibit-read-only t))
    ;; FIXME: `vc-bzr-command' runs `bzr log' with `LC_MESSAGES=C', so
    ;; the log display may not what the user wants - but I see no other
    ;; way of getting the above regexps working.
    (dolist (file files)
      (with-current-buffer buffer
	;; Insert the file name so that log-view.el can find it.
	(insert "Working file: " file "\n")) ;; Like RCS/CVS.
      (apply 'vc-bzr-command "log" buffer 0 file
	     (if (stringp vc-bzr-log-switches)
		 (list vc-bzr-log-switches)
	       vc-bzr-log-switches))))
  ;; FIXME: Until Emacs-23, VC was missing a hook to sort out the mode for
  ;; the buffer, or at least set the regexps right.
  (unless (fboundp 'vc-default-log-view-mode)
    (add-hook 'log-view-mode-hook 'vc-bzr-log-view-mode)))

(defun vc-bzr-show-log-entry (revision)
  "Find entry for patch name REVISION in bzr change log buffer."
  (goto-char (point-min))
  (let (case-fold-search)
    (if (re-search-forward
	 ;; "revno:" can appear either at the beginning of a line, or indented.
	 (concat "^[ ]*-+\n[ ]*revno: " 
		 ;; The revision can contain ".", quote it so that it
		 ;; does not interfere with regexp matching.
		 (regexp-quote revision) "$") nil t)
        (beginning-of-line 0)
      (goto-char (point-min)))))

(defun vc-bzr-diff (files &optional rev1 rev2 buffer)
  "VC bzr backend for diff."
  ;; `bzr diff' exits with code 1 if diff is non-empty.
  (apply #'vc-bzr-command "diff" (or buffer "*vc-diff*") 1 files
         "--diff-options" (mapconcat 'identity 
                                     (vc-diff-switches-list bzr)
				     " ")
         ;; This `when' is just an optimization because bzr-1.2 is *much*
         ;; faster when the revision argument is not given.
         (when (or rev1 rev2)
           (list "-r" (format "%s..%s"
                              (or rev1 "revno:-1") 
                              (or rev2 ""))))))


;; FIXME: vc-{next,previous}-revision need fixing in vc.el to deal with
;; straight integer revisions.

(defun vc-bzr-delete-file (file)
  "Delete FILE and delete it in the bzr repository."
  (condition-case ()
      (delete-file file)
    (file-error nil))
  (vc-bzr-command "remove" nil 0 file))

(defun vc-bzr-rename-file (old new)
  "Rename file from OLD to NEW using `bzr mv'."
  (vc-bzr-command "mv" nil 0 new old))

(defvar vc-bzr-annotation-table nil
  "Internal use.")
(make-variable-buffer-local 'vc-bzr-annotation-table)

(defun vc-bzr-annotate-command (file buffer &optional revision)
  "Prepare BUFFER for `vc-annotate' on FILE.
Each line is tagged with the revision number, which has a `help-echo'
property containing author and date information."
  (apply #'vc-bzr-command "annotate" buffer 0 file "--long" "--all"
         (if revision (list "-r" revision)))
  (with-current-buffer buffer
    ;; Store the tags for the annotated source lines in a hash table
    ;; to allow saving space by sharing the text properties.
    (setq vc-bzr-annotation-table (make-hash-table :test 'equal))
    (goto-char (point-min))
    (while (re-search-forward "^\\( *[0-9]+\\) +\\(.+\\) +\\([0-9]\\{8\\}\\) |"
                              nil t)
      (let* ((rev (match-string 1))
             (author (match-string 2))
             (date (match-string 3))
             (key (match-string 0))
             (tag (gethash key vc-bzr-annotation-table)))
        (unless tag
          (setq tag (propertize rev 'help-echo (concat "Author: " author
                                                       ", date: " date)
                                'mouse-face 'highlight))
          (puthash key tag vc-bzr-annotation-table))
        (replace-match "")
        (insert tag " |")))))

(defun vc-bzr-annotate-time ()
  (when (re-search-forward "^ *[0-9]+ |" nil t)
    (let ((prop (get-text-property (line-beginning-position) 'help-echo)))
      (string-match "[0-9]+\\'" prop)
      (vc-annotate-convert-time
       (encode-time 0 0 0
                    (string-to-number (substring (match-string 0 prop) 6 8))
                    (string-to-number (substring (match-string 0 prop) 4 6))
                    (string-to-number (substring (match-string 0 prop) 0 4))
                    )))))

(defun vc-bzr-annotate-extract-revision-at-line ()
  "Return revision for current line of annoation buffer, or nil.
Return nil if current line isn't annotated."
  (save-excursion
    (beginning-of-line)
    (if (looking-at " *\\([0-9]+\\) | ")
        (match-string-no-properties 1))))

(defun vc-bzr-command-discarding-stderr (command &rest args)
  "Execute shell command COMMAND (with ARGS); return its output and exitcode.
Return value is a cons (EXITCODE . OUTPUT), where EXITCODE is
the (numerical) exit code of the process, and OUTPUT is a string
containing whatever the process sent to its standard output
stream.  Standard error output is discarded."
  (with-temp-buffer
    (cons
     (apply #'call-process command nil (list (current-buffer) nil) nil args)
     (buffer-substring (point-min) (point-max)))))

;; TODO: it would be nice to mark the conflicted files in  VC Dired,
;; and implement a command to run ediff and `bzr resolve' once the 
;; changes have been merged.
(defun vc-bzr-dir-state (dir &optional localp)
  "Find the VC state of all files in DIR and its subdirectories.
Optional argument LOCALP is always ignored."
  (let ((bzr-root-directory (vc-bzr-root dir))
        (at-start t)
        current-bzr-state current-vc-state)
    ;; Check that DIR is a bzr repository.
    (unless (file-name-absolute-p bzr-root-directory)
      (error "Cannot find bzr repository for directory `%s'" dir))
    ;; `bzr ls --versioned' lists all versioned files;
    ;; assume they are up-to-date, unless we are given
    ;; evidence of the contrary.
    (setq at-start t)
    (with-temp-buffer
      (buffer-disable-undo)		;; Because these buffers can get huge
      (vc-bzr-command "ls" t 0 nil "--versioned")
      (goto-char (point-min))
      (while (or at-start
                 (eq 0 (forward-line)))
        (setq at-start nil)
        (let ((file (expand-file-name
                     (buffer-substring-no-properties 
                      (line-beginning-position) (line-end-position))
                     bzr-root-directory)))
          ;; files are up-to-date unless they appear in the `bzr
          ;; status' output below
          (vc-file-setprop file 'vc-state 'up-to-date)
          ;; XXX: is this correct? what happens if one 
          ;; mixes different SCMs in the same dir?
          ;; Anyway, we're looking at the output of `bzr ls --versioned',
          ;; so we know these files are registered with Bzr.
          (vc-file-setprop file 'vc-backend 'Bzr))))
    ;; `bzr status' reports on added/modified/renamed and unknown/ignored files
    (setq at-start t)
    (with-temp-buffer 
      (vc-bzr-command "status" t 0 nil)
      (goto-char (point-min))
      (while (or at-start
                 (eq 0 (forward-line)))
        (setq at-start nil)
        (cond 
         ((looking-at "^added") 
          (setq current-vc-state 'added)
          (setq current-bzr-state 'added))
         ((looking-at "^kind changed") 
          (setq current-vc-state 'edited)
          (setq current-bzr-state 'kindchanged))
         ((looking-at "^modified") 
          (setq current-vc-state 'edited)
          (setq current-bzr-state 'modified))
         ((looking-at "^renamed") 
          (setq current-vc-state 'edited)
          (setq current-bzr-state 'renamed))
         ((looking-at "^ignored")
          (setq current-vc-state 'ignored)
          (setq current-bzr-state 'not-versioned))
         ((looking-at "^unknown")
          (setq current-vc-state 'unregistered)
          (setq current-bzr-state 'not-versioned))
         ((looking-at "  ")
          ;; file names are indented by two spaces
          (when current-vc-state
            (let ((file (expand-file-name
                         (buffer-substring-no-properties
                          (match-end 0) (line-end-position))
                         bzr-root-directory)))
              (vc-file-setprop file 'vc-state current-vc-state)
              (vc-file-setprop file 'vc-bzr-state current-bzr-state)
              (when (eq 'added current-bzr-state)
                (vc-file-setprop file 'vc-working-revision "0"))))
          (when (eq 'not-versioned current-bzr-state)
            (let ((file (expand-file-name
                         (buffer-substring-no-properties
                          (match-end 0) (line-end-position))
                         bzr-root-directory)))
              (vc-file-setprop file 'vc-backend 'none)
              (vc-file-setprop file 'vc-state nil))))
         (t
          ;; skip this part of `bzr status' output
          (setq current-vc-state nil)
          (setq current-bzr-state nil)))))))

(defun vc-bzr-dired-state-info (file)
  "Bzr-specific version of `vc-dired-state-info'."
  (if (eq 'edited (vc-state file))
        (concat "(" (symbol-name (or (vc-file-getprop file 'vc-bzr-state) 
                                     'edited)) ")")
    ;; else fall back to default vc.el representation
    (vc-default-dired-state-info 'Bzr file)))

;; XXX Experimental function for the vc-dired replacement.
;; XXX: this needs testing, it's probably incomplete. 
(defun vc-bzr-after-dir-status (update-function status-buffer)
  (let ((status-str nil)
	(file nil)
	(translation '(("+N" . added)
		       ("-D" . removed)
		       (" M" . edited)
		       ;; XXX: what about ignored files?
		       (" D" . deleted)
		       ("? " . unregistered)))
	(translated nil)
	(result nil))
      (goto-char (point-min))
      (while (not (eobp))
	(setq status-str
	      (buffer-substring-no-properties (point) (+ (point) 2)))
	(setq file
	      (buffer-substring-no-properties (+ (point) 4)
					      (line-end-position)))
	(setq translated (assoc status-str translation))
	(push (cons file (cdr translated)) result)
	(forward-line))
      ;; Remove the temporary buffer.
      (kill-buffer (current-buffer))
      (funcall update-function result status-buffer)))

;; XXX Experimental function for the vc-dired replacement.
;; XXX This probably needs some further refinement and testing.
(defun vc-bzr-dir-status (dir update-function status-buffer)
  "Return a list of conses (file . state) for DIR."
  (with-current-buffer
      (get-buffer-create
       (expand-file-name " *VC-bzr* tmp status" dir))
    (erase-buffer)
    ;; XXX: Is this the right command to use?
    (vc-bzr-command "status" (current-buffer) 'async dir "-v" "-S")
    (vc-exec-after
     `(vc-bzr-after-dir-status (quote ,update-function) ,status-buffer))
    (current-buffer)))

;;; Revision completion

(defun vc-bzr-complete-with-prefix (prefix action table string pred)
  (let ((comp (complete-with-action action table string pred)))
    (if (stringp comp)
        (concat prefix comp)
      comp)))

(defun vc-bzr-revision-completion-table (files)
  (lexical-let ((files files))
    ;; What about using `files'?!?  --Stef
    (lambda (string pred action)
      (cond
       ((string-match "\\`\\(ancestor\\|branch\\|\\(revno:\\)?[-0-9]+:\\):"
                      string)
        (vc-bzr-complete-with-prefix (substring string 0 (match-end 0))
                                     action
                                     'read-file-name-internal
                                     (substring string (match-end 0))
                                     ;; Dropping `pred'.   Maybe we should just
                                     ;; stash it in `read-file-name-predicate'?
                                     nil))
       ((string-match "\\`\\(before\\):" string)
        (vc-bzr-complete-with-prefix (substring string 0 (match-end 0))
                                     action
                                     (vc-bzr-revision-completion-table files)
                                     (substring string (match-end 0))
                                     pred))
       ((string-match "\\`\\(tag\\):" string)
        (let ((prefix (substring string 0 (match-end 0)))
              (tag (substring string (match-end 0)))
              (table nil))
          (with-temp-buffer
            ;; "bzr-1.2 tags" is much faster with --show-ids.
            (call-process vc-bzr-program nil '(t) nil "tags" "--show-ids")
            ;; The output is ambiguous, unless we assume that revids do not
            ;; contain spaces.
            (goto-char (point-min))
            (while (re-search-forward "^\\(.*[^ \n]\\) +[^ \n]*$" nil t)
              (push (match-string-no-properties 1) table)))
          (vc-bzr-complete-with-prefix prefix action table tag pred)))

       ((string-match "\\`\\(revid\\):" string)
        ;; FIXME: How can I get a list of revision ids?
        )
       (t
        (complete-with-action action '("revno:" "revid:" "last:" "before:"
                                       "tag:" "date:" "ancestor:" "branch:"
                                       "submit:")
                              string pred))))))

(eval-after-load "vc"
  '(add-to-list 'vc-directory-exclusion-list vc-bzr-admin-dirname t))

(provide 'vc-bzr)
;; arch-tag: 8101bad8-4e92-4e7d-85ae-d8e08b4e7c06
;;; vc-bzr.el ends here
