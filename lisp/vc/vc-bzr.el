;;; vc-bzr.el --- VC backend for the bzr revision control system  -*- lexical-binding: t -*-

;; Copyright (C) 2006-2020 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; 	   Riccardo Murri <riccardo.murri@gmail.com>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: vc tools
;; Created: Sept 2006
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

;; See <URL:http://bazaar.canonical.com/> concerning bzr.

;; This library provides bzr support in VC.

;; Known bugs
;; ==========

;; When editing a symlink and *both* the symlink and its target
;; are bzr-versioned, `vc-bzr' presently runs `bzr status' on the
;; symlink, thereby not detecting whether the actual contents
;; (that is, the target contents) are changed.

;;; Properties of the backend

(defun vc-bzr-revision-granularity () 'repository)
(defun vc-bzr-checkout-model (_files) 'implicit)

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'vc-dispatcher)
  (require 'vc-dir))                    ; vc-dir-at-event

(declare-function vc-deduce-fileset "vc"
                  (&optional observer allow-unregistered
                             state-model-only-files))


;; Clear up the cache to force vc-call to check again and discover
;; new functions when we reload this file.
(put 'Bzr 'vc-functions nil)

(defgroup vc-bzr nil
  "VC Bazaar (bzr) backend."
  :version "22.2"
  :group 'vc)

(defcustom vc-bzr-program "bzr"
  "Name of the bzr command (excluding any arguments)."
  :group 'vc-bzr
  :type 'string)

(defcustom vc-bzr-diff-switches nil
  "String or list of strings specifying switches for bzr diff under VC.
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
                 (const :tag "None" t)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string))
  :group 'vc-bzr)

(defcustom vc-bzr-annotate-switches nil
  "String or list of strings specifying switches for bzr annotate under VC.
If nil, use the value of `vc-annotate-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "25.1"
  :group 'vc-bzr)

(defcustom vc-bzr-log-switches nil
  "String or list of strings specifying switches for bzr log under VC."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string))
  :group 'vc-bzr)

(defcustom vc-bzr-status-switches
  (ignore-errors
    (with-temp-buffer
      (let ((process-environment (cons (format "BZR_LOG=%s" null-device)
                                       process-environment)))
        (call-process vc-bzr-program nil t nil "help" "status"))
      (if (search-backward "--no-classify" nil t)
          "--no-classify")))
  "String or list of strings specifying switches for bzr status under VC.
The option \"--no-classify\" should be present if your bzr supports it."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string))
  :group 'vc-bzr
  :version "24.1")

;; since v0.9, bzr supports removing the progress indicators
;; by setting environment variable BZR_PROGRESS_BAR to "none".
(defun vc-bzr-command (bzr-command buffer okstatus file-or-list &rest args)
  "Wrapper round `vc-do-command' using `vc-bzr-program' as COMMAND.
Invoke the bzr command adding `BZR_PROGRESS_BAR=none' and
`LC_MESSAGES=C' to the environment.  If BZR-COMMAND is \"status\",
prepends `vc-bzr-status-switches' to ARGS."
  (let ((process-environment
         `("BZR_PROGRESS_BAR=none" ; Suppress progress output (bzr >=0.9)
           "LC_MESSAGES=C"         ; Force English output
           ,@process-environment)))
    (apply 'vc-do-command (or buffer "*vc*") okstatus vc-bzr-program
           file-or-list bzr-command
           (if (and (string-equal "status" bzr-command)
                    vc-bzr-status-switches)
               (append (if (stringp vc-bzr-status-switches)
                           (list vc-bzr-status-switches)
                         vc-bzr-status-switches)
                       args)
             args))))

(defun vc-bzr-async-command (bzr-command &rest args)
  "Wrapper round `vc-do-async-command' using `vc-bzr-program' as COMMAND.
Invoke the bzr command adding `BZR_PROGRESS_BAR=none' and
`LC_MESSAGES=C' to the environment.
Use the current Bzr root directory as the ROOT argument to
`vc-do-async-command', and specify an output buffer named
\"*vc-bzr : ROOT*\".  Return this buffer."
  (let* ((process-environment
	  `("BZR_PROGRESS_BAR=none" "LC_MESSAGES=C"
            ,@process-environment))
	 (root (vc-bzr-root default-directory))
	 (buffer (format "*vc-bzr : %s*" (expand-file-name root))))
    (apply 'vc-do-async-command buffer root
	   vc-bzr-program bzr-command args)
    buffer))

;;;###autoload
(defconst vc-bzr-admin-dirname ".bzr"
  "Name of the directory containing Bzr repository status files.")
;; Used in the autoloaded vc-bzr-registered; see below.
;;;###autoload
(defconst vc-bzr-admin-checkout-format-file
  (concat vc-bzr-admin-dirname "/checkout/format")
  "Name of the format file in a .bzr directory.")
(defconst vc-bzr-admin-dirstate
  (concat vc-bzr-admin-dirname "/checkout/dirstate"))
(defconst vc-bzr-admin-branch-format-file
  (concat vc-bzr-admin-dirname "/branch/format"))
(defconst vc-bzr-admin-revhistory
  (concat vc-bzr-admin-dirname "/branch/revision-history"))
(defconst vc-bzr-admin-lastrev
  (concat vc-bzr-admin-dirname "/branch/last-revision"))
(defconst vc-bzr-admin-branchconf
  (concat vc-bzr-admin-dirname "/branch/branch.conf"))

(defun vc-bzr-root (file)
  "Return the root directory of the bzr repository containing FILE."
  ;; Cache technique copied from vc-arch.el.
  (or (vc-file-getprop file 'bzr-root)
      (let ((root (vc-find-root file vc-bzr-admin-checkout-format-file)))
	(when root (vc-file-setprop file 'bzr-root root)))))

(defun vc-bzr-branch-conf (file)
  "Return the Bazaar branch settings for file FILE, as an alist.
Each element of the returned alist has the form (NAME . VALUE),
which are the name and value of a Bazaar setting, as strings.

The settings are read from the file \".bzr/branch/branch.conf\"
in the repository root directory of FILE."
  (let (settings)
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name vc-bzr-admin-branchconf (vc-bzr-root file)))
      (while (re-search-forward "^\\([^#=][^=]*?\\) *= *\\(.*\\)$" nil t)
	(push (cons (match-string 1) (match-string 2)) settings)))
    settings))

(defun vc-bzr-sha1 (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (sha1 (current-buffer))))

(defun vc-bzr-state-heuristic (file)
  "Like `vc-bzr-state' but hopefully without running Bzr."
  ;; `bzr status' could be slow with large histories and pending merges,
  ;; so this tries to avoid calling it if possible.  bzr status is
  ;; faster now, so this is not as important as it was.
  ;;
  ;; This function tries first to parse Bzr internal file
  ;; `checkout/dirstate', but it may fail if Bzr internal file format
  ;; has changed.  As a safeguard, the `checkout/dirstate' file is
  ;; only parsed if it contains the string `#bazaar dirstate flat
  ;; format 3' in the first line.
  ;; If the `checkout/dirstate' file cannot be parsed, fall back to
  ;; running `vc-bzr-state'."
  ;;
  ;; The format of the dirstate file is explained in bzrlib/dirstate.py
  ;; in the bzr distribution.  Basically:
  ;; header-line giving the version of the file format in use.
  ;; a few lines of stuff
  ;; entries, one per line, with null-separated fields.  Each line:
  ;; entry_key = dirname (may be empty), basename, file-id
  ;; current = common ( = kind, fingerprint, size, executable )
  ;;           + working ( = packed_stat )
  ;; parent = common ( as above ) + history ( = rev_id )
  ;; kinds = (r)elocated, (a)bsent, (d)irectory, (f)ile, (l)ink
  (let* ((root (vc-bzr-root file))
         (dirstate (expand-file-name vc-bzr-admin-dirstate root)))
    (when root                          ; Short cut.
      (condition-case err
          (with-temp-buffer
            (insert-file-contents dirstate)
            (goto-char (point-min))
            (if (not (looking-at "#bazaar dirstate flat format 3"))
                (vc-bzr-state file)     ; Some other unknown format?
              (let* ((relfile (file-relative-name file root))
                     (reldir (file-name-directory relfile)))
                (cond
                 ((not
                   (re-search-forward
                    (concat "^\0"
                            (if reldir (regexp-quote
                                        (directory-file-name reldir)))
                            "\0"
                            (regexp-quote (file-name-nondirectory relfile))
                            "\0"
                            "[^\0]*\0"             ;id?
                            "\\([^\0]*\\)\0"       ;"a/f/d", a=removed?
                            "\\([^\0]*\\)\0"       ;sha1 (empty if conflicted)?
                            "\\([^\0]*\\)\0"       ;size?p
                            ;; y/n.  Whether or not the current copy
                            ;; was executable the last time bzr checked?
                            "[^\0]*\0"
                            "[^\0]*\0"  ;?
                            ;; Parent information.  Absent in a new repo.
                            "\\(?:\\([^\0]*\\)\0"  ;"a/f/d" a=added?
                            "\\([^\0]*\\)\0"       ;sha1 again?
                            "\\([^\0]*\\)\0"       ;size again?
                            ;; y/n.  Whether or not the repo thinks
                            ;; the file should be executable?
                            "\\([^\0]*\\)\0"
                            "[^\0]*\0\\)?" ;last revid?
                            ;; There are more fields when merges are pending.
                            )
                    nil t))
                  'unregistered)
                 ;; Apparently the second sha1 is the one we want: when
                 ;; there's a conflict, the first sha1 is absent (and the
                 ;; first size seems to correspond to the file with
                 ;; conflict markers).
                 ((eq (char-after (match-beginning 1)) ?a) 'removed)
                 ;; If there is no parent, this must be a new repo.
                 ;; If file is in dirstate, can only be added (b#8025).
                 ((or (not (match-beginning 4))
                      (eq (char-after (match-beginning 4)) ?a)) 'added)
                 ((or (and (eql (string-to-number (match-string 3))
				(file-attribute-size (file-attributes file)))
                           (equal (match-string 5)
                                  (save-match-data (vc-bzr-sha1 file)))
                           ;; For a file, does the executable state match?
                           ;; (Bug#7544)
                           (or (not
                                (eq (char-after (match-beginning 1)) ?f))
                               (let ((exe
                                      (memq
                                       ?x
                                       (mapcar
                                        'identity
					(file-attribute-modes
					 (file-attributes file))))))
                                 (if (eq (char-after (match-beginning 7))
                                         ?y)
                                     exe
                                   (not exe)))))
                      (and
                       ;; It looks like for lightweight
                       ;; checkouts \2 is empty and we need to
                       ;; look for size in \6.
                       (eq (match-beginning 2) (match-end 2))
                       (eql (string-to-number (match-string 6))
                            (file-attribute-size (file-attributes file)))
                       (equal (match-string 5)
                              (vc-bzr-sha1 file))))
                  'up-to-date)
                 (t 'edited)))))
        ;; The dirstate file can't be read, or some other problem.
        (error
         (message "Falling back on \"slow\" status detection (%S)" err)
         (vc-bzr-state file))))))

;; This is a cheap approximation that is autoloaded.  If it finds a
;; possible match it loads this file and runs the real function.
;; It requires vc-bzr-admin-checkout-format-file to be autoloaded too.
;;;###autoload (defun vc-bzr-registered (file)
;;;###autoload   (if (vc-find-root file vc-bzr-admin-checkout-format-file)
;;;###autoload       (progn
;;;###autoload         (load "vc-bzr" nil t)
;;;###autoload         (vc-bzr-registered file))))

(defun vc-bzr-registered (file)
  "Return non-nil if FILE is registered with bzr."
  (let ((state (vc-bzr-state-heuristic file)))
    (not (memq state '(nil unregistered ignored)))))

(defconst vc-bzr-state-words
  "added\\|ignored\\|kind changed\\|modified\\|removed\\|renamed\\|unknown"
  "Regexp matching file status words as reported in `bzr' output.")

;; History of Bzr commands.
(defvar vc-bzr-history nil)

(defun vc-bzr-file-name-relative (filename)
  "Return file name FILENAME stripped of the initial Bzr repository path."
  (let* ((filename* (expand-file-name filename))
         (rootdir (vc-bzr-root filename*)))
    (when rootdir
         (file-relative-name filename* rootdir))))

(defvar vc-bzr-error-regexp-alist
  '(("^\\( M[* ]\\|\\+N \\|-D \\|\\|  \\*\\|R[M ] \\) \\(.+\\)" 2 nil nil 1)
    ("^C  \\(.+\\)" 2)
    ("^Text conflict in \\(.+\\)" 1 nil nil 2)
    ("^Using saved parent location: \\(.+\\)" 1 nil nil 0))
  "Value of `compilation-error-regexp-alist' in *vc-bzr* buffers.")

;; To be called via vc-pull from vc.el, which requires vc-dispatcher.
(declare-function vc-exec-after "vc-dispatcher" (code))
(declare-function vc-set-async-update "vc-dispatcher" (process-buffer))
(declare-function vc-compilation-mode "vc-dispatcher" (backend))

(defun vc-bzr--pushpull (command prompt)
    "Run COMMAND (a string; either push or pull) on the current Bzr branch.
If PROMPT is non-nil, prompt for the Bzr command to run."
  (let* ((vc-bzr-program vc-bzr-program)
	 (branch-conf (vc-bzr-branch-conf default-directory))
	 ;; Check whether the branch is bound.
	 (bound (assoc "bound" branch-conf))
	 (bound (and bound (equal "true" (downcase (cdr bound)))))
	 (has-loc (assoc (if (equal command "push")
			     "push_location"
			   "parent_location")
			 branch-conf))
	 args)
    (when bound
      (if (equal command "push")
	  (user-error "Cannot push a bound branch")
	(setq command "update")))
    ;; If necessary, prompt for the exact command.
    (when (or prompt (if (equal command "push")
			 (not has-loc)
		       (not (or bound has-loc))))
      (setq args (split-string
		  (read-shell-command
		   (format "Bzr %s command: " command)
		   (format "%s %s" vc-bzr-program command)
		   'vc-bzr-history)
		  " " t))
      (setq vc-bzr-program (car  args)
	    command        (cadr args)
	    args           (cddr args)))
    (require 'vc-dispatcher)
    (let ((buf (apply 'vc-bzr-async-command command args)))
      (with-current-buffer buf
        (vc-run-delayed
          (vc-compilation-mode 'bzr)
          (setq-local compile-command
                      (concat vc-bzr-program " " command " "
                              (if args (mapconcat 'identity args " ") "")))))
      (vc-set-async-update buf))))

(defun vc-bzr-pull (prompt)
  "Pull changes into the current Bzr branch.
Normally, this runs \"bzr pull\".  However, if the branch is a
bound branch, run \"bzr update\" instead.  If there is no default
location from which to pull or update, or if PROMPT is non-nil,
prompt for the Bzr command to run."
  (vc-bzr--pushpull "pull" prompt))

(defun vc-bzr-push (prompt)
  "Push changes from the current Bzr branch.
Normally, this runs \"bzr push\".  If there is no push location,
or if PROMPT is non-nil, prompt for the Bzr command to run."
  (vc-bzr--pushpull "push" prompt))

(defun vc-bzr-merge-branch ()
  "Merge another Bzr branch into the current one.
Prompt for the Bzr command to run, providing a pre-defined merge
source (an upstream branch or a previous merge source) as a
default if it is available."
  (let* ((branch-conf (vc-bzr-branch-conf default-directory))
	 ;; "bzr merge" without an argument defaults to submit_branch,
	 ;; then parent_location.  Extract the specific location and
	 ;; add it explicitly to the command line.
	 (setting nil)
	 (location
	  (cond
	   ((setq setting (assoc "submit_branch" branch-conf))
	    (cdr setting))
	   ((setq setting (assoc "parent_location" branch-conf))
	    (cdr setting))))
	 (cmd
	  (split-string
	   (read-shell-command
	    "Bzr merge command: "
	    (concat vc-bzr-program " merge --pull"
		    (if location (concat " " location) ""))
	    'vc-bzr-history)
	   " " t))
	 (vc-bzr-program (car  cmd))
	 (command        (cadr cmd))
	 (args           (cddr cmd)))
    (let ((buf (apply 'vc-bzr-async-command command args)))
      (with-current-buffer buf (vc-run-delayed (vc-compilation-mode 'bzr)))
      (vc-set-async-update buf))))

(defun vc-bzr-status (file)
  "Return FILE status according to Bzr.
Return value is a cons (STATUS . WARNING), where WARNING is a
string or nil, and STATUS is one of the symbols: `added',
`ignored', `kindchanged', `modified', `removed', `renamed', `unknown',
which directly correspond to `bzr status' output, or `unchanged'
for files whose copy in the working tree is identical to the one
in the branch repository (or whose status not be determined)."
;; Doc used to also say the following, but AFAICS, it has never been true.
;;
;;   ", or nil for files that are not registered with Bzr.
;;   If any error occurred in running `bzr status', then return nil."
;;
;; Rather than returning nil in case of an error, it returns
;; (unchanged . WARNING).  FIXME unchanged is not the best status to
;; return in case of error.
  (with-temp-buffer
    ;; This is with-demoted-errors without the condition-case-unless-debug
    ;; annoyance, which makes it fail during ert testing.
    (condition-case err (vc-bzr-command "status" t 0 file)
      (error (message "Error: %S" err) nil))
    (let ((status 'unchanged))
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
        (let ((statusword (match-string 1)))
          ;; Erase the status text that matched.
          (delete-region (match-beginning 0) (match-end 0))
          (setq status
                (intern (replace-regexp-in-string " " "" statusword)))))
      (when status
        (goto-char (point-min))
        (skip-chars-forward " \n\t") ;Throw away spaces.
        (cons status
              ;; "bzr" will output warnings and informational messages to
              ;; stderr; due to Emacs's `vc-do-command' (and, it seems,
              ;; `start-process' itself) limitations, we cannot catch stderr
              ;; and stdout into different buffers.  So, if there's anything
              ;; left in the buffer after removing the above status
              ;; keywords, let us just presume that any other message from
              ;; "bzr" is a user warning, and display it.
              (unless (eobp) (buffer-substring (point) (point-max))))))))

(defun vc-bzr-state (file)
  (let ((result (vc-bzr-status file)))
    (when (consp result)
      (let ((warnings (cdr result)))
        (when warnings
          ;; bzr 2.3.0 returns info about shelves, which is not really a warning
          (when (string-match "[0-9]+ shel\\(f\\|ves\\) exists?\\..*?\n" warnings)
            (setq warnings (replace-match "" nil nil warnings)))
          (unless (string= warnings "")
            (message "Warnings in `bzr' output: %s" warnings))))
      (cdr (assq (car result)
                 '((added . added)
                   (kindchanged . edited)
                   (renamed . edited)
                   (modified . edited)
                   (removed . removed)
                   (ignored . ignored)
                   (unknown . unregistered)
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
    (vc-message-unresolved-conflicts buffer-file-name)))

(defun vc-bzr-version-dirstate (dir)
  "Try to return as a string the bzr revision ID of directory DIR.
This uses the dirstate file's parent revision entry.
Returns nil if unable to find this information."
  (let ((file (expand-file-name ".bzr/checkout/dirstate" dir)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (and (looking-at "#bazaar dirstate flat format 3")
             (forward-line 3)
             (looking-at "[0-9]+\0\\([^\0\n]+\\)\0")
             (match-string 1))))))

(defun vc-bzr-working-revision (file)
  (let* ((rootdir (vc-bzr-root file))
         (branch-format-file (expand-file-name vc-bzr-admin-branch-format-file
                                               rootdir))
         (revhistory-file (expand-file-name vc-bzr-admin-revhistory rootdir))
         (lastrev-file (expand-file-name vc-bzr-admin-lastrev rootdir)))
    ;; This looks at internal files to avoid forking a bzr process.
    ;; May break if they change their format.
    (if (and (file-exists-p branch-format-file)
	     ;; For lightweight checkouts (obtained with bzr co --lightweight)
	     ;; the branch-format-file does not contain the revision
	     ;; information, we need to look up the branch-format-file
	     ;; in the place where the lightweight checkout comes
	     ;; from.  We only do that if it's a local file.
	     (let ((location-fname (expand-file-name
				    (concat vc-bzr-admin-dirname
					    "/branch/location") rootdir)))
	       ;; The existence of this file is how we distinguish
	       ;; lightweight checkouts.
	       (if (file-exists-p location-fname)
		   (with-temp-buffer
		     (insert-file-contents location-fname)
		     ;; If the lightweight checkout points to a
		     ;; location in the local file system, then we can
		     ;; look there for the version information.
		     (when (re-search-forward "file://\\(.+\\)" nil t)
		       (let ((l-c-parent-dir (match-string 1)))
			 (when (and (memq system-type '(ms-dos windows-nt))
				    (string-match-p "^/[[:alpha:]]:"
                                                    l-c-parent-dir))
			   ;;; The non-Windows code takes a shortcut by using
			   ;;; the host/path separator slash as the start of
			   ;;; the absolute path.  That does not work on
			   ;;; Windows, so we must remove it (bug#5345)
			   (setq l-c-parent-dir (substring l-c-parent-dir 1)))
			 (setq branch-format-file
			       (expand-file-name vc-bzr-admin-branch-format-file
						 l-c-parent-dir))
			 (setq lastrev-file
			       (expand-file-name vc-bzr-admin-lastrev
                                                 l-c-parent-dir))
			 ;; FIXME: maybe it's overkill to check if both these
			 ;; files exist.
			 (and (file-exists-p branch-format-file)
			      (file-exists-p lastrev-file)
			      (equal (vc-bzr-version-dirstate l-c-parent-dir)
				     (vc-bzr-version-dirstate rootdir))))))
		 t)))
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
           ((or
	     (looking-at "Bazaar Branch Format 6 (bzr 0.15)")
	     (looking-at "Bazaar Branch Format 7 (needs bzr 1.6)"))
            ;; revno is the first number in .bzr/branch/last-revision
            (insert-file-contents lastrev-file)
            (when (re-search-forward "[0-9]+" nil t)
	      (buffer-substring (match-beginning 0) (match-end 0))))))
      ;; Fallback to calling "bzr revno --tree".
      ;; The "--tree" matters for lightweight checkouts not on the same
      ;; revision as the parent.
      (let* ((result (vc-bzr-command-discarding-stderr
                      vc-bzr-program "revno" "--tree"
                      (file-relative-name file)))
             (exitcode (car result))
             (output (cdr result)))
        (cond
         ((and (eq exitcode 0) (not (zerop (length output))))
          (substring output 0 -1))
         (t nil))))))

(defun vc-bzr-create-repo ()
  "Create a new Bzr repository."
  (vc-bzr-command "init" nil 0 nil))

(defun vc-bzr-previous-revision (_file rev)
  (if (string-match "\\`[0-9]+\\'" rev)
      (number-to-string (1- (string-to-number rev)))
    (concat "before:" rev)))

(defun vc-bzr-next-revision (_file rev)
  (if (string-match "\\`[0-9]+\\'" rev)
      (number-to-string (1+ (string-to-number rev)))
    (error "Don't know how to compute the next revision of %s" rev)))

(defun vc-bzr-register (files &optional _comment)
  "Register FILES under bzr. COMMENT is ignored."
  (vc-bzr-command "add" nil 0 files))

;; Could run `bzr status' in the directory and see if it succeeds, but
;; that's relatively expensive.
(defalias 'vc-bzr-responsible-p 'vc-bzr-root
  "Return non-nil if FILE is (potentially) controlled by bzr.
The criterion is that there is a `.bzr' directory in the same
or a superior directory.")

(defun vc-bzr-unregister (file)
  "Unregister FILE from bzr."
  (vc-bzr-command "remove" nil 0 file "--keep"))

(declare-function log-edit-extract-headers "log-edit" (headers string))

(defun vc-bzr--sanitize-header (arg)
  ;; Newlines in --fixes (and probably other fields as well) trigger a nasty
  ;; Bazaar bug; see https://bugs.launchpad.net/bzr/+bug/1094180.
  (lambda (str) (list arg
                 (replace-regexp-in-string "\\`[ \t]+\\|[ \t]+\\'"
                                           "" (replace-regexp-in-string
                                               "\n[ \t]?" " " str)))))

(defun vc-bzr-checkin (files comment &optional _rev)
  "Check FILES in to bzr with log message COMMENT."
  (apply 'vc-bzr-command "commit" nil 0 files
         (cons "-m" (log-edit-extract-headers
                     `(("Author" . ,(vc-bzr--sanitize-header "--author"))
                       ("Date" . ,(vc-bzr--sanitize-header "--commit-time"))
                       ("Fixes" . ,(vc-bzr--sanitize-header "--fixes")))
                     comment))))

(defun vc-bzr-find-revision (file rev buffer)
  "Fetch revision REV of file FILE and put it into BUFFER."
    (with-current-buffer buffer
      (if (and rev (stringp rev) (not (string= rev "")))
          (vc-bzr-command "cat" t 0 file "-r" rev)
        (vc-bzr-command "cat" t 0 file))))

(defun vc-bzr-find-ignore-file (file)
  "Return the root directory of the repository of FILE."
  (expand-file-name ".bzrignore"
		    (vc-bzr-root file)))

(defun vc-bzr-checkout (_file &optional rev)
  (if rev (error "Operation not supported")
    ;; Else, there's nothing to do.
    nil))

(defun vc-bzr-revert (file &optional contents-done)
  (unless contents-done
    (with-temp-buffer (vc-bzr-command "revert" t 0 file "--no-backup"))))

(defvar log-view-message-re)
(defvar log-view-file-re)
(defvar log-view-font-lock-keywords)
(defvar log-view-per-file-logs)
(defvar log-view-expanded-log-entry-function)

(define-derived-mode vc-bzr-log-view-mode log-view-mode "Bzr-Log-View"
  (remove-hook 'log-view-mode-hook 'vc-bzr-log-view-mode) ;Deactivate the hack.
  (require 'add-log)
  (set (make-local-variable 'log-view-per-file-logs) nil)
  (set (make-local-variable 'log-view-file-re) regexp-unmatchable)
  (set (make-local-variable 'log-view-message-re)
       (if (eq vc-log-view-type 'short)
	   "^ *\\([0-9.]+\\): \\(.*?\\)[ \t]+\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\( \\[merge\\]\\)?"
	 "^ *\\(?:revno: \\([0-9.]+\\)\\|merged: .+\\)"))
  ;; Allow expanding short log entries
  (when (eq vc-log-view-type 'short)
    (setq truncate-lines t)
    (set (make-local-variable 'log-view-expanded-log-entry-function)
	 'vc-bzr-expanded-log-entry))
  (set (make-local-variable 'log-view-font-lock-keywords)
       ;; log-view-font-lock-keywords is careful to use the buffer-local
       ;; value of log-view-message-re only since Emacs-23.
       (if (eq vc-log-view-type 'short)
	 (append `((,log-view-message-re
		    (1 'log-view-message)
		    (2 'change-log-name)
		    (3 'change-log-date)
		    (4 'change-log-list nil lax))))
	 (append `((,log-view-message-re . 'log-view-message))
		 ;; log-view-font-lock-keywords
		 '(("^ *\\(?:committer\\|author\\): \
\\([^<(]+?\\)[  ]*[(<]\\([[:alnum:]_.+-]+@[[:alnum:]_.-]+\\)[>)]"
		    (1 'change-log-name)
		    (2 'change-log-email))
		   ("^ *timestamp: \\(.*\\)" (1 'change-log-date)))))))

(autoload 'vc-setup-buffer "vc-dispatcher")

(defun vc-bzr-print-log (files buffer &optional shortlog start-revision limit)
  "Print commit log associated with FILES into specified BUFFER.
If SHORTLOG is non-nil, use --line format.
If START-REVISION is non-nil, it is the newest revision to show.
If LIMIT is non-nil, show no more than this many entries."
  ;; `vc-do-command' creates the buffer, but we need it before running
  ;; the command.
  (vc-setup-buffer buffer)
  ;; If the buffer exists from a previous invocation it might be
  ;; read-only.
  ;; FIXME: `vc-bzr-command' runs `bzr log' with `LC_MESSAGES=C', so
  ;; the log display may not what the user wants - but I see no other
  ;; way of getting the above regexps working.
  (with-current-buffer buffer
    (apply 'vc-bzr-command "log" buffer 'async files
	   (append
	    (if shortlog '("--line") '("--long"))
	    ;; The extra complications here when start-revision and limit
	    ;; are set are due to bzr log's --forward argument, which
	    ;; could be enabled via an alias in bazaar.conf.
	    ;; Svn, for example, does not have this problem, because
	    ;; it doesn't have --forward.  Instead, you can use
	    ;; svn --log -r HEAD:0 or -r 0:HEAD as you prefer.
	    ;; Bzr, however, insists in -r X..Y that X come before Y.
	    (if start-revision
		(list (format
		       (if (and limit (= limit 1))
			   ;; This means we don't have to use --no-aliases.
			   ;; Is -c any different to -r in this case?
			   "-r%s"
			 "-r..%s") start-revision)))
            (if (eq vc-log-view-type 'with-diff) (list "-p"))
	    (when limit (list "-l" (format "%s" limit)))
	    ;; There is no sensible way to combine --limit and --forward,
	    ;; and it breaks the meaning of START-REVISION as the
	    ;; _newest_ revision.  See bug#14168.
	    ;; Eg bzr log --forward -r ..100 --limit 50 prints
	    ;; revisions 1-50 rather than 50-100.  There
	    ;; seems no way in general to get bzr to print revisions
	    ;; 50-100 in --forward order in that case.
	    ;; FIXME There may be other alias stuff we want to keep.
	    ;; Is there a way to just suppress --forward?
	    ;; As of 2013/4 the only caller uses limit = 1, so it does
	    ;; not matter much.
	    (and start-revision limit (> limit 1) '("--no-aliases"))
	    (if (stringp vc-bzr-log-switches)
		(list vc-bzr-log-switches)
	      vc-bzr-log-switches)))))

(defun vc-bzr-expanded-log-entry (revision)
  (with-temp-buffer
    (apply 'vc-bzr-command "log" t nil nil
           (append
            (list "--long" (format "-r%s" revision))
            (if (stringp vc-bzr-log-switches)
                (list vc-bzr-log-switches)
              vc-bzr-log-switches)))
    (goto-char (point-min))
    (when (looking-at "^-+\n")
      ;; Indent the expanded log entry.
      (indent-region (match-end 0) (point-max) 2)
      (buffer-substring (match-end 0) (point-max)))))

(defun vc-bzr-log-incoming (buffer remote-location)
  (apply 'vc-bzr-command "missing" buffer 'async nil
	 (list "--theirs-only" (unless (string= remote-location "") remote-location))))

(defun vc-bzr-log-outgoing (buffer remote-location)
  (apply 'vc-bzr-command "missing" buffer 'async nil
	 (list "--mine-only" (unless (string= remote-location "") remote-location))))

(defun vc-bzr-show-log-entry (revision)
  "Find entry for patch name REVISION in bzr change log buffer."
  (goto-char (point-min))
  (when revision
    (let (case-fold-search
	  found)
      (if (re-search-forward
	   ;; "revno:" can appear either at the beginning of a line,
	   ;; or indented.
	   (concat "^[ ]*-+\n[ ]*revno: "
		   ;; The revision can contain ".", quote it so that it
		   ;; does not interfere with regexp matching.
		   (regexp-quote revision) "$") nil t)
	  (progn
	    (beginning-of-line 0)
	    (setq found t))
	(goto-char (point-min)))
      found)))

(autoload 'vc-switches "vc")

(defun vc-bzr-diff (files &optional rev1 rev2 buffer async)
  "VC bzr backend for diff."
  (let* ((switches (vc-switches 'bzr 'diff))
         (args
          (append
           ;; Only add --diff-options if there are any diff switches.
           (unless (zerop (length switches))
             (list "--diff-options" (mapconcat 'identity switches " ")))
           ;; This `when' is just an optimization because bzr-1.2 is *much*
           ;; faster when the revision argument is not given.
           (when (or rev1 rev2)
             (list "-r" (format "%s..%s"
                                (or rev1 "revno:-1")
                                (or rev2 "")))))))
    ;; `bzr diff' exits with code 1 if diff is non-empty.
    (apply #'vc-bzr-command "diff" (or buffer "*vc-diff*")
           (if async 1 'async) files
           args)))


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
  (setq old (expand-file-name old))
  (setq new (expand-file-name new))
  (vc-bzr-command "mv" nil 0 new old)
  (message "Renamed %s => %s" old new))

(defvar vc-bzr-annotation-table nil
  "Internal use.")
(make-variable-buffer-local 'vc-bzr-annotation-table)

(defun vc-bzr-annotate-command (file buffer &optional revision)
  "Prepare BUFFER for `vc-annotate' on FILE.
Each line is tagged with the revision number, which has a `help-echo'
property containing author and date information."
  (apply #'vc-bzr-command "annotate" buffer 'async file "--long" "--all"
         (append (vc-switches 'bzr 'annotate)
		 (if revision (list "-r" revision)))))

(declare-function vc-annotate-convert-time "vc-annotate" (&optional time))

(defun vc-bzr-annotate-time ()
  (when (re-search-forward "^[0-9.]+ +[^\n ]* +\\([0-9]\\{8\\}\\) |" nil t)
    (let ((str (match-string-no-properties 1)))
      (vc-annotate-convert-time
       (encode-time 0 0 0
                    (string-to-number (substring str 6 8))
                    (string-to-number (substring str 4 6))
                    (string-to-number (substring str 0 4)))))))

(defun vc-bzr-annotate-extract-revision-at-line ()
  "Return revision for current line of annotation buffer, or nil.
Return nil if current line isn't annotated."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^\\([0-9.]+\\) +[^\n ]* +\\([0-9]\\{8\\}\\) |")
        (match-string-no-properties 1))))

(defun vc-bzr-command-discarding-stderr (command &rest args)
  "Execute shell command COMMAND (with ARGS); return its output and exitcode.
Return value is a cons (EXITCODE . OUTPUT), where EXITCODE is
the (numerical) exit code of the process, and OUTPUT is a string
containing whatever the process sent to its standard output
stream.  Standard error output is discarded."
  (with-temp-buffer
    (cons
     (apply #'process-file command nil (list (current-buffer) nil) nil args)
     (buffer-substring (point-min) (point-max)))))

(cl-defstruct (vc-bzr-extra-fileinfo
            (:copier nil)
            (:constructor vc-bzr-create-extra-fileinfo (extra-name))
            (:conc-name vc-bzr-extra-fileinfo->))
  extra-name)         ;; original name for rename targets, new name for

(declare-function vc-default-dir-printer "vc-dir" (backend fileentry))

(defun vc-bzr-dir-printer (info)
  "Pretty-printer for the vc-dir-fileinfo structure."
  (let ((extra (vc-dir-fileinfo->extra info)))
    (vc-default-dir-printer 'Bzr info)
    (when extra
      (insert (propertize
	       (format "   (renamed from %s)"
		       (vc-bzr-extra-fileinfo->extra-name extra))
	       'face 'font-lock-comment-face)))))

;; FIXME: this needs testing, it's probably incomplete.
(defun vc-bzr-after-dir-status (update-function relative-dir)
  (let ((status-str nil)
	(translation '(("+N " . added)
		       ("-D " . removed)
		       (" M " . edited) ;; file text modified
		       ("  *" . edited) ;; execute bit changed
		       (" M*" . edited) ;; text modified + execute bit changed
		       ("I  " . ignored)
		       (" D " . missing)
                       ;; For conflicts, should we list the .THIS/.BASE/.OTHER?
		       ("C  " . conflict)
		       ("?  " . unregistered)
		       ;; No such state, but we need to distinguish this case.
		       ("R  " . renamed)
		       ("RM " . renamed)
		       ;; For a non existent file FOO, the output is:
		       ;; bzr: ERROR: Path(s) do not exist: FOO
		       ("bzr" . not-found)
		       ;; If the tree is not up to date, bzr will print this warning:
		       ;; working tree is out of date, run 'bzr update'
		       ;; ignore it.
		       ;; FIXME: maybe this warning can be put in the vc-dir header...
		       ("wor" . not-found)
                       ;; Ignore "P " and "P." for pending patches.
		       ("P  " . not-found)
		       ("P. " . not-found)
                       ))
	(translated nil)
	(result nil))
      (goto-char (point-min))
      ;; Skip a warning message that can occur in some bzr installations.
      ;; vc-bzr-dir-extra-headers already reports it.
      ;; Perhaps we should just discard stderr?
      (and (looking-at "bzr: WARNING: bzrlib version doesn't match")
           (re-search-forward "^bzr is version" nil t)
           (forward-line 1))
      (while (not (eobp))
        ;; Bzr 2.3.0 added this if there are shelves.  (Bug#8170)
        (unless (looking-at "[0-9]+ shel\\(f\\|ves\\) exists?\\.")
          (setq status-str
                (buffer-substring-no-properties (point) (+ (point) 3)))
          (setq translated (cdr (assoc status-str translation)))
          (cond
           ((eq translated 'conflict)
            ;; For conflicts the file appears twice in the listing: once
            ;; with the M flag and once with the C flag, so take care
            ;; not to add it twice to `result'.  Ugly.
            (let* ((file
                    (buffer-substring-no-properties
                     ;;For files with conflicts the format is:
                     ;;C   Text conflict in FILENAME
                     ;; Bah.
                     (+ (point) 21) (line-end-position)))
                   (entry (assoc file result)))
              (when entry
                (setf (nth 1 entry) 'conflict))))
           ((eq translated 'renamed)
            (re-search-forward "R[ M]  \\(.*\\) => \\(.*\\)$" (line-end-position) t)
            (let ((new-name (file-relative-name (match-string 2) relative-dir))
                  (old-name (file-relative-name (match-string 1) relative-dir)))
              (push (list new-name 'edited
                          (vc-bzr-create-extra-fileinfo old-name)) result)))
           ;; do nothing for non existent files
           ((eq translated 'not-found))
           (t
            (push (list (file-relative-name
                         (buffer-substring-no-properties
                          (+ (point) 4)
                          (line-end-position)) relative-dir)
                        translated) result))))
        (forward-line))
      (funcall update-function result)))

(defun vc-bzr-dir-status-files (dir files update-function)
  "Return a list of conses (file . state) for DIR."
  (apply 'vc-bzr-command "status" (current-buffer) 'async dir "-v" "-S" files)
  (vc-run-delayed
   (vc-bzr-after-dir-status update-function
                            ;; "bzr status" results are relative to
                            ;; the bzr root directory, NOT to the
                            ;; directory "bzr status" was invoked in.
                            ;; Ugh.
                            ;; We pass the relative directory here so
                            ;; that `vc-bzr-after-dir-status' can
                            ;; frob the results accordingly.
                            (file-relative-name dir (vc-bzr-root dir)))))

(defvar vc-bzr-shelve-map
  (let ((map (make-sparse-keymap)))
    ;; Turn off vc-dir marking
    (define-key map [mouse-2] 'ignore)

    (define-key map [down-mouse-3] 'vc-bzr-shelve-menu)
    (define-key map "\C-k" 'vc-bzr-shelve-delete-at-point)
    (define-key map "=" 'vc-bzr-shelve-show-at-point)
    (define-key map "\C-m" 'vc-bzr-shelve-show-at-point)
    (define-key map "A" 'vc-bzr-shelve-apply-and-keep-at-point)
    (define-key map "P" 'vc-bzr-shelve-apply-at-point)
    (define-key map "S" 'vc-bzr-shelve-snapshot)
    map))

(defvar vc-bzr-shelve-menu-map
  (let ((map (make-sparse-keymap "Bzr Shelve")))
    (define-key map [de]
      '(menu-item "Delete Shelf" vc-bzr-shelve-delete-at-point
		  :help "Delete the current shelf"))
    (define-key map [ap]
      '(menu-item "Apply and Keep Shelf" vc-bzr-shelve-apply-and-keep-at-point
		  :help "Apply the current shelf and keep it"))
    (define-key map [po]
      '(menu-item "Apply and Remove Shelf (Pop)" vc-bzr-shelve-apply-at-point
		  :help "Apply the current shelf and remove it"))
    (define-key map [sh]
      '(menu-item "Show Shelve" vc-bzr-shelve-show-at-point
    		  :help "Show the contents of the current shelve"))
    map))

(defvar vc-bzr-extra-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [bzr-sn]
      '(menu-item "Shelve a Snapshot" vc-bzr-shelve-snapshot
		  :help "Shelve the current state of the tree and keep the current state"))
    (define-key map [bzr-sh]
      '(menu-item "Shelve..." vc-bzr-shelve
		  :help "Shelve changes"))
    map))

(defun vc-bzr-extra-menu () vc-bzr-extra-menu-map)

(defun vc-bzr-extra-status-menu () vc-bzr-extra-menu-map)

(defun vc-bzr-dir-extra-headers (dir)
  (let*
      ((str (with-temp-buffer
	      (vc-bzr-command "info" t 0 dir)
	      (buffer-string)))
       (shelve (vc-bzr-shelve-list))
       (shelve-help-echo "Use M-x vc-bzr-shelve to create shelves")
       (root-dir (vc-bzr-root dir))
       (pending-merge
	;; FIXME: looking for .bzr/checkout/merge-hashes is not a
	;; reliable method to detect pending merges, disable this
	;; until a proper solution is implemented.
	(and nil
	 (file-exists-p
	 (expand-file-name ".bzr/checkout/merge-hashes" root-dir))))
       (pending-merge-help-echo
	(format "A merge has been performed.\nA commit from the top-level directory (%s)\nis required before being able to check in anything else" root-dir))
       (light-checkout
	(when (string-match ".+light checkout root: \\(.+\\)$" str)
	  (match-string 1 str)))
       (light-checkout-branch
	(when light-checkout
	  (when (string-match ".+checkout of branch: \\(.+\\)$" str)
	    (match-string 1 str)))))
    (concat
     (propertize "Parent branch      : " 'face 'font-lock-type-face)
     (propertize
      (if (string-match "parent branch: \\(.+\\)$" str)
 	  (match-string 1 str)
 	"None")
       'face 'font-lock-variable-name-face)
     "\n"
      (when light-checkout
	(concat
	 (propertize "Light checkout root: " 'face 'font-lock-type-face)
	 (propertize light-checkout 'face 'font-lock-variable-name-face)
	 "\n"))
      (when light-checkout-branch
	(concat
	 (propertize "Checkout of branch : " 'face 'font-lock-type-face)
	 (propertize light-checkout-branch 'face 'font-lock-variable-name-face)
	 "\n"))
      (when pending-merge
	(concat
	 (propertize "Warning            : " 'face 'font-lock-warning-face
		     'help-echo pending-merge-help-echo)
	 (propertize "Pending merges, commit recommended before any other action"
		     'help-echo pending-merge-help-echo
		     'face 'font-lock-warning-face)
	 "\n"))
      (if shelve
	  (concat
	   (propertize "Shelves            :\n" 'face 'font-lock-type-face
		       'help-echo shelve-help-echo)
	   (mapconcat
	    (lambda (x)
	      (propertize x
			  'face 'font-lock-variable-name-face
			  'mouse-face 'highlight
			  'help-echo "mouse-3: Show shelve menu\nA: Apply and keep shelf\nP: Apply and remove shelf (pop)\nS: Snapshot to a shelf\nC-k: Delete shelf"
			  'keymap vc-bzr-shelve-map))
	    shelve "\n"))
	(concat
	 (propertize "Shelves            : " 'face 'font-lock-type-face
		     'help-echo shelve-help-echo)
	 (propertize "No shelved changes"
		     'help-echo shelve-help-echo
		     'face 'font-lock-variable-name-face))))))

;; Follows vc-bzr-command, which uses vc-do-command from vc-dispatcher.
(declare-function vc-resynch-buffer "vc-dispatcher"
                  (file &optional keep noquery reset-vc-info))

(defun vc-bzr-shelve (name)
  "Shelve the changes of the selected files."
  (interactive "sShelf name: ")
  (let ((root (vc-bzr-root default-directory))
        (fileset (vc-deduce-fileset)))
    (when root
      (vc-bzr-command "shelve" nil 0 (nth 1 fileset) "--all" "-m" name)
      (vc-resynch-buffer root t t))))

(defun vc-bzr-shelve-show (name)
  "Show the contents of shelve NAME."
  (interactive "sShelve name: ")
  (vc-setup-buffer "*vc-diff*")
  ;; FIXME: how can you show the contents of a shelf?
  (vc-bzr-command "unshelve" "*vc-diff*" 'async nil "--preview" name)
  (set-buffer "*vc-diff*")
  (diff-mode)
  (setq buffer-read-only t)
  (pop-to-buffer (current-buffer)))

(defun vc-bzr-shelve-apply (name)
  "Apply shelve NAME and remove it afterwards."
  (interactive "sApply (and remove) shelf: ")
  (vc-bzr-command "unshelve" nil 0 nil "--apply" name)
  (vc-resynch-buffer (vc-bzr-root default-directory) t t))

(defun vc-bzr-shelve-apply-and-keep (name)
  "Apply shelve NAME and keep it afterwards."
  (interactive "sApply (and keep) shelf: ")
  (vc-bzr-command "unshelve" nil 0 nil "--apply" "--keep" name)
  (vc-resynch-buffer (vc-bzr-root default-directory) t t))

(defun vc-bzr-shelve-snapshot ()
  "Create a stash with the current tree state."
  (interactive)
  (vc-bzr-command "shelve" nil 0 nil "--all" "-m"
		  (format-time-string "Snapshot on %Y-%m-%d at %H:%M"))
  (vc-bzr-command "unshelve" nil 0 nil "--apply" "--keep")
  (vc-resynch-buffer (vc-bzr-root default-directory) t t))

(defun vc-bzr-shelve-list ()
  (with-temp-buffer
    (vc-bzr-command "shelve" (current-buffer) 1 nil "--list" "-q")
    (delete
     ""
     (split-string
      (buffer-substring (point-min) (point-max))
      "\n"))))

(defun vc-bzr-shelve-get-at-point (point)
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (if (looking-at "^ +\\([0-9]+\\):")
	(match-string 1)
      (error "Cannot find shelf at point"))))

;; vc-bzr-shelve-delete-at-point must be called from a vc-dir buffer.
(declare-function vc-dir-refresh "vc-dir" ())

(defun vc-bzr-shelve-delete-at-point ()
  (interactive)
  (let ((shelve (vc-bzr-shelve-get-at-point (point))))
    (when (y-or-n-p (format "Remove shelf %s ? " shelve))
      (vc-bzr-command "unshelve" nil 0 nil "--delete-only" shelve)
      (vc-dir-refresh))))

(defun vc-bzr-shelve-show-at-point ()
  (interactive)
  (vc-bzr-shelve-show (vc-bzr-shelve-get-at-point (point))))

(defun vc-bzr-shelve-apply-at-point ()
  (interactive)
  (vc-bzr-shelve-apply (vc-bzr-shelve-get-at-point (point))))

(defun vc-bzr-shelve-apply-and-keep-at-point ()
  (interactive)
  (vc-bzr-shelve-apply-and-keep (vc-bzr-shelve-get-at-point (point))))

(defun vc-bzr-shelve-menu (e)
  (interactive "e")
  (vc-dir-at-event e (popup-menu vc-bzr-shelve-menu-map e)))

(defun vc-bzr-revision-table (files)
  (let ((vc-bzr-revisions '())
        (default-directory (file-name-directory (car files))))
    (with-temp-buffer
      (apply 'vc-bzr-command "log" t 0 files
             (append '("--line")
                     (if (stringp vc-bzr-log-switches)
                         (list vc-bzr-log-switches)
                       vc-bzr-log-switches)))
      (let ((start (point-min))
            (loglines (buffer-substring-no-properties (point-min) (point-max))))
        (while (string-match "^\\([0-9]+\\):" loglines)
          (push (match-string 1 loglines) vc-bzr-revisions)
          (setq start (+ start (match-end 0)))
          (setq loglines (buffer-substring-no-properties start (point-max))))))
    vc-bzr-revisions))

(defun vc-bzr-conflicted-files (dir)
  (let ((default-directory (vc-bzr-root dir))
        (files ()))
    (with-temp-buffer
      (vc-bzr-command "status" t 0 default-directory)
      (goto-char (point-min))
      (when (re-search-forward "^conflicts:\n" nil t)
        (while (looking-at "  \\(?:Text conflict in \\(.*\\)\\|.*\\)\n")
          (if (match-end 1)
              (push (expand-file-name (match-string 1)) files))
          (goto-char (match-end 0)))))
    files))

;;; Revision completion

(eval-and-compile
  (defconst vc-bzr-revision-keywords
    ;; bzr help revisionspec  | sed -ne 's/^\([a-z]*\):$/"\1"/p' | sort -u
    '("ancestor" "annotate" "before" "branch" "date" "last" "mainline" "revid"
      "revno" "submit" "tag")))

(defun vc-bzr-revision-completion-table (files)
  ;; What about using `files'?!?  --Stef
  (lambda (string pred action)
    (cond
     ((string-match "\\`\\(ancestor\\|branch\\|\\(revno:\\)?[-0-9]+:\\):"
                    string)
      (completion-table-with-context (substring string 0 (match-end 0))
                                     (apply-partially
                                      'completion-table-with-predicate
                                      'completion-file-name-table
                                      'file-directory-p t)
                                     (substring string (match-end 0))
                                     pred
                                     action))
     ((string-match "\\`\\(before\\):" string)
      (completion-table-with-context (substring string 0 (match-end 0))
                                     (vc-bzr-revision-completion-table files)
                                     (substring string (match-end 0))
                                     pred
                                     action))
     ((string-match "\\`\\(tag\\):" string)
      (let ((prefix (substring string 0 (match-end 0)))
            (tag (substring string (match-end 0)))
            (table nil)
            process-file-side-effects)
        (with-temp-buffer
          ;; "bzr-1.2 tags" is much faster with --show-ids.
          (process-file vc-bzr-program nil '(t) nil "tags" "--show-ids")
          ;; The output is ambiguous, unless we assume that revids do not
          ;; contain spaces.
          (goto-char (point-min))
          (while (re-search-forward "^\\(.*[^ \n]\\) +[^ \n]*$" nil t)
            (push (match-string-no-properties 1) table)))
        (completion-table-with-context prefix table tag pred action)))

     ((string-match "\\`annotate:" string)
      (completion-table-with-context
       (substring string 0 (match-end 0))
       (apply-partially #'completion-table-with-terminator
                        (cons ":" regexp-unmatchable)
                        #'completion-file-name-table)
       (substring string (match-end 0)) pred action))

     ((string-match "\\`date:" string)
      (completion-table-with-context
       (substring string 0 (match-end 0))
       '("yesterday" "today" "tomorrow")
       (substring string (match-end 0)) pred action))

     ((string-match "\\`\\([a-z]+\\):" string)
      ;; no actual completion for the remaining keywords.
      (completion-table-with-context (substring string 0 (match-end 0))
                                     (if (member (match-string 1 string)
                                                 vc-bzr-revision-keywords)
                                         ;; If it's a valid keyword,
                                         ;; use a non-empty table to
                                         ;; indicate it.
                                         '("") nil)
                                     (substring string (match-end 0))
                                     pred
                                     action))
     (t
      ;; Could use completion-table-with-terminator, except that it
      ;; currently doesn't work right w.r.t pcm and doesn't give
      ;; the *Completions* output we want.
      (complete-with-action action (eval-when-compile
                                     (mapcar (lambda (s) (concat s ":"))
                                             vc-bzr-revision-keywords))
                            string pred)))))

(defun vc-bzr-repository-url (file-or-dir &optional _remote-name)
  (let ((default-directory (vc-bzr-root file-or-dir)))
    (with-temp-buffer
      (vc-bzr-command "info" (current-buffer) 0 nil)
      (goto-char (point-min))
      (if (re-search-forward "parent branch: \\(.*\\)$" nil t)
          (match-string 1)
        (error "Cannot determine Bzr repository URL")))))

(provide 'vc-bzr)

;;; vc-bzr.el ends here
