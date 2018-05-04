;;; vc-cvs.el --- non-resident support for CVS version-control  -*- lexical-binding: t -*-

;; Copyright (C) 1995, 1998-2018 Free Software Foundation, Inc.

;; Author:      FSF (see vc.el for full credits)
;; Maintainer:  emacs-devel@gnu.org
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

;;; Code:

(eval-when-compile (require 'vc))

(declare-function vc-branch-p "vc" (rev))
(declare-function vc-checkout "vc" (file &optional rev))
(declare-function vc-expand-dirs "vc" (file-or-dir-list backend))
(declare-function vc-read-revision "vc"
                  (prompt &optional files backend default initial-input))

;; Clear up the cache to force vc-call to check again and discover
;; new functions when we reload this file.
(put 'CVS 'vc-functions nil)

;;; Properties of the backend.

(defun vc-cvs-revision-granularity () 'file)

(defun vc-cvs-checkout-model (files)
  "CVS-specific version of `vc-checkout-model'."
  (if (getenv "CVSREAD")
      'announce
    (let* ((file (if (consp files) (car files) files))
           (attrib (file-attributes file)))
      (or (vc-file-getprop file 'vc-checkout-model)
          (vc-file-setprop
           file 'vc-checkout-model
           (if (and attrib ;; don't check further if FILE doesn't exist
                    ;; If the file is not writable (despite CVSREAD being
                    ;; undefined), this is probably because the file is being
                    ;; "watched" by other developers.
                    ;; (We actually shouldn't trust this, but there is
                    ;; no other way to learn this from CVS at the
                    ;; moment (version 1.9).)
                    (string-match "r-..-..-." (nth 8 attrib)))
               'announce
             'implicit))))))

;;;
;;; Customization options
;;;

(defgroup vc-cvs nil
  "VC CVS backend."
  :version "24.1"
  :group 'vc)

(defcustom vc-cvs-global-switches nil
  "Global switches to pass to any CVS command."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "22.1"
  :group 'vc-cvs)

(defcustom vc-cvs-register-switches nil
  "Switches for registering a file into CVS.
A string or list of strings passed to the checkin program by
\\[vc-register].  If nil, use the value of `vc-register-switches'.
If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "21.1"
  :group 'vc-cvs)

(defcustom vc-cvs-diff-switches nil
  "String or list of strings specifying switches for CVS diff under VC.
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
                 (const :tag "None" t)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string))
  :version "21.1"
  :group 'vc-cvs)

(defcustom vc-cvs-annotate-switches nil
  "String or list of strings specifying switches for cvs annotate under VC.
If nil, use the value of `vc-annotate-switches'.  If t, use no
switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "25.1"
  :group 'vc-cvs)

(defcustom vc-cvs-header '("$Id\ $")
  "Header keywords to be inserted by `vc-insert-headers'."
  :version "24.1"     ; no longer consult the obsolete vc-header-alist
  :type '(repeat string)
  :group 'vc-cvs)

(defcustom vc-cvs-use-edit t
  "Non-nil means to use `cvs edit' to \"check out\" a file.
This is only meaningful if you don't use the implicit checkout model
\(i.e. if you have $CVSREAD set)."
  :type 'boolean
  :version "21.1"
  :group 'vc-cvs)

(defcustom vc-cvs-stay-local 'only-file
  "Non-nil means use local operations when possible for remote repositories.
This avoids slow queries over the network and instead uses heuristics
and past information to determine the current status of a file.

If value is the symbol `only-file', `vc-dir' will connect to the
server, but heuristics will be used to determine the status for
all other VC operations.

The value can also be a regular expression or list of regular
expressions to match against the host name of a repository; then
vc-cvs only stays local for hosts that match it.  Alternatively,
the value can be a list of regular expressions where the first
element is the symbol `except'; then vc-cvs always stays local
except for hosts matched by these regular expressions."
  :type '(choice (const :tag "Always stay local" t)
		 (const :tag "Only for file operations" only-file)
		 (const :tag "Don't stay local" nil)
                 (list :format "\nExamine hostname and %v"
                       :tag "Examine hostname ..."
                       (set :format "%v" :inline t
                            (const :format "%t" :tag "don't" except))
                       (regexp :format " stay local,\n%t: %v"
                               :tag "if it matches")
                       (repeat :format "%v%i\n" :inline t (regexp :tag "or"))))
  :version "23.1"
  :group 'vc-cvs)

(defcustom vc-cvs-sticky-date-format-string "%c"
  "Format string for mode-line display of sticky date.
Format is according to `format-time-string'.  Only used if
`vc-cvs-sticky-tag-display' is t."
  :type '(string)
  :version "22.1"
  :group 'vc-cvs)

(defcustom vc-cvs-sticky-tag-display t
  "Specify the mode-line display of sticky tags.
Value t means default display, nil means no display at all.  If the
value is a function or macro, it is called with the sticky tag and
its type as parameters, in that order.  TYPE can have three different
values: `symbolic-name' (TAG is a string), `revision-number' (TAG is a
string) and `date' (TAG is a date as returned by `encode-time').  The
return value of the function or macro will be displayed as a string.

Here's an example that will display the formatted date for sticky
dates and the word \"Sticky\" for sticky tag names and revisions.

  (lambda (tag type)
    (cond ((eq type \\='date) (format-time-string
                              vc-cvs-sticky-date-format-string tag))
          ((eq type \\='revision-number) \"Sticky\")
          ((eq type \\='symbolic-name) \"Sticky\")))

Here's an example that will abbreviate to the first character only,
any text before the first occurrence of `-' for sticky symbolic tags.
If the sticky tag is a revision number, the word \"Sticky\" is
displayed.  Date and time is displayed for sticky dates.

   (lambda (tag type)
     (cond ((eq type \\='date) (format-time-string \"%Y%m%d %H:%M\" tag))
           ((eq type \\='revision-number) \"Sticky\")
           ((eq type \\='symbolic-name)
            (condition-case nil
                (progn
                  (string-match \"\\\\([^-]*\\\\)\\\\(.*\\\\)\" tag)
                  (concat (substring (match-string 1 tag) 0 1) \":\"
                          (substring (match-string 2 tag) 1 nil)))
              (error tag)))))       ; Fall-back to given tag name.

See also variable `vc-cvs-sticky-date-format-string'."
  :type '(choice boolean function)
  :version "22.1"
  :group 'vc-cvs)

;;;
;;; Internal variables
;;;


;;;
;;; State-querying functions
;;;

;;;###autoload(defun vc-cvs-registered (f)
;;;###autoload   "Return non-nil if file F is registered with CVS."
;;;###autoload   (when (file-readable-p (expand-file-name
;;;###autoload 			  "CVS/Entries" (file-name-directory f)))
;;;###autoload       (load "vc-cvs" nil t)
;;;###autoload       (vc-cvs-registered f)))

(defun vc-cvs-registered (file)
  "Check if FILE is CVS registered."
  (let ((dirname (or (file-name-directory file) ""))
	(basename (file-name-nondirectory file))
        ;; make sure that the file name is searched case-sensitively
        (case-fold-search nil))
    (if (file-readable-p (expand-file-name "CVS/Entries" dirname))
        (or (string= basename "")
            (with-temp-buffer
              (vc-cvs-get-entries dirname)
              (goto-char (point-min))
              (cond ((re-search-forward
                      (concat "^/" (regexp-quote basename) "/[^/]") nil t)
                     (beginning-of-line)
                     (vc-cvs-parse-entry file)
                     t)
                    (t nil))))
      nil)))

(defun vc-cvs-state (file)
  "CVS-specific version of `vc-state'."
  (if (vc-cvs-stay-local-p file)
      (let ((state (vc-file-getprop file 'vc-state)))
        ;; If we should stay local, use the heuristic but only if
        ;; we don't have a more precise state already available.
	(if (memq state '(up-to-date edited nil))
	    (vc-cvs-state-heuristic file)
	  state))
    (with-temp-buffer
      (cd (file-name-directory file))
      (let (process-file-side-effects)
	(vc-cvs-command t 0 file "status"))
      (vc-cvs-parse-status t))))

(defun vc-cvs-state-heuristic (file)
  "CVS-specific state heuristic."
  ;; If the file has not changed since checkout, consider it `up-to-date'.
  ;; Otherwise consider it `edited'.
  (let ((checkout-time (vc-file-getprop file 'vc-checkout-time))
        (lastmod (nth 5 (file-attributes file))))
    (cond
     ((equal checkout-time lastmod) 'up-to-date)
     ((string= (vc-working-revision file) "0") 'added)
     ((null checkout-time) 'unregistered)
     (t 'edited))))

(defun vc-cvs-working-revision (file)
  "CVS-specific version of `vc-working-revision'."
  ;; There is no need to consult RCS headers under CVS, because we
  ;; get the workfile version for free when we recognize that a file
  ;; is registered in CVS.
  (vc-cvs-registered file)
  (vc-file-getprop file 'vc-working-revision))

(defun vc-cvs-mode-line-string (file)
  "Return a string for `vc-mode-line' to put in the mode line for FILE.
Compared to the default implementation, this function does two things:
Handle the special case of a CVS file that is added but not yet
committed and support display of sticky tags."
  (let* ((sticky-tag (vc-file-getprop file 'vc-cvs-sticky-tag))
	 help-echo
	 (string
          (let ((def-ml (vc-default-mode-line-string 'CVS file)))
            (setq help-echo
                  (get-text-property 0 'help-echo def-ml))
            def-ml)))
    (propertize
     (if (zerop (length sticky-tag))
	 string
       (setq help-echo (format-message "%s on the `%s' branch"
                                       help-echo sticky-tag))
       (concat string "[" sticky-tag "]"))
     'help-echo help-echo)))


;;;
;;; State-changing functions
;;;

(autoload 'vc-switches "vc")

(defun vc-cvs-register (files &optional comment)
  "Register FILES into the CVS version-control system.
COMMENT can be used to provide an initial description of FILES.
Passes either `vc-cvs-register-switches' or `vc-register-switches'
to the CVS command."
  ;; Register the directories if needed.
  (let (dirs)
    (dolist (file files)
      (and (not (vc-cvs-responsible-p file))
           (vc-cvs-could-register file)
           (push (directory-file-name (file-name-directory file)) dirs)))
    (if dirs (vc-cvs-register dirs)))
  (apply 'vc-cvs-command nil 0 files
         "add"
         (and comment (string-match "[^\t\n ]" comment)
              (concat "-m" comment))
         (vc-switches 'CVS 'register)))

(defun vc-cvs-responsible-p (file)
  "Return non-nil if CVS thinks it is responsible for FILE."
  (file-directory-p (expand-file-name "CVS"
				      (if (file-directory-p file)
					  file
					(file-name-directory file)))))

(defun vc-cvs-could-register (file)
  "Return non-nil if FILE could be registered in CVS.
This is only possible if CVS is managing FILE's directory or one of
its parents."
  (let ((dir file))
    (while (and (stringp dir)
                (not (equal dir (setq dir (file-name-directory dir))))
                dir)
      (setq dir (if (file-exists-p
                     (expand-file-name "CVS/Entries" dir))
                    t
                  (directory-file-name dir))))
    (eq dir t)))

(defun vc-cvs-checkin (files comment &optional rev)
  "CVS-specific version of `vc-backend-checkin'."
 (unless (or (not rev) (vc-cvs-valid-revision-number-p rev))
   (if (not (vc-cvs-valid-symbolic-tag-name-p rev))
	(error "%s is not a valid symbolic tag name" rev)
     ;; If the input revision is a valid symbolic tag name, we create it
     ;; as a branch, commit and switch to it.
     (apply 'vc-cvs-command nil 0 files "tag" "-b" (list rev))
     (apply 'vc-cvs-command nil 0 files "update" "-r" (list rev))
     (mapc (lambda (file) (vc-file-setprop file 'vc-cvs-sticky-tag rev))
	    files)))
  (let ((status (apply 'vc-cvs-command nil 1 files
		       "ci" (if rev (concat "-r" rev))
                       (concat "-m" comment)
		       (vc-switches 'CVS 'checkin))))
    (set-buffer "*vc*")
    (goto-char (point-min))
    (when (not (zerop status))
      ;; Check checkin problem.
      (cond
       ((re-search-forward "Up-to-date check failed" nil t)
	(mapc (lambda (file) (vc-file-setprop file 'vc-state 'needs-merge))
	      files)
        (error "%s" (substitute-command-keys
                (concat "Up-to-date check failed: "
                        "type \\[vc-next-action] to merge in changes"))))
       (t
        (pop-to-buffer (current-buffer))
        (goto-char (point-min))
        (shrink-window-if-larger-than-buffer)
        (error "Check-in failed"))))
    ;; Single-file commit?  Then update the revision by parsing the buffer.
    ;; Otherwise we can't necessarily tell what goes with what; clear
    ;; its properties so they have to be refetched.
    (if (= (length files) 1)
	(vc-file-setprop
	 (car files) 'vc-working-revision
	 (vc-parse-buffer "^\\(new\\|initial\\) revision: \\([0-9.]+\\)" 2))
      (mapc 'vc-file-clearprops files))
    ;; Anyway, forget the checkout model of the file, because we might have
    ;; guessed wrong when we found the file.  After commit, we can
    ;; tell it from the permissions of the file (see
    ;; vc-cvs-checkout-model).
    (mapc (lambda (file) (vc-file-setprop file 'vc-checkout-model nil))
	  files)
    ;; if this was an explicit check-in (does not include creation of
    ;; a branch), remove the sticky tag.
    (if (and rev (not (vc-cvs-valid-symbolic-tag-name-p rev)))
	(vc-cvs-command nil 0 files "update" "-A"))))

(defun vc-cvs-find-revision (file rev buffer)
  (apply 'vc-cvs-command
	 buffer 0 file
	 "-Q"				; suppress diagnostic output
	 "update"
	 (and rev (not (string= rev ""))
	      (concat "-r" rev))
	 "-p"
	 (vc-switches 'CVS 'checkout)))

(defun vc-cvs-checkout (file &optional rev)
  "Checkout a revision of FILE into the working area.
REV is the revision to check out."
  (message "Checking out %s..." file)
  ;; Change buffers to get local value of vc-checkout-switches.
  (with-current-buffer (or (get-file-buffer file) (current-buffer))
    (if (and (file-exists-p file) (not rev))
        ;; If no revision was specified, just make the file writable
        ;; if necessary (using `cvs-edit' if requested).
        (and (not (eq (vc-cvs-checkout-model (list file)) 'implicit))
             (if vc-cvs-use-edit
                 (vc-cvs-command nil 0 file "edit")
               (set-file-modes file (logior (file-modes file) 128))
               (if (equal file buffer-file-name) (read-only-mode -1))))
      ;; Check out a particular revision (or recreate the file).
      (vc-file-setprop file 'vc-working-revision nil)
      (apply 'vc-cvs-command nil 0 file
             "-w"
             "update"
             (when rev
               (unless (eq rev t)
                 ;; default for verbose checkout: clear the
                 ;; sticky tag so that the actual update will
                 ;; get the head of the trunk
                 (if (string= rev "")
                     "-A"
                   (concat "-r" rev))))
             (vc-switches 'CVS 'checkout)))
    (vc-mode-line file 'CVS))
  (message "Checking out %s...done" file))

(defun vc-cvs-delete-file (file)
  (vc-cvs-command nil 0 file "remove" "-f"))

(autoload 'vc-default-revert "vc")

(defun vc-cvs-revert (file &optional contents-done)
  "Revert FILE to the working revision on which it was based."
  (vc-default-revert 'CVS file contents-done)
  (unless (eq (vc-cvs-checkout-model (list file)) 'implicit)
    (if vc-cvs-use-edit
        (vc-cvs-command nil 0 file "unedit")
      ;; Make the file read-only by switching off all w-bits
      (set-file-modes file (logand (file-modes file) 3950)))))

(defun vc-cvs-merge-file (file)
  "Accept a file merge request, prompting for revisions."
  (let* ((first-revision
        (vc-read-revision
         (concat "Merge " file
                 " from branch or revision "
                 "(default news on current branch): ")
         (list file)
         'CVS))
        second-revision
        status)
    (cond
     ((string= first-revision "")
      (setq status (vc-cvs-merge-news file)))
     (t
      (if (not (vc-branch-p first-revision))
         (setq second-revision
               (vc-read-revision
                "Second revision: "
                (list file) 'CVS nil
                (concat (vc-branch-part first-revision) ".")))
       ;; We want to merge an entire branch.  Set revisions
       ;; accordingly, so that vc-cvs-merge understands us.
       (setq second-revision first-revision)
       ;; first-revision must be the starting point of the branch
       (setq first-revision (vc-branch-part first-revision)))
      (setq status (vc-cvs-merge file first-revision second-revision))))
    status))

(defun vc-cvs-merge (file first-revision &optional second-revision)
  "Merge changes into current working copy of FILE.
The changes are between FIRST-REVISION and SECOND-REVISION."
  (vc-cvs-command nil 0 file
                 "update" "-kk"
                 (concat "-j" first-revision)
                 (concat "-j" second-revision))
  (vc-file-setprop file 'vc-state 'edited)
  (with-current-buffer (get-buffer "*vc*")
    (goto-char (point-min))
    (if (re-search-forward "conflicts during merge" nil t)
	(progn
	  (vc-file-setprop file 'vc-state 'conflict)
	  ;; signal error
	  1)
      (vc-file-setprop file 'vc-state 'edited)
      ;; signal success
      0)))

(defun vc-cvs-merge-news (file)
  "Merge in any new changes made to FILE."
  (message "Merging changes into %s..." file)
  ;; (vc-file-setprop file 'vc-working-revision nil)
  (vc-file-setprop file 'vc-checkout-time 0)
  (vc-cvs-command nil nil file "update")
  ;; Analyze the merge result reported by CVS, and set
  ;; file properties accordingly.
  (with-current-buffer (get-buffer "*vc*")
    (goto-char (point-min))
    ;; get new working revision
    (if (re-search-forward
	 "^Merging differences between [0-9.]* and \\([0-9.]*\\) into" nil t)
	(vc-file-setprop file 'vc-working-revision (match-string 1))
      (vc-file-setprop file 'vc-working-revision nil))
    ;; get file status
    (prog1
        (if (eq (buffer-size) 0)
            0 ;; there were no news; indicate success
          (if (re-search-forward
               (concat "^\\([CMUP] \\)?"
                       (regexp-quote
                        (substring file (1+ (length (expand-file-name
                                                     "." default-directory)))))
                       "\\( already contains the differences between \\)?")
               nil t)
              (cond
               ;; Merge successful, we are in sync with repository now
               ((or (match-string 2)
                    (string= (match-string 1) "U ")
                    (string= (match-string 1) "P "))
                (vc-file-setprop file 'vc-state 'up-to-date)
                (vc-file-setprop file 'vc-checkout-time
                                 (nth 5 (file-attributes file)))
                0);; indicate success to the caller
               ;; Merge successful, but our own changes are still in the file
               ((string= (match-string 1) "M ")
                (vc-file-setprop file 'vc-state 'edited)
                0);; indicate success to the caller
               ;; Conflicts detected!
               (t
                (vc-file-setprop file 'vc-state 'conflict)
                1);; signal the error to the caller
               )
            (pop-to-buffer "*vc*")
            (error "Couldn't analyze cvs update result")))
      (message "Merging changes into %s...done" file))))

(defun vc-cvs-modify-change-comment (files rev comment)
  "Modify the change comments for FILES on a specified REV.
Will fail unless you have administrative privileges on the repo."
  (vc-cvs-command nil 0 files "admin" (concat "-m" rev ":" comment)))

;;;
;;; History functions
;;;

(declare-function vc-rcs-print-log-cleanup "vc-rcs" ())
;; Follows vc-cvs-command, which uses vc-do-command from vc-dispatcher.
(declare-function vc-exec-after "vc-dispatcher" (code))

(defun vc-cvs-print-log (files buffer &optional _shortlog _start-revision limit)
  "Print commit log associated with FILES into specified BUFFER.
Remaining arguments are ignored."
  (require 'vc-rcs)
  ;; It's just the catenation of the individual logs.
  (vc-cvs-command
   buffer
   (if (vc-cvs-stay-local-p files) 'async 0)
   files "log")
  (with-current-buffer buffer
    (vc-run-delayed (vc-rcs-print-log-cleanup)))
  (when limit 'limit-unsupported))

(defun vc-cvs-comment-history (file)
  "Get comment history of a file."
  (vc-call-backend 'RCS 'comment-history file))

(autoload 'vc-version-backup-file "vc")
(declare-function vc-coding-system-for-diff "vc" (file))

(defun vc-cvs-diff (files &optional oldvers newvers buffer async)
  "Get a difference report using CVS between two revisions of FILE."
  (let* (process-file-side-effects
	 (async (and async (vc-cvs-stay-local-p files)))
	 (invoke-cvs-diff-list nil)
	 status)
    ;; Look through the file list and see if any files have backups
    ;; that can be used to do a plain "diff" instead of "cvs diff".
    (dolist (file files)
      (let ((ov oldvers)
	    (nv newvers))
	(when (or (not ov) (string-equal ov ""))
	  (setq ov (vc-working-revision file)))
	(when (string-equal nv "")
	  (setq nv nil))
	(let ((file-oldvers (vc-version-backup-file file ov))
	      (file-newvers (if (not nv)
				file
			      (vc-version-backup-file file nv)))
	      (coding-system-for-read (vc-coding-system-for-diff file)))
	  (if (and file-oldvers file-newvers)
	      (progn
		;; This used to append diff-switches and vc-diff-switches,
		;; which was consistent with the vc-diff-switches doc at that
		;; time, but not with the actual behavior of any other VC diff.
		(apply 'vc-do-command (or buffer "*vc-diff*") 1 "diff" nil
		       ;; Not a CVS diff, does not use vc-cvs-diff-switches.
		       (append (vc-switches nil 'diff)
			       (list (file-relative-name file-oldvers)
				     (file-relative-name file-newvers))))
		(setq status 0))
	    (push file invoke-cvs-diff-list)))))
    (when invoke-cvs-diff-list
      (setq status (apply 'vc-cvs-command (or buffer "*vc-diff*")
			  (if async 'async 1)
			  invoke-cvs-diff-list "diff"
			  (and oldvers (concat "-r" oldvers))
			  (and newvers (concat "-r" newvers))
			  (vc-switches 'CVS 'diff))))
    (if async 1 status))) ; async diff, pessimistic assumption

(defconst vc-cvs-annotate-first-line-re "^[0-9]")

(defun vc-cvs-annotate-process-filter (filter process string)
  (setq string (concat (process-get process 'output) string))
  (if (not (string-match vc-cvs-annotate-first-line-re string))
      ;; Still waiting for the first real line.
      (process-put process 'output string)
    (remove-function (process-filter process) #'vc-cvs-annotate-process-filter)
    (funcall filter process (substring string (match-beginning 0)))))

(defun vc-cvs-annotate-command (file buffer &optional revision)
  "Execute \"cvs annotate\" on FILE, inserting the contents in BUFFER.
Optional arg REVISION is a revision to annotate from."
  (apply #'vc-cvs-command buffer
	 (if (vc-cvs-stay-local-p file)
	     'async 0)
	 file "annotate"
	 (append (vc-switches 'cvs 'annotate)
		 (if revision (list (concat "-r" revision)))))
  ;; Strip the leading few lines.
  (let ((proc (get-buffer-process buffer)))
    (if proc
        ;; If running asynchronously, use a process filter.
        (add-function :around (process-filter proc)
                      #'vc-cvs-annotate-process-filter)
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward vc-cvs-annotate-first-line-re)
        (delete-region (point-min) (1- (point)))))))

(declare-function vc-annotate-convert-time "vc-annotate" (&optional time))

(defun vc-cvs-annotate-current-time ()
  "Return the current time, based at midnight of the current day, and
encoded as fractional days."
  (vc-annotate-convert-time
   (apply 'encode-time 0 0 0 (nthcdr 3 (decode-time)))))

(defun vc-cvs-annotate-time ()
  "Return the time of the next annotation (as fraction of days)
systime, or nil if there is none."
  (let* ((bol (point))
         (cache (get-text-property bol 'vc-cvs-annotate-time))
         (inhibit-read-only t)
         (inhibit-modification-hooks t))
    (cond
     (cache)
     ((looking-at
       "^\\S-+\\s-+\\S-+\\s-+\\([0-9]+\\)-\\(\\sw+\\)-\\([0-9]+\\)): ")
      (let ((day (string-to-number (match-string 1)))
            (month (cdr (assq (intern (match-string 2))
                              '((Jan .  1) (Feb .  2) (Mar .  3)
                                (Apr .  4) (May .  5) (Jun .  6)
                                (Jul .  7) (Aug .  8) (Sep .  9)
                                (Oct . 10) (Nov . 11) (Dec . 12)))))
            (year (let ((tmp (string-to-number (match-string 3))))
                    ;; Years 0..68 are 2000..2068.
                    ;; Years 69..99 are 1969..1999.
                    (+ (cond ((> 69 tmp) 2000)
                             ((> 100 tmp) 1900)
                             (t 0))
                       tmp))))
        (put-text-property
         bol (1+ bol) 'vc-cvs-annotate-time
         (setq cache (cons
                      ;; Position at end makes for nicer overlay result.
                      ;; Don't put actual buffer pos here, but only relative
                      ;; distance, so we don't ever move backward in the
                      ;; goto-char below, even if the text is moved.
                      (- (match-end 0) (match-beginning 0))
                      (vc-annotate-convert-time
                       (encode-time 0 0 0 day month year))))))))
    (when cache
      (goto-char (+ bol (car cache)))   ; Fontify from here to eol.
      (cdr cache))))                    ; days (float)

(defun vc-cvs-annotate-extract-revision-at-line ()
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "^\\([0-9]+\\.[0-9]+\\(\\.[0-9]+\\)*\\) +("
			   (line-end-position) t)
	(match-string-no-properties 1)
      nil)))

(defun vc-cvs-previous-revision (file rev)
  (vc-call-backend 'RCS 'previous-revision file rev))

(defun vc-cvs-next-revision (file rev)
  (vc-call-backend 'RCS 'next-revision file rev))

;; FIXME: This should probably be replaced by code using cvs2cl.
(defun vc-cvs-update-changelog (files)
  (vc-call-backend 'RCS 'update-changelog files))

;;;
;;; Tag system
;;;

(defun vc-cvs-create-tag (dir name branchp)
  "Assign to DIR's current revision a given NAME.
If BRANCHP is non-nil, the name is created as a branch (and the current
workspace is immediately moved to that new branch)."
  (vc-cvs-command nil 0 dir "tag" "-c" (if branchp "-b") name)
  (when branchp (vc-cvs-command nil 0 dir "update" "-r" name)))

;; Follows vc-cvs-command, which uses vc-do-command from vc-dispatcher.
(declare-function vc-resynch-buffer "vc-dispatcher"
                  (file &optional keep noquery reset-vc-info))

(defun vc-cvs-retrieve-tag (dir name update)
  "Retrieve a tag at and below DIR.
NAME is the name of the tag; if it is empty, do a `cvs update'.
If UPDATE is non-nil, then update (resynch) any affected buffers."
  (with-current-buffer (get-buffer-create "*vc*")
    (let ((default-directory dir)
	  (sticky-tag))
      (erase-buffer)
      (if (or (not name) (string= name ""))
	  (vc-cvs-command t 0 nil "update")
	(vc-cvs-command t 0 nil "update" "-r" name)
	(setq sticky-tag name))
      (when update
	(goto-char (point-min))
	(while (not (eobp))
	  (if (looking-at "\\([CMUP]\\) \\(.*\\)")
	      (let* ((file (expand-file-name (match-string 2) dir))
		     (state (match-string 1))
		     (buffer (find-buffer-visiting file)))
		(when buffer
		  (cond
		   ((or (string= state "U")
			(string= state "P"))
		    (vc-file-setprop file 'vc-state 'up-to-date)
		    (vc-file-setprop file 'vc-working-revision nil)
		    (vc-file-setprop file 'vc-checkout-time
				     (nth 5 (file-attributes file))))
		   ((or (string= state "M")
			(string= state "C"))
		    (vc-file-setprop file 'vc-state 'edited)
		    (vc-file-setprop file 'vc-working-revision nil)
		    (vc-file-setprop file 'vc-checkout-time 0)))
		  (vc-file-setprop file 'vc-cvs-sticky-tag sticky-tag)
		  (vc-resynch-buffer file t t))))
	  (forward-line 1))))))


;;;
;;; Miscellaneous
;;;

(defun vc-cvs-make-version-backups-p (file)
  "Return non-nil if version backups should be made for FILE."
  (vc-cvs-stay-local-p file))

(defun vc-cvs-check-headers ()
  "Check if the current file has any headers in it."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\\$[A-Za-z\300-\326\330-\366\370-\377]+\
\\(: [\t -#%-\176\240-\377]*\\)?\\$" nil t)))


;;;
;;; Internal functions
;;;

(defun vc-cvs-command (buffer okstatus files &rest flags)
  "A wrapper around `vc-do-command' for use in vc-cvs.el.
The difference to vc-do-command is that this function always invokes `cvs',
and that it passes `vc-cvs-global-switches' to it before FLAGS."
  (apply 'vc-do-command (or buffer "*vc*") okstatus "cvs" files
         (if (stringp vc-cvs-global-switches)
             (cons vc-cvs-global-switches flags)
           (append vc-cvs-global-switches
                   flags))))

(defun vc-cvs-stay-local-p (file)
  "Return non-nil if VC should stay local when handling FILE.
If FILE is a list of files, return non-nil if any of them
individually should stay local."
  (if (listp file)
      (delq nil (mapcar (lambda (arg) (vc-cvs-stay-local-p arg)) file))
    (let ((stay-local vc-cvs-stay-local))
      (if (symbolp stay-local) stay-local
       (let ((dirname (if (file-directory-p file)
                          (directory-file-name file)
                        (file-name-directory file))))
         (eq 'yes
             (or (vc-file-getprop dirname 'vc-cvs-stay-local-p)
                 (vc-file-setprop
                  dirname 'vc-cvs-stay-local-p
                  (let ((hostname (vc-cvs-repository-hostname dirname)))
                    (if (not hostname)
                        'no
                      (let ((default t))
                        (if (eq (car-safe stay-local) 'except)
                            (setq default nil stay-local (cdr stay-local)))
                        (when (consp stay-local)
                          (setq stay-local
                                (mapconcat 'identity stay-local "\\|")))
                        (if (if (string-match stay-local hostname)
                                default (not default))
                            'yes 'no))))))))))))

(defun vc-cvs-repository-hostname (dirname)
  "Hostname of the CVS server associated to workarea DIRNAME."
  (let ((rootname (expand-file-name "CVS/Root" dirname)))
    (when (file-readable-p rootname)
      (with-temp-buffer
	(let ((coding-system-for-read
	       (or file-name-coding-system
		   default-file-name-coding-system)))
	  (vc-insert-file rootname))
	(goto-char (point-min))
	(nth 2 (vc-cvs-parse-root
		(buffer-substring (point)
				  (line-end-position))))))))

(defun vc-cvs-parse-uhp (path)
  "parse user@host/path into (user@host /path)"
  (if (string-match "\\([^/]+\\)\\(/.*\\)" path)
      (list (match-string 1 path) (match-string 2 path))
      (list nil path)))

(defun vc-cvs-parse-root (root)
  "Split CVS ROOT specification string into a list of fields.
A CVS root specification of the form
  [:METHOD:][[USER@]HOSTNAME]:?/path/to/repository
is converted to a normalized record with the following structure:
  \(METHOD USER HOSTNAME CVS-ROOT).
The default METHOD for a CVS root of the form
  /path/to/repository
is `local'.
The default METHOD for a CVS root of the form
  [USER@]HOSTNAME:/path/to/repository
is `ext'.
For an empty string, nil is returned (invalid CVS root)."
  ;; Split CVS root into colon separated fields (0-4).
  ;; The `x:' makes sure, that leading colons are not lost;
  ;; `HOST:/PATH' is then different from `:METHOD:/PATH'.
  (let* ((root-list (cdr (split-string (concat "x:" root) ":")))
         (len (length root-list))
         ;; All syntactic varieties will get a proper METHOD.
         (root-list
          (cond
           ((= len 0)
            ;; Invalid CVS root
            nil)
           ((= len 1)
            (let ((uhp (vc-cvs-parse-uhp (car root-list))))
              (cons (if (car uhp) "ext" "local") uhp)))
           ((= len 2)
            ;; [USER@]HOST:PATH => method `ext'
            (and (not (equal (car root-list) ""))
                 (cons "ext" root-list)))
           ((= len 3)
            ;; :METHOD:PATH or :METHOD:USER@HOSTNAME/PATH
            (cons (cadr root-list)
                  (vc-cvs-parse-uhp (nth 2 root-list))))
           (t
            ;; :METHOD:[USER@]HOST:PATH
            (cdr root-list)))))
    (if root-list
        (let ((method (car root-list))
              (uhost (or (cadr root-list) ""))
              (root (nth 2 root-list))
              user host)
          ;; Split USER@HOST
          (if (string-match "\\(.*\\)@\\(.*\\)" uhost)
              (setq user (match-string 1 uhost)
                    host (match-string 2 uhost))
            (setq host uhost))
          ;; Remove empty HOST
          (and (equal host "")
               (setq host nil))
          ;; Fix windows style CVS root `:local:C:\\project\\cvs\\some\\dir'
          (and host
               (equal method "local")
               (setq root (concat host ":" root) host nil))
          ;; Normalize CVS root record
          (list method user host root)))))

;; XXX: This does not work correctly for subdirectories.  "cvs status"
;; information is context sensitive, it contains lines like:
;; cvs status: Examining DIRNAME
;; and the file entries after that don't show the full path.
;; Because of this VC directory listings only show changed files
;; at the top level for CVS.
(defun vc-cvs-parse-status (&optional full)
  "Parse output of \"cvs status\" command in the current buffer.
Set file properties accordingly.  Unless FULL is t, parse only
essential information. Note that this can never set the `ignored'
state."
  (let (file status missing)
    (goto-char (point-min))
    (while (looking-at "? \\(.*\\)")
      (setq file (expand-file-name (match-string 1)))
      (vc-file-setprop file 'vc-state 'unregistered)
      (forward-line 1))
    (when (re-search-forward "^File: " nil t)
      (when (setq missing (looking-at "no file "))
	(goto-char (match-end 0)))
      (cond
       ((re-search-forward "\\=\\([^ \t]+\\)" nil t)
	(setq file (expand-file-name (match-string 1)))
	(setq status(if (re-search-forward "\\=[ \t]+Status: \\(.*\\)" nil t)
                        (match-string 1) "Unknown"))
	(when (and full
		   (re-search-forward
		    "\\(RCS Version\\|RCS Revision\\|Repository revision\\):\
[\t ]+\\([0-9.]+\\)"
		    nil t))
	    (vc-file-setprop file 'vc-latest-revision (match-string 2)))
	(vc-file-setprop
	 file 'vc-state
	 (cond
	  ((string-match "Up-to-date" status)
	   (vc-file-setprop file 'vc-checkout-time
			    (nth 5 (file-attributes file)))
	   'up-to-date)
	  ((string-match "Locally Modified" status)             'edited)
	  ((string-match "Needs Merge" status)                  'needs-merge)
	  ((string-match "Needs \\(Checkout\\|Patch\\)" status)
	   (if missing 'missing 'needs-update))
	  ((string-match "Locally Added" status)                'added)
	  ((string-match "Locally Removed" status)              'removed)
	  ((string-match "File had conflicts " status)          'conflict)
          ((string-match "Unknown" status)			'unregistered)
	  (t 'edited))))))))

(defun vc-cvs-after-dir-status (update-function)
  (let ((result nil)
        (translation '((?? . unregistered)
                       (?A . added)
                       (?C . conflict)
                       (?M . edited)
                       (?P . needs-merge)
                       (?R . removed)
                       (?U . needs-update))))
    (goto-char (point-min))
    (while (not (eobp))
      (if (looking-at "^[ACMPRU?] \\(.*\\)$")
          (push (list (match-string 1)
                      (cdr (assoc (char-after) translation)))
                result)
        (cond
         ((looking-at "cvs update: warning: \\(.*\\) was lost")
          ;; Format is:
          ;; cvs update: warning: FILENAME was lost
          ;; U FILENAME
          (push (list (match-string 1) 'missing) result)
          ;; Skip the "U" line
          (forward-line 1))
         ((looking-at "cvs update: New directory `\\(.*\\)' -- ignored")
          (push (list (match-string 1) 'unregistered) result))))
      (forward-line 1))
    (funcall update-function result)))

;; Based on vc-cvs-dir-state-heuristic from Emacs 22.
;; FIXME does not mention unregistered files.
(defun vc-cvs-dir-status-heuristic (dir update-function &optional basedir)
  "Find the CVS state of all files in DIR, using only local information."
  (let (file basename status result dirlist)
    (with-temp-buffer
      (vc-cvs-get-entries dir)
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at "D/\\([^/]*\\)////")
            (push (expand-file-name (match-string 1) dir) dirlist)
          ;; CVS-removed files are not taken under VC control.
          (when (looking-at "/\\([^/]*\\)/[^/-]")
            (setq basename (match-string 1)
                  file (expand-file-name basename dir)
                  status (or (vc-file-getprop file 'vc-state)
                             (vc-cvs-parse-entry file t)))
            (unless (eq status 'up-to-date)
              (push (list (if basedir
                              (file-relative-name file basedir)
                            basename)
                          status) result))))
        (forward-line 1)))
    (dolist (subdir dirlist)
      (setq result (append result
                           (vc-cvs-dir-status-heuristic subdir nil
                                                        (or basedir dir)))))
    (if basedir result
      (funcall update-function result))))

(defun vc-cvs-dir-status-files (dir files update-function)
  "Create a list of conses (file . state) for FILES in DIR.
Query all files in DIR if files is nil."
  (let ((local (vc-cvs-stay-local-p dir)))
    (if (and (not files) local (not (eq local 'only-file)))
        (vc-cvs-dir-status-heuristic dir update-function))
    (vc-cvs-command (current-buffer) 'async
                    files
                    "-f" "-n" "-q" "update")
    (vc-run-delayed
      (vc-cvs-after-dir-status update-function))))

(defun vc-cvs-file-to-string (file)
  "Read the content of FILE and return it as a string."
  (condition-case nil
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(buffer-substring (point) (point-max)))
    (file-error nil)))

(defun vc-cvs-dir-extra-headers (_dir)
  "Extract and represent per-directory properties of a CVS working copy."
  (let ((repo
	 (condition-case nil
	     (with-temp-buffer
	       (insert-file-contents "CVS/Root")
	       (goto-char (point-min))
	       (and (looking-at ":ext:") (delete-char 5))
	       (concat (buffer-substring (point) (1- (point-max))) "\n"))
	   (file-error nil)))
	(module
	 (condition-case nil
	     (with-temp-buffer
	       (insert-file-contents "CVS/Repository")
	       (goto-char (point-min))
	       (skip-chars-forward "^\n")
	       (concat (buffer-substring (point-min) (point)) "\n"))
	   (file-error nil))))
    (concat
     (cond (repo
	    (concat (propertize "Repository : " 'face 'font-lock-type-face)
                    (propertize repo 'face 'font-lock-variable-name-face)))
	   (t ""))
     (cond (module
	    (concat (propertize "Module     : " 'face 'font-lock-type-face)
                    (propertize module 'face 'font-lock-variable-name-face)))
	   (t ""))
     (if (file-readable-p "CVS/Tag")
	 (let ((tag (vc-cvs-file-to-string "CVS/Tag")))
	   (cond
	    ((string-match "\\`T" tag)
	     (concat (propertize "Tag        : " 'face 'font-lock-type-face)
		     (propertize (substring tag 1)
				 'face 'font-lock-variable-name-face)))
	    ((string-match "\\`D" tag)
	     (concat (propertize "Date       : " 'face 'font-lock-type-face)
		     (propertize (substring tag 1)
				 'face 'font-lock-variable-name-face)))
	    (t ""))))

     ;; In CVS, branch is a per-file property, not a per-directory property.
     ;; We can't really do this here without making dangerous assumptions.
     ;;(propertize "Branch:     " 'face 'font-lock-type-face)
     ;;(propertize "ADD CODE TO PRINT THE BRANCH NAME\n"
     ;;	 'face 'font-lock-warning-face)
     )))

(defun vc-cvs-get-entries (dir)
  "Insert the CVS/Entries file from below DIR into the current buffer.
This function ensures that the correct coding system is used for that,
which may not be the one that is used for the files' contents.
CVS/Entries should only be accessed through this function."
  (let ((coding-system-for-read (or file-name-coding-system
                                    default-file-name-coding-system)))
    (vc-insert-file (expand-file-name "CVS/Entries" dir))))

(defun vc-cvs-valid-symbolic-tag-name-p (tag)
  "Return non-nil if TAG is a valid symbolic tag name."
  ;; According to the CVS manual, a valid symbolic tag must start with
  ;; an uppercase or lowercase letter and can contain uppercase and
  ;; lowercase letters, digits, `-', and `_'.
  (and (string-match "^[a-zA-Z]" tag)
       (not (string-match "[^a-z0-9A-Z-_]" tag))))

(defun vc-cvs-valid-revision-number-p (tag)
  "Return non-nil if TAG is a valid revision number."
  (and (string-match "^[0-9]" tag)
       (not (string-match "[^0-9.]" tag))))

(defun vc-cvs-parse-sticky-tag (match-type match-tag)
  "Parse and return the sticky tag as a string.
`match-data' is protected."
  (let ((data (match-data))
	(tag)
	(type (cond ((string= match-type "D") 'date)
		    ((string= match-type "T")
		     (if (vc-cvs-valid-symbolic-tag-name-p match-tag)
			 'symbolic-name
		       'revision-number))
		    (t nil))))
    (unwind-protect
	(progn
	  (cond
	   ;; Sticky Date tag.  Convert to a proper date value (`encode-time')
	   ((eq type 'date)
	    (string-match
	     "\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)"
	     match-tag)
	    (let* ((year-tmp (string-to-number (match-string 1 match-tag)))
		   (month    (string-to-number (match-string 2 match-tag)))
		   (day      (string-to-number (match-string 3 match-tag)))
		   (hour     (string-to-number (match-string 4 match-tag)))
		   (min      (string-to-number (match-string 5 match-tag)))
		   (sec      (string-to-number (match-string 6 match-tag)))
		   ;; Years 0..68 are 2000..2068.
		   ;; Years 69..99 are 1969..1999.
		   (year (+ (cond ((> 69 year-tmp) 2000)
				  ((> 100 year-tmp) 1900)
				  (t 0))
			    year-tmp)))
	      (setq tag (encode-time sec min hour day month year))))
	   ;; Sticky Tag name or revision number
	   ((eq type 'symbolic-name) (setq tag match-tag))
	   ((eq type 'revision-number) (setq tag match-tag))
	   ;; Default is no sticky tag at all
	   (t nil))
	  (cond ((eq vc-cvs-sticky-tag-display nil) nil)
		((eq vc-cvs-sticky-tag-display t)
		 (cond ((eq type 'date) (format-time-string
					 vc-cvs-sticky-date-format-string
					 tag))
		       ((eq type 'symbolic-name) tag)
		       ((eq type 'revision-number) tag)
		       (t nil)))
		((functionp vc-cvs-sticky-tag-display)
		 (funcall vc-cvs-sticky-tag-display tag type))
		(t nil)))

      (set-match-data data))))

(defun vc-cvs-parse-entry (file &optional set-state)
  "Parse a line from CVS/Entries.
Compare modification time to that of the FILE, set file properties
accordingly.  However, `vc-state' is set only if optional arg SET-STATE
is non-nil."
  (cond
   ;; entry for a "locally added" file (not yet committed)
   ((looking-at "/[^/]+/0/")
    (vc-file-setprop file 'vc-checkout-time 0)
    (vc-file-setprop file 'vc-working-revision "0")
    (if set-state (vc-file-setprop file 'vc-state 'added)))
   ;; normal entry
   ((looking-at
     (concat "/[^/]+"
	     ;; revision
	     "/\\([^/]*\\)"
	     ;; timestamp and optional conflict field
	     "/\\([^/]*\\)/"
	     ;; options
	     "\\([^/]*\\)/"
	     ;; sticky tag
	     "\\(.\\|\\)" ;Sticky tag type (date or tag name, could be empty)
	     "\\(.*\\)"))		;Sticky tag
    (vc-file-setprop file 'vc-working-revision (match-string 1))
    (vc-file-setprop file 'vc-cvs-sticky-tag
		     (vc-cvs-parse-sticky-tag (match-string 4)
                                              (match-string 5)))
    ;; Compare checkout time and modification time.
    ;; This is intentionally different from the algorithm that CVS uses
    ;; (which is based on textual comparison), because there can be problems
    ;; generating a time string that looks exactly like the one from CVS.
    (let* ((time (match-string 2))
           (mtime (nth 5 (file-attributes file)))
           (parsed-time (progn (require 'parse-time)
                               (parse-time-string (concat time " +0000")))))
      (cond ((and (not (string-match "\\+" time))
                  (car parsed-time)
                  ;; Compare just the seconds part of the file time,
                  ;; since CVS file time stamp resolution is just 1 second.
                  (let ((ptime (apply 'encode-time parsed-time)))
                    (and (eq (car mtime) (car ptime))
                         (eq (cadr mtime) (cadr ptime)))))
             (vc-file-setprop file 'vc-checkout-time mtime)
             (if set-state (vc-file-setprop file 'vc-state 'up-to-date)))
            (t
             (vc-file-setprop file 'vc-checkout-time 0)
             (if set-state (vc-file-setprop file 'vc-state 'edited))))))))

;; Completion of revision names.
;; Just so I don't feel like I'm duplicating code from pcl-cvs, I'll use
;; `cvs log' so I can list all the revision numbers rather than only
;; tag names.

(defun vc-cvs-revision-table (file)
  (let (process-file-side-effects
	(default-directory (file-name-directory file))
        (res nil))
    (with-temp-buffer
      (vc-cvs-command t nil file "log")
      (goto-char (point-min))
      (when (re-search-forward "^symbolic names:\n" nil t)
        (while (looking-at "^	\\(.*\\): \\(.*\\)")
          (push (cons (match-string 1) (match-string 2)) res)
          (forward-line 1)))
      (while (re-search-forward "^revision \\([0-9.]+\\)" nil t)
        (push (match-string 1) res))
      res)))

(defun vc-cvs-revision-completion-table (files)
  (letrec ((table (lazy-completion-table
                   table (lambda () (vc-cvs-revision-table (car files))))))
    table))

(defun vc-cvs-find-admin-dir (file)
  "Return the administrative directory of FILE."
  (vc-find-root file "CVS"))

(defun vc-cvs-ignore (file &optional _directory _remove)
  "Ignore FILE under CVS."
  (vc-cvs-append-to-ignore (file-name-directory file) file))

(defun vc-cvs-append-to-ignore (dir str &optional old-dir)
  "In DIR, add STR to the .cvsignore file.
If OLD-DIR is non-nil, then this is a directory that we don't want
to hear about anymore."
  (with-current-buffer
      (find-file-noselect (expand-file-name ".cvsignore" dir))
    (when (ignore-errors
	    (and buffer-read-only
		 (eq 'CVS (vc-backend buffer-file-name))
		 (not (vc-editable-p buffer-file-name))))
      ;; CVSREAD=on special case
      (vc-checkout buffer-file-name t))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert str (if old-dir "/\n" "\n"))
    ;; FIXME this is a pcvs variable.
    (if (bound-and-true-p cvs-sort-ignore-file)
        (sort-lines nil (point-min) (point-max)))
    (save-buffer)))

(provide 'vc-cvs)

;;; vc-cvs.el ends here
