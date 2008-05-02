;;; vc-cvs.el --- non-resident support for CVS version-control

;; Copyright (C) 1995, 1998, 1999, 2000, 2001, 2002, 2003,
;;   2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author:      FSF (see vc.el for full credits)
;; Maintainer:  Andre Spiegel <spiegel@gnu.org>

;; $Id$

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

;;; Code:

(eval-when-compile (require 'cl) (require 'vc))

;; Clear up the cache to force vc-call to check again and discover
;; new functions when we reload this file.
(put 'CVS 'vc-functions nil)

;;;
;;; Customization options
;;;

(defcustom vc-cvs-global-switches nil
  "*Global switches to pass to any CVS command."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "22.1"
  :group 'vc)

(defcustom vc-cvs-register-switches nil
  "*Extra switches for registering a file into CVS.
A string or list of strings passed to the checkin program by
\\[vc-register]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "21.1"
  :group 'vc)

(defcustom vc-cvs-diff-switches nil
  "*A string or list of strings specifying extra switches for cvs diff under VC."
    :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "21.1"
  :group 'vc)

(defcustom vc-cvs-header (or (cdr (assoc 'CVS vc-header-alist)) '("\$Id\$"))
  "*Header keywords to be inserted by `vc-insert-headers'."
  :version "21.1"
  :type '(repeat string)
  :group 'vc)

(defcustom vc-cvs-use-edit t
  "*Non-nil means to use `cvs edit' to \"check out\" a file.
This is only meaningful if you don't use the implicit checkout model
\(i.e. if you have $CVSREAD set)."
  :type 'boolean
  :version "21.1"
  :group 'vc)

(defcustom vc-cvs-stay-local t
  "*Non-nil means use local operations when possible for remote repositories.
This avoids slow queries over the network and instead uses heuristics
and past information to determine the current status of a file.

The value can also be a regular expression or list of regular
expressions to match against the host name of a repository; then VC
only stays local for hosts that match it.  Alternatively, the value
can be a list of regular expressions where the first element is the
symbol `except'; then VC always stays local except for hosts matched
by these regular expressions."
  :type '(choice (const :tag "Always stay local" t)
                (const :tag "Don't stay local" nil)
                 (list :format "\nExamine hostname and %v" :tag "Examine hostname ..."
                       (set :format "%v" :inline t (const :format "%t" :tag "don't" except))
                       (regexp :format " stay local,\n%t: %v" :tag "if it matches")
                       (repeat :format "%v%i\n" :inline t (regexp :tag "or"))))
  :version "21.1"
  :group 'vc)

(defcustom vc-cvs-sticky-date-format-string "%c"
  "*Format string for mode-line display of sticky date.
Format is according to `format-time-string'.  Only used if
`vc-cvs-sticky-tag-display' is t."
  :type '(string)
  :version "22.1"
  :group 'vc)

(defcustom vc-cvs-sticky-tag-display t
  "*Specify the mode-line display of sticky tags.
Value t means default display, nil means no display at all.  If the
value is a function or macro, it is called with the sticky tag and
its' type as parameters, in that order.  TYPE can have three different
values: `symbolic-name' (TAG is a string), `revision-number' (TAG is a
string) and `date' (TAG is a date as returned by `encode-time').  The
return value of the function or macro will be displayed as a string.

Here's an example that will display the formatted date for sticky
dates and the word \"Sticky\" for sticky tag names and revisions.

  (lambda (tag type)
    (cond ((eq type 'date) (format-time-string
                              vc-cvs-sticky-date-format-string tag))
          ((eq type 'revision-number) \"Sticky\")
          ((eq type 'symbolic-name) \"Sticky\")))

Here's an example that will abbreviate to the first character only,
any text before the first occurrence of `-' for sticky symbolic tags.
If the sticky tag is a revision number, the word \"Sticky\" is
displayed.  Date and time is displayed for sticky dates.

   (lambda (tag type)
     (cond ((eq type 'date) (format-time-string \"%Y%m%d %H:%M\" tag))
           ((eq type 'revision-number) \"Sticky\")
           ((eq type 'symbolic-name)
            (condition-case nil
                (progn
                  (string-match \"\\\\([^-]*\\\\)\\\\(.*\\\\)\" tag)
                  (concat (substring (match-string 1 tag) 0 1) \":\"
                          (substring (match-string 2 tag) 1 nil)))
              (error tag)))))       ; Fall-back to given tag name.

See also variable `vc-cvs-sticky-date-format-string'."
  :type '(choice boolean function)
  :version "22.1"
  :group 'vc)

;;;
;;; Internal variables
;;;


;;;
;;; State-querying functions
;;;

;;;###autoload (defun vc-cvs-registered (f)
;;;###autoload   (when (file-readable-p (expand-file-name
;;;###autoload 			  "CVS/Entries" (file-name-directory f)))
;;;###autoload       (load "vc-cvs")
;;;###autoload       (vc-cvs-registered f)))

(defun vc-cvs-registered (file)
  "Check if FILE is CVS registered."
  (let ((dirname (or (file-name-directory file) ""))
	(basename (file-name-nondirectory file))
        ;; make sure that the file name is searched case-sensitively
        (case-fold-search nil))
    (if (file-readable-p (expand-file-name "CVS/Entries" dirname))
	(with-temp-buffer
          (vc-cvs-get-entries dirname)
          (goto-char (point-min))
	  (cond
	   ((re-search-forward
	     (concat "^/" (regexp-quote basename) "/[^/]") nil t)
	    (beginning-of-line)
	    (vc-cvs-parse-entry file)
	    t)
	   (t nil)))
      nil)))

(defun vc-cvs-state (file)
  "CVS-specific version of `vc-state'."
  (if (vc-stay-local-p file)
      (let ((state (vc-file-getprop file 'vc-state)))
        ;; If we should stay local, use the heuristic but only if
        ;; we don't have a more precise state already available.
	(if (memq state '(up-to-date edited nil))
	    (vc-cvs-state-heuristic file)
	  state))
    (with-temp-buffer
      (cd (file-name-directory file))
      (vc-cvs-command t 0 file "status")
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
     (t 'edited))))

(defun vc-cvs-dir-state (dir)
  "Find the CVS state of all files in DIR and subdirectories."
  ;; if DIR is not under CVS control, don't do anything.
  (when (file-readable-p (expand-file-name "CVS/Entries" dir))
    (if (vc-stay-local-p dir)
	(vc-cvs-dir-state-heuristic dir)
      (let ((default-directory dir))
	;; Don't specify DIR in this command, the default-directory is
	;; enough.  Otherwise it might fail with remote repositories.
	(with-temp-buffer
	  (buffer-disable-undo)		;; Because these buffers can get huge
	  (vc-cvs-command t 0 nil "status")
	  (goto-char (point-min))
	  (while (re-search-forward "^=+\n\\([^=\n].*\n\\|\n\\)+" nil t)
	    (narrow-to-region (match-beginning 0) (match-end 0))
	    (vc-cvs-parse-status)
	    (goto-char (point-max))
	    (widen)))))))

(defun vc-cvs-working-revision (file)
  "CVS-specific version of `vc-working-revision'."
  ;; There is no need to consult RCS headers under CVS, because we
  ;; get the workfile version for free when we recognize that a file
  ;; is registered in CVS.
  (vc-cvs-registered file)
  (vc-file-getprop file 'vc-working-revision))

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
                    ;; (If vc-mistrust-permissions was t, we actually shouldn't
                    ;; trust this, but there is no other way to learn this from
                    ;; CVS at the moment (version 1.9).)
                    (string-match "r-..-..-." (nth 8 attrib)))
               'announce
             'implicit))))))

(defun vc-cvs-mode-line-string (file)
  "Return string for placement into the modeline for FILE.
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
       (setq help-echo (format "%s on the '%s' branch" 
			       help-echo sticky-tag))
       (concat string "[" sticky-tag "]"))
     'help-echo help-echo)))


;;;
;;; State-changing functions
;;;

(defun vc-cvs-register (files &optional rev comment)
  "Register FILES into the CVS version-control system.
COMMENT can be used to provide an initial description of FILES.

`vc-register-switches' and `vc-cvs-register-switches' are passed to
the CVS command (in that order)."
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
      (setq dir (if (file-directory-p
                     (expand-file-name "CVS/Entries" dir))
                    t (directory-file-name dir))))
    (eq dir t)))

(defun vc-cvs-checkin (files rev comment)
  "CVS-specific version of `vc-backend-checkin'."
  (unless (or (not rev) (vc-cvs-valid-revision-number-p rev))
    (if (not (vc-cvs-valid-symbolic-tag-name-p rev))
	(error "%s is not a valid symbolic tag name" rev)
      ;; If the input revison is a valid symbolic tag name, we create it
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

(defun vc-cvs-checkout (file &optional editable rev)
  "Checkout a revision of FILE into the working area.
EDITABLE non-nil means that the file should be writable.
REV is the revision to check out."
  (message "Checking out %s..." file)
  ;; Change buffers to get local value of vc-checkout-switches.
  (with-current-buffer (or (get-file-buffer file) (current-buffer))
    (if (and (file-exists-p file) (not rev))
        ;; If no revision was specified, just make the file writable
        ;; if necessary (using `cvs-edit' if requested).
        (and editable (not (eq (vc-cvs-checkout-model file) 'implicit))
             (if vc-cvs-use-edit
                 (vc-cvs-command nil 0 file "edit")
               (set-file-modes file (logior (file-modes file) 128))
               (if (equal file buffer-file-name) (toggle-read-only -1))))
      ;; Check out a particular revision (or recreate the file).
      (vc-file-setprop file 'vc-working-revision nil)
      (apply 'vc-cvs-command nil 0 file
             (and editable "-w")
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
    (vc-mode-line file))
  (message "Checking out %s...done" file))

(defun vc-cvs-delete-file (file)
  (vc-cvs-command nil 0 file "remove" "-f"))

(defun vc-cvs-revert (file &optional contents-done)
  "Revert FILE to the working revision on which it was based."
  (vc-default-revert 'CVS file contents-done)
  (unless (eq (vc-cvs-checkout-model file) 'implicit)
    (if vc-cvs-use-edit
        (vc-cvs-command nil 0 file "unedit")
      ;; Make the file read-only by switching off all w-bits
      (set-file-modes file (logand (file-modes file) 3950)))))

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
                       (regexp-quote (file-name-nondirectory file))
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

(defun vc-cvs-print-log (files &optional buffer)
  "Get change logs associated with FILES."
  ;; It's just the catenation of the individual logs.
  (vc-cvs-command
   buffer
   (if (vc-stay-local-p files) 'async 0)
   files "log"))

(defun vc-cvs-wash-log ()
  "Remove all non-comment information from log output."
  (vc-call-backend 'RCS 'wash-log)
  nil)

(defun vc-cvs-diff (files &optional oldvers newvers buffer)
  "Get a difference report using CVS between two revisions of FILE."
  (let* ((async (and (not vc-disable-async-diff)
		     (vc-stay-local-p files)))
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
		(apply 'vc-do-command (or buffer "*vc-diff*") 1 "diff" nil
		       (append (if (listp diff-switches)
				   diff-switches
				 (list diff-switches))
			       (if (listp vc-diff-switches)
				   vc-diff-switches
				 (list vc-diff-switches))
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

(defun vc-cvs-annotate-process-filter (process string)
  (setq string (concat (process-get process 'output) string))
  (if (not (string-match vc-cvs-annotate-first-line-re string))
      ;; Still waiting for the first real line.
      (process-put process 'output string)
    (let ((vc-filter (process-get process 'vc-filter)))
      (set-process-filter process vc-filter)
      (funcall vc-filter process (substring string (match-beginning 0))))))

(defun vc-cvs-annotate-command (file buffer &optional revision)
  "Execute \"cvs annotate\" on FILE, inserting the contents in BUFFER.
Optional arg REVISION is a revision to annotate from."
  (vc-cvs-command buffer
                  (if (vc-stay-local-p file)
		      'async 0)
                  file "annotate"
                  (if revision (concat "-r" revision)))
  ;; Strip the leading few lines.
  (let ((proc (get-buffer-process buffer)))
    (if proc
        ;; If running asynchronously, use a process filter.
        (progn
          (process-put proc 'vc-filter (process-filter proc))
          (set-process-filter proc 'vc-cvs-annotate-process-filter))
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward vc-cvs-annotate-first-line-re)
        (delete-region (point-min) (1- (point)))))))

(defun vc-cvs-annotate-current-time ()
  "Return the current time, based at midnight of the current day, and
encoded as fractional days."
  (vc-annotate-convert-time
   (apply 'encode-time 0 0 0 (nthcdr 3 (decode-time (current-time))))))

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

;;;
;;; Snapshot system
;;;

(defun vc-cvs-create-snapshot (dir name branchp)
  "Assign to DIR's current revision a given NAME.
If BRANCHP is non-nil, the name is created as a branch (and the current
workspace is immediately moved to that new branch)."
  (vc-cvs-command nil 0 dir "tag" "-c" (if branchp "-b") name)
  (when branchp (vc-cvs-command nil 0 dir "update" "-r" name)))

(defun vc-cvs-retrieve-snapshot (dir name update)
  "Retrieve a snapshot at and below DIR.
NAME is the name of the snapshot; if it is empty, do a `cvs update'.
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

(defalias 'vc-cvs-make-version-backups-p 'vc-stay-local-p
  "Return non-nil if version backups should be made for FILE.")

(defun vc-cvs-check-headers ()
  "Check if the current file has any headers in it."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\\$[A-Za-z\300-\326\330-\366\370-\377]+\
\\(: [\t -#%-\176\240-\377]*\\)?\\$" nil t)))


;;;
;;; Internal functions
;;;

(defun vc-cvs-root (dir)
  (vc-find-root dir "CVS" t))

(defun vc-cvs-command (buffer okstatus files &rest flags)
  "A wrapper around `vc-do-command' for use in vc-cvs.el.
The difference to vc-do-command is that this function always invokes `cvs',
and that it passes `vc-cvs-global-switches' to it before FLAGS."
  (apply 'vc-do-command buffer okstatus "cvs" files
         (if (stringp vc-cvs-global-switches)
             (cons vc-cvs-global-switches flags)
           (append vc-cvs-global-switches
                   flags))))

(defalias 'vc-cvs-stay-local-p 'vc-stay-local-p)  ;Back-compatibility.

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

(defun vc-cvs-parse-root (root)
  "Split CVS ROOT specification string into a list of fields.
A CVS root specification of the form
  [:METHOD:][[USER@]HOSTNAME:]/path/to/repository
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
            ;; Simple PATH => method `local'
            (cons "local"
                  (cons nil root-list)))
           ((= len 2)
            ;; [USER@]HOST:PATH => method `ext'
            (and (not (equal (car root-list) ""))
                 (cons "ext" root-list)))
           ((= len 3)
            ;; :METHOD:PATH
            (cons (cadr root-list)
                  (cons nil (cddr root-list))))
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
               (setq host))
          ;; Fix windows style CVS root `:local:C:\\project\\cvs\\some\\dir'
          (and host
               (equal method "local")
               (setq root (concat host ":" root) host))
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
essential information. Note that this can never set the 'ignored
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
	(vc-file-setprop file 'vc-backend 'CVS)
	(if (not (re-search-forward "\\=[ \t]+Status: \\(.*\\)" nil t))
	    (setq status "Unknown")
	  (setq status (match-string 1)))
	(when (and full
		   (re-search-forward
		    "\\(RCS Version\\|RCS Revision\\|Repository revision\\):\
\[\t ]+\\([0-9.]+\\)"
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
	  (t 'edited))))))))

(defun vc-cvs-dir-state-heuristic (dir)
  "Find the CVS state of all files in DIR, using only local information."
  (with-temp-buffer
    (vc-cvs-get-entries dir)
    (goto-char (point-min))
    (while (not (eobp))
      ;; CVS-removed files are not taken under VC control.
      (when (looking-at "/\\([^/]*\\)/[^/-]")
	(let ((file (expand-file-name (match-string 1) dir)))
	  (unless (vc-file-getprop file 'vc-state)
	    (vc-cvs-parse-entry file t))))
      (forward-line 1))))

(defun vc-cvs-after-dir-status (update-function)
  ;; Heavily inspired by vc-cvs-parse-status. AKA a quick hack.
  ;; This needs a lot of testing.
  (let ((status nil)
	(status-str nil)
	(file nil)
	(result nil)
	(missing nil)
	(subdir default-directory))
    (goto-char (point-min))
    (while
	;; Look for either a file entry, an unregistered file, or a
	;; directory change.
	(re-search-forward
	 "\\(^=+\n\\([^=c?\n].*\n\\|\n\\)+\\)\\|\\(\\(^?? .*\n\\)+\\)\\|\\(^cvs status: Examining .*\n\\)"
	 nil t)
      ;; XXX: get rid of narrowing here.
      (narrow-to-region (match-beginning 0) (match-end 0))
      (goto-char (point-min))
      ;; The subdir
      (when (looking-at "cvs status: Examining \\(.+\\)")
	(setq subdir (expand-file-name (match-string 1))))
      ;; Unregistered files
      (while (looking-at "? \\(.*\\)")
	(setq file (file-relative-name 
		    (expand-file-name (match-string 1) subdir)))
	(push (list file 'unregistered) result)
	(forward-line 1))
      ;; A file entry.
      (when (re-search-forward "^File: " nil t)
	(when (setq missing (looking-at "no file "))
	  (goto-char (match-end 0)))
	(cond
	 ((re-search-forward "\\=\\([^ \t]+\\)" nil t)
	  (setq file (file-relative-name 
		      (expand-file-name (match-string 1) subdir)))
	  (if (not (re-search-forward "\\=[ \t]+Status: \\(.*\\)" nil t))
	      (push (list file 'unregistered) result)
	    (setq status-str (match-string 1))
	    (setq status
		  (cond
		   ((string-match "Up-to-date" status-str) 'up-to-date)
		   ((string-match "Locally Modified" status-str) 'edited)
		   ((string-match "Needs Merge" status-str) 'needs-merge)
		   ((string-match "Needs \\(Checkout\\|Patch\\)" status-str)
		    (if missing 'missing 'needs-update))
		   ((string-match "Locally Added" status-str) 'added)
		   ((string-match "Locally Removed" status-str) 'removed)
		   ((string-match "File had conflicts " status-str) 'conflict)
		   (t 'edited)))
	    (unless (eq status 'up-to-date)
	      (push (list file status) result))))))
      (goto-char (point-max))
      (widen))
      (funcall update-function result))
  ;; Alternative implementation: use the "update" command instead of
  ;; the "status" command.
  ;; (let ((result nil)
  ;; 	(translation '((?? . unregistered)
  ;; 		       (?A . added)
  ;; 		       (?C . conflict)
  ;; 		       (?M . edited)
  ;; 		       (?P . needs-merge)
  ;; 		       (?R . removed)
  ;; 		       (?U . needs-update))))
  ;;   (goto-char (point-min))
  ;;   (while (not (eobp))
  ;;     (if (looking-at "^[ACMPRU?] \\(.*\\)$")
  ;; 	  (push (list (match-string 1) 
  ;; 		      (cdr (assoc (char-after) translation))) 
  ;; 		result)
  ;; 	(cond
  ;; 	 ((looking-at "cvs update: warning: \\(.*\\) was lost")
  ;; 	  ;; Format is:
  ;; 	  ;; cvs update: warning: FILENAME was lost
  ;; 	  ;; U FILENAME
  ;; 	  (push (list (match-string 1) 'missing) result)
  ;; 	  ;; Skip the "U" line
  ;; 	  (forward-line 1))
  ;; 	 ((looking-at "cvs update: New directory `\\(.*\\)' -- ignored")
  ;; 	  (push (list (match-string 1) 'unregistered) result))))
  ;;     (forward-line 1))
  ;;   (funcall update-function result)))
  )

(defun vc-cvs-dir-status (dir update-function)
  "Create a list of conses (file . state) for DIR."
  (vc-cvs-command (current-buffer) 'async dir "status")
  ;; Alternative implementation: use the "update" command instead of
  ;; the "status" command.
  ;; (vc-cvs-command (current-buffer) 'async
  ;; 		  (file-relative-name dir)
  ;; 		  "-f" "-n" "update" "-d" "-P")
  (vc-exec-after
   `(vc-cvs-after-dir-status (quote ,update-function))))

(defun vc-cvs-status-extra-headers (dir)
  (concat
   ;; FIXME: see how PCL-CVS gets the data to print all these
   (propertize "Module     : " 'face 'font-lock-type-face)
   (propertize "ADD CODE TO PRINT THE MODULE\n"
	       'face 'font-lock-warning-face)
   (propertize "Repository : " 'face 'font-lock-type-face)
   (propertize "ADD CODE TO PRINT THE REPOSITORY\n"
	       'face 'font-lock-warning-face)
   (propertize "Branch     : " 'face 'font-lock-type-face)
   (propertize "ADD CODE TO PRINT THE BRANCH NAME\n"
	       'face 'font-lock-warning-face)))

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
    (vc-file-setprop file 'vc-backend 'CVS)
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
    (vc-file-setprop file 'vc-backend 'CVS)
    (vc-file-setprop file 'vc-working-revision (match-string 1))
    (vc-file-setprop file 'vc-cvs-sticky-tag
		     (vc-cvs-parse-sticky-tag (match-string 4)
                                              (match-string 5)))
    ;; Compare checkout time and modification time.
    ;; This is intentionally different from the algorithm that CVS uses
    ;; (which is based on textual comparison), because there can be problems
    ;; generating a time string that looks exactly like the one from CVS.
    (let ((mtime (nth 5 (file-attributes file))))
      (require 'parse-time)
      (let ((parsed-time
	     (parse-time-string (concat (match-string 2) " +0000"))))
	(cond ((and (not (string-match "\\+" (match-string 2)))
		    (car parsed-time)
		    (equal mtime (apply 'encode-time parsed-time)))
	       (vc-file-setprop file 'vc-checkout-time mtime)
	       (if set-state (vc-file-setprop file 'vc-state 'up-to-date)))
	      (t
	       (vc-file-setprop file 'vc-checkout-time 0)
	       (if set-state (vc-file-setprop file 'vc-state 'edited)))))))))

;; Completion of revision names.
;; Just so I don't feel like I'm duplicating code from pcl-cvs, I'll use
;; `cvs log' so I can list all the revision numbers rather than only
;; tag names.

(defun vc-cvs-revision-table (file)
  (let ((default-directory (file-name-directory file))
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
  (lexical-let ((files files)
                table)
    (setq table (lazy-completion-table
                 table (lambda () (vc-cvs-revision-table (car files)))))
    table))
                                           

(provide 'vc-cvs)

;; arch-tag: 60e1402a-aa53-4607-927a-cf74f144b432
;;; vc-cvs.el ends here
