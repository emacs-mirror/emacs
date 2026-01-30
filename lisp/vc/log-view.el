;;; log-view.el --- Major mode for browsing revision log histories -*- lexical-binding: t -*-

;; Copyright (C) 1999-2026 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: tools, vc

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

;; Major mode to browse revision log histories.
;; Currently supports the format output by:
;; SCCS, RCS, CVS, Subversion, DaRCS, and Mercurial.

;; Examples of log output:

;;;; SCCS:

;;;; RCS/CVS:

;; ----------------------------
;; revision 1.35	locked by: turlutut
;; date: 2005-03-22 18:48:38 +0000;  author: monnier;  state: Exp;  lines: +6 -8
;; (gnus-display-time-event-handler):
;; Check display-time-timer at runtime rather than only at load time
;; in case display-time-mode is turned off in the mean time.
;; ----------------------------
;; revision 1.34
;; date: 2005-02-09 15:50:38 +0000;  author: kfstorm;  state: Exp;  lines: +7 -7
;; branches:  1.34.2;
;; Change release version from 21.4 to 22.1 throughout.
;; Change development version from 21.3.50 to 22.0.50.

;;;; Subversion:

;; ------------------------------------------------------------------------
;; r4622 | ckuethe | 2007-12-23 18:18:01 -0500 (Sun, 23 Dec 2007) | 2 lines
;;
;; uBlox AEK-4T in binary mode.  Added to unstable because it breaks gpsfake
;;
;; ------------------------------------------------------------------------
;; r4621 | ckuethe | 2007-12-23 16:48:11 -0500 (Sun, 23 Dec 2007) | 3 lines
;;
;; Add a note about requiring usbfs to use the garmin gps18 (usb)
;; Mention firmware testing the AC12 with firmware BQ00 and BQ04
;;
;; ------------------------------------------------------------------------
;; r4620 | ckuethe | 2007-12-23 15:52:34 -0500 (Sun, 23 Dec 2007) | 1 line
;;
;; add link to latest hardware reference
;; ------------------------------------------------------------------------
;; r4619 | ckuethe | 2007-12-23 14:37:31 -0500 (Sun, 23 Dec 2007) | 1 line
;;
;; there is now a regression test for AC12 without raw data output

;;;; Darcs:

;; Changes to darcsum.el:
;;
;; Mon Nov 28 15:19:38 GMT 2005  Dave Love <fx@gnu.org>
;;   * Abstract process startup into darcsum-start-process.  Use TERM=dumb.
;;   TERM=dumb avoids escape characters, at least, for any old darcs that
;;   doesn't understand DARCS_DONT_COLOR & al.
;;
;; Thu Nov 24 15:20:45 GMT 2005  Dave Love <fx@gnu.org>
;;   * darcsum-mode-related changes.
;;   Don't call font-lock-mode (unnecessary) or use-local-map (redundant).
;;   Use mode-class 'special.  Add :group.
;;   Add trailing-whitespace option to mode hook and fix
;;   darcsum-display-changeset not to use trailing whitespace.

;;;; Mercurial

;; changeset:   11:8ff1a4166444
;; tag:         tip
;; user:        Eric S. Raymond <esr@thyrsus.com>
;; date:        Wed Dec 26 12:18:58 2007 -0500
;; summary:     Explain keywords.  Add markup fixes.
;;
;; changeset:   10:20abc7ab09c3
;; user:        Eric S. Raymond <esr@thyrsus.com>
;; date:        Wed Dec 26 11:37:28 2007 -0500
;; summary:     Typo fixes.
;;
;; changeset:   9:ada9f4da88aa
;; user:        Eric S. Raymond <esr@thyrsus.com>
;; date:        Wed Dec 26 11:23:00 2007 -0500
;; summary:     Add RCS example session.

;;; Todo:

;; - add ability to modify a log-entry (via cvs-mode-admin ;-)
;; - remove references to cvs-*
;; - make it easier to add support for new backends without changing the code.

;;; Code:

(require 'pcvs-util)
(require 'easy-mmode)
(require 'log-edit)
(autoload 'vc-find-revision "vc")
(autoload 'vc-diff-internal "vc")
(autoload 'vc--pick-or-revert "vc")
(autoload 'vc--remove-revisions-from-end "vc")
(autoload 'vc--prompt-other-working-tree "vc")
(autoload 'vc-exec-after "vc-dispatcher")
(eval-when-compile (require 'vc-dispatcher))

(defvar cvs-minor-wrap-function)
(defvar cvs-force-command)

(defgroup log-view nil
  "Major mode for browsing log output of revision log histories."
  :group 'pcl-cvs
  :prefix "log-view-")

(defvar-keymap log-view-mode-map
  "RET" #'log-view-toggle-entry-display
  "M-RET" #'log-view-display-entry-and-diff
  "m" #'log-view-mark-entry
  "u" #'log-view-unmark-entry
  "U" #'log-view-unmark-all-entries
  "e" #'log-view-modify-change-comment
  "d" #'log-view-diff
  "=" #'log-view-diff
  "D" #'log-view-diff-changeset
  "a" #'log-view-annotate-version
  "f" #'log-view-find-revision
  "n" #'log-view-msg-next
  "p" #'log-view-msg-prev
  "M-n" #'log-view-msg-and-diff-next
  "M-p" #'log-view-msg-and-diff-prev
  "w" #'log-view-copy-revision-as-kill
  "TAB" #'log-view-msg-next
  "<backtab>" #'log-view-msg-prev
  "C" #'log-view-cherry-pick
  "R" #'log-view-revert-or-delete-revisions
  "x" #'log-view-uncommit-revisions-from-end
  "X" #'log-view-delete-revisions-from-end)

(easy-menu-define log-view-mode-menu log-view-mode-map
  "Log-View Display Menu."
  '("Log-View"
    ;; XXX Do we need menu entries for these?
    ;; ["Quit"  quit-window]
    ;; ["Kill This Buffer"  kill-this-buffer]
    ["Mark Log Entry for Diff"  set-mark-command
     :help ""]
    ["Diff Revisions"  log-view-diff
     :help "Get the diff between two revisions"]
    ["Changeset Diff"  log-view-diff-changeset
     :help "Get the changeset diff between two revisions"]
    ["Visit Version"  log-view-find-revision
     :help "Visit the version at point"]
    ["Annotate Version"  log-view-annotate-version
     :help "Annotate the version at point"]
    ["Modify Log Comment" log-view-modify-change-comment
     :help "Edit the change comment displayed at point"]
    ["Toggle Details at Point" log-view-toggle-entry-display
     :active log-view-expanded-log-entry-function]
    "-----"
    ["Cherry-Pick Revision(s)" log-view-cherry-pick
     :help "Copy changes from revision(s) to a branch"]
    ["Revert Revision(s)" log-view-revert-or-delete-revisions
     :help "Undo the effects of old revision(s)"]
    "-----"
    ["Next Log Entry"  log-view-msg-next
     :help "Go to the next count'th log message"]
    ["Previous Log Entry"  log-view-msg-prev
     :help "Go to the previous count'th log message"]
    ["Next File"  log-view-file-next
     :help "Go to the next count'th file"
     :active (derived-mode-p 'vc-cvs-log-view-mode
                             'vc-rcs-log-view-mode
                             'vc-sccs-log-view-mode)]
    ["Previous File"  log-view-file-prev
     :help "Go to the previous count'th file"
     :active (derived-mode-p 'vc-cvs-log-view-mode
                             'vc-rcs-log-view-mode
                             'vc-sccs-log-view-mode)]))

(defvar log-view-mode-hook nil
  "Hook run at the end of `log-view-mode'.")

(defvar log-view-expanded-log-entry-function nil
  "Function returning the detailed description of a Log View entry.
It is called by the command `log-view-toggle-entry-display' with
one arg, the revision tag (a string), and should return a string.
If it is nil, `log-view-toggle-entry-display' does nothing.")

(defface log-view-file
  '((((class color) (background light))
     (:background "grey70" :weight bold :extend t))
    (t (:weight bold :extend t)))
  "Face for the file header line in `log-view-mode'."
  :group 'log-view)

(defface log-view-message
  '((((class color) (background light))
     (:background "grey85" :extend t))
    (t (:weight bold :extend t)))
  "Face for the message header line in `log-view-mode'."
  :group 'log-view)

(defface log-view-commit-body '((t :inherit font-lock-comment-face))
  "Face for the commit body in `log-view-mode'."
  :version "28.1")

(defvar log-view-file-re
  (concat "^\\(?:Working file: \\(?1:.+\\)"                ;RCS and CVS.
          ;; Subversion has no such thing??
          "\\|\\(?:SCCS/s\\.\\|Changes to \\)\\(?1:.+\\):" ;SCCS and Darcs.
	  "\\)\n")                    ;Include the \n for font-lock reasons.
  "Regexp matching the text identifying the file.
The match group number 1 should match the file name itself.")

(defvar log-view-per-file-logs t
  "Set to t if the logs are shown one file at a time.")

(defvar log-view-message-re
  (concat "^\\(?:revision \\(?1:[.0-9]+\\)\\(?:\t.*\\)?" ; RCS and CVS.
          "\\|r\\(?1:[0-9]+\\) | .* | .*"                ; Subversion.
          "\\|D \\(?1:[.0-9]+\\) .*"                     ; SCCS.
          ;; Darcs doesn't have revision names.  VC-darcs uses patch names
          ;; instead.  Darcs patch names are hashcodes, which do not appear
          ;; in the log output :-(, but darcs accepts any prefix of the log
          ;; message as a patch name, so we match the first line of the log
          ;; message.
          ;; First loosely match the date format.
          (concat "\\|[^ \n].*[^0-9\n][0-9][0-9]:[0-9][0-9][^0-9\n].*[^ \n]"
                  ;;Email of user and finally Msg, used as revision name.
                  "  .*@.*\n\\(?:  \\* \\(?1:.*\\)\\)?")
          "\\)$")
  "Regexp matching the text identifying a revision.
The match group number 1 should match the revision number itself.")

(defvar log-view-font-lock-keywords
  ;; We use `eval' so as to use the buffer-local value of log-view-file-re
  ;; and log-view-message-re, if applicable.
  '((eval . `(,log-view-file-re
              (1 (if (boundp 'cvs-filename-face) cvs-filename-face))
              (0 'log-view-file append)))
    (eval . `(,log-view-message-re . 'log-view-message))))

(defconst log-view-font-lock-defaults
  '(log-view-font-lock-keywords t nil nil nil))

(defvar log-view-vc-fileset nil
  "The VC fileset corresponding to the current log.")

(defvar log-view-vc-backend nil
  "The VC backend that created the current log.")

;;;;
;;;; Actual code
;;;;

(declare-function project-change-to-matching-directory "project")

;;;###autoload
(define-derived-mode log-view-mode special-mode "Log-View"
  "Major mode for browsing CVS log output."
  (setq buffer-read-only t)
  (setq-local font-lock-defaults log-view-font-lock-defaults)
  (setq-local beginning-of-defun-function #'log-view-beginning-of-defun)
  (setq-local end-of-defun-function #'log-view-end-of-defun)
  (setq-local cvs-minor-wrap-function #'log-view-minor-wrap)
  (setq-local project-find-matching-buffer-function
              #'project-change-to-matching-directory)
  (add-hook 'revert-buffer-restore-functions #'log-view--restore-marks
            nil t)
  (hack-dir-local-variables-non-file-buffer))

;;;;
;;;; Navigation
;;;;

;; define log-view-{msg,file}-{next,prev}
(easy-mmode-define-navigation log-view-msg log-view-message-re "log message")
(easy-mmode-define-navigation log-view-file log-view-file-re "file")

(defun log-view-goto-rev (rev)
  "Go to revision REV."
  (goto-char (point-min))
  (ignore-errors
    (while (not (equal rev (log-view-current-tag)))
      (log-view-msg-next))
    t))

(defun log-view-msg-and-diff-next (count)
  "Go to next COUNT'th entry, expand it if possible, and show its diff.
Interactively, COUNT is the numeric prefix argument, and defaults to 1."
  (interactive "p")
  (log-view-msg-next count)
  (log-view-display-entry-and-diff))

(defun log-view-msg-and-diff-prev (count)
  "Go to previous COUNT'th entry, expand it if possible, and show its diff.
Interactively, COUNT is the numeric prefix argument, and defaults to 1."
  (interactive "p")
  (log-view-msg-next (- count))
  (log-view-display-entry-and-diff))

;;;;
;;;; Linkage to PCL-CVS (mostly copied from cvs-status.el)
;;;;

(defconst log-view-dir-re "^cvs[.ex]* [a-z]+: Logging \\(.+\\)$")

(defun log-view-current-file ()
  "Return the current file."
  (save-excursion
    (forward-line 1)
    (or (re-search-backward log-view-file-re nil t)
	(re-search-forward log-view-file-re nil t)
	(error "Unable to determine the current file"))
    (let* ((file (match-string 1))
	   (cvsdir (and (re-search-backward log-view-dir-re nil t)
			(match-string 1)))
	   (pcldir (and (boundp 'cvs-pcl-cvs-dirchange-re)
			(re-search-backward cvs-pcl-cvs-dirchange-re nil t)
			(match-string 1)))
	   (dir ""))
      (let ((default-directory ""))
	(when pcldir (setq dir (expand-file-name pcldir dir)))
	(when cvsdir (setq dir (expand-file-name cvsdir dir))))
      (expand-file-name file dir))))

(defun log-view-current-entry (&optional pos move)
  "Return the position and revision tag of the Log View entry at POS.
This is a list (BEG TAG), where BEG is a buffer position and TAG
is a string.  If POS is nil or omitted, it defaults to point.
If there is no entry at POS, return nil.

If optional arg MOVE is non-nil, move point to BEG if found.
Otherwise, don't move point."
  (let ((looping t)
	result)
    (save-excursion
      (when pos (goto-char pos))
      (forward-line 0)
      ;; Treat "---" separator lines as part of the following revision.
      (forward-line (if (looking-at "-\\{20,\\}$") 2 1))
      (while looping
	(setq pos (re-search-backward log-view-message-re nil 'move)
	      looping (and pos (log-view-inside-comment-p (point)))))
      (when pos
	(setq result
	      (list pos (match-string-no-properties 1)))))
    (and move result (goto-char pos))
    result))

(defun log-view-inside-comment-p (pos)
  "Return non-nil if POS lies inside an expanded log entry."
  (eq (get-text-property pos 'log-view-comment) t))

(defun log-view-current-tag (&optional pos)
  "Return the revision tag (a string) of the Log View entry at POS.
if POS is omitted or nil, it defaults to point."
  (cadr (log-view-current-entry pos)))

(defun log-view-toggle-mark-entry ()
  "Toggle the marked state for the log entry at point.
See `log-view-mark-entry'."
  (interactive)
  (save-excursion
    (when-let* ((entry (log-view-current-entry)))
      (if (get-char-property (car entry) 'log-view-self)
          (log-view-unmark-entry 1)
        (log-view-mark-entry 1)))))

(defun log-view--mark-unmark (mark-unmark-function arg beg end)
  "Call MARK-UNMARK-FUNCTION on each line of an active region or ARG times.
MARK-UNMARK-FUNCTION should end by advancing point to the next line to
be processed.
The last line of an active region is excluded in the case that the
region ends right at the beginning of the line, or after only non-word
characters."
  (when (xor beg end)
    (error "log-view--mark-unmark called with invalid arguments"))
  (if (and beg end)
      (let ((processed-line nil)
            ;; Exclude the region's last line if the region ends right
            ;; at the beginning of that line or almost at the beginning.
            ;; This is like the `file' value of `dired-mark-region'.
            ;; We don't want to include the last line unless the region
            ;; visually includes that revision.
            (lastl (save-excursion
                     (goto-char end)
                     (skip-syntax-backward "^w")
                     (if (bolp)
                         (1- (line-number-at-pos))
                       (line-number-at-pos)))))
        (save-excursion
          (goto-char beg)
          (while-let ((n (line-number-at-pos))
                      ;; Make sure we don't get stuck processing the
                      ;; same line infinitely.
                      ((<= (line-number-at-pos) lastl))
                      ((not (eq processed-line n))))
            (setq processed-line n)
            (funcall mark-unmark-function)))
        (setq deactivate-mark t))
    (dotimes (_ arg)
      (funcall mark-unmark-function))))

(defun log-view-mark-entry (arg &optional beg end)
  "Mark the log entry at point.
If the region is active in Transient Mark mode, mark all entries.
When called with a prefix argument, mark that many log entries.
When called from Lisp, mark ARG entries or all entries lying between BEG
and END.  If BEG and END are supplied, ARG is ignored.

When entries are marked, some commands that usually operate on the entry
at point will instead operate on all marked entries.
Use \\[log-view-unmark-entry] to unmark an entry.

Lisp programs can use `log-view-get-marked' to obtain a list of all
marked revisions."
  (interactive "p\nR")
  (log-view--mark-unmark #'log-view--mark-entry arg beg end))

(defun log-view--mark-entry ()
  "Mark the log entry at point."
  (when-let* ((entry (log-view-current-entry))
	      (beg (car entry)))
    (save-excursion
      (goto-char beg)
      (unless (get-char-property beg 'log-view-self)
        (let* ((end (if (get-text-property beg 'log-view-entry-expanded)
			(next-single-property-change beg 'log-view-comment)
		      (log-view-end-of-defun)
		      (point)))
	       (ov (make-overlay beg end)))
	  (overlay-put ov 'face 'log-view-file)
	  ;; This is used to check if the overlay is present.
	  (overlay-put ov 'log-view-self ov)
	  (overlay-put ov 'log-view-marked (nth 1 entry)))))
    (log-view-msg-next 1)))

(defun log-view-unmark-entry (arg &optional beg end)
  "Unmark the log entry at point.
If the region is active in Transient Mark mode, unmark all entries.
When called with a prefix argument, unmark that many log entries.
When called from Lisp, mark ARG entries or all entries lying between BEG
and END.  If BEG and END are supplied, ARG is ignored.

See `log-view-mark-entry'."
  (interactive "p\nR")
  (log-view--mark-unmark #'log-view--unmark-entry arg beg end))

(defun log-view--unmark-entry ()
  "Unmark the log entry at point."
  (when-let* ((entry (log-view-current-entry)))
    (when-let* ((found (get-char-property (car entry) 'log-view-self)))
      (delete-overlay found))
    (log-view-msg-next 1)))

(defun log-view-unmark-all-entries ()
  "Unmark all marked log entries in this buffer."
  (interactive)
  (log-view--mark-unmark #'log-view--unmark-entry
                         nil (point-min) (point-max)))

;;;###autoload
(defun log-view-get-marked ()
  "Return the list of tags for the marked log entries."
  (save-excursion
    (let ((pos (point-min))
	  marked-list ov)
      (while (setq pos (next-single-property-change pos 'face))
	(when (setq ov (get-char-property pos 'log-view-self))
	  (push (overlay-get ov 'log-view-marked) marked-list)
	  (setq pos (overlay-end ov))))
      marked-list)))

(defun log-view--restore-marks ()
  "Return a function to restore log entry marks after `revert-buffer'.
Added to `revert-buffer-restore-functions' by Log View mode."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (mark (log-view-get-marked))
      (puthash mark t table))
    (lambda ()
      (vc-run-delayed
        (log-view--mark-unmark (lambda ()
                                 (if (gethash (log-view-current-tag) table)
                                     (log-view--mark-entry)
                                   (log-view-msg-next 1)))
                               nil (point-min) (point-max))))))

(defun log-view-toggle-entry-display ()
  "If possible, expand the current Log View entry.
This calls `log-view-expanded-log-entry-function' to do the work.
See also `log-view-display-entry-and-diff'."
  (interactive)
  ;; Don't do anything unless `log-view-expanded-log-entry-function'
  ;; is defined in this mode.
  (when (functionp log-view-expanded-log-entry-function)
    (let* ((opoint (point))
	   (entry (log-view-current-entry nil t))
	   (beg (car entry))
           (inhibit-read-only t)
	   deactivate-mark)
      (when entry
	(if (get-text-property beg 'log-view-entry-expanded)
	    ;; If the entry is expanded, collapse it.
	    (let ((pos (next-single-property-change beg 'log-view-comment)))
	      (unless (and pos (log-view-inside-comment-p pos))
		(error "Broken markup in `log-view-toggle-entry-display'"))
	      (delete-region pos
                             (or
                              (next-single-property-change pos 'log-view-comment)
                              (point-max)))
	      (put-text-property beg (1+ beg) 'log-view-entry-expanded nil)
	      (if (< opoint pos)
		  (goto-char opoint)))
	  ;; Otherwise, expand the entry.
	  (let ((long-entry (funcall log-view-expanded-log-entry-function
				     (nth 1 entry))))
	    (when long-entry
	      (put-text-property beg (1+ beg) 'log-view-entry-expanded t)
	      (log-view-end-of-defun)
	      (setq beg (point))
	      (insert long-entry "\n")
	      (add-text-properties
	       beg (point)
	       '(font-lock-face log-view-commit-body log-view-comment t))
	      (goto-char opoint))))))))

(defun log-view-display-entry-and-diff ()
  "Expand current Log View entry, if possible, and also display its diff.
If the current Log View entry is already expanded, only display its
diff; this command never collapses entries.
In contrast with `log-view-diff-changeset', the window displaying the
diff is not selected.
See also `log-view-toggle-entry-display'."
  (interactive)
  (when-let* ((beg (car (log-view-current-entry))))
    (unless (get-text-property beg 'log-view-entry-expanded)
      (log-view-toggle-entry-display)))
  (save-selected-window
    (log-view-diff (point) (point))))

(defun log-view-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a Log View entry.
With ARG, do it that many times.  Negative ARG means move forward
to the beginning of the ARGth following entry.

This is Log View mode's default `beginning-of-defun-function'.
It assumes that a log entry starts with a line matching
`log-view-message-re'."
  (when (null arg) (setq arg 1))
  (if (minusp arg)
      ;; In log view, the end of one defun is the beginning of the
      ;; next, so punting to log-view-end-of-defun is safe in this
      ;; context.
      (log-view-end-of-defun (- arg))
    (let ((found t))
      (while (plusp arg)
        (decf arg)
        (let ((cur-start (log-view-current-entry)))
          (setq found (cond ((null cur-start)
                             (goto-char (point-min))
                             nil)
                            ((>= (car cur-start) (point))
                             (unless (bobp)
                               (forward-line -1)
                               (incf arg))
                             nil)
                            (t
                             (goto-char (car cur-start))
                             t)))))
      found)))

(defun log-view-end-of-defun-1 ()
  "Move forward to the next Log View entry."
  (let ((looping t))
    (if (looking-at log-view-message-re)
	(goto-char (match-end 0)))
    (while looping
      (cond
       ((re-search-forward log-view-message-re nil 'move)
	(unless (log-view-inside-comment-p (point))
	  (setq looping nil)
	  (goto-char (match-beginning 0))))
       ;; Don't advance past the end buttons inserted by
       ;; `vc-print-log-setup-buttons'.
       ((looking-back "Show 2X entries    Show unlimited entries"
                      (line-beginning-position))
	(setq looping nil)
	(forward-line -1))
       ;; There are no buttons if we've turned on unlimited entries.
       ((eobp)
        (setq looping nil))))))

(defun log-view-end-of-defun (&optional arg)
  "Move forward to the next Log View entry.
Works like `end-of-defun'."
  (when (null arg) (setq arg 1))
  (if (minusp arg)
      (log-view-beginning-of-defun (- arg))
    (dotimes (_n arg)
      (log-view-end-of-defun-1))))

(defvar cvs-minor-current-files)
(defvar cvs-branch-prefix)
(defvar cvs-secondary-branch-prefix)

(defun log-view-minor-wrap (buf f)
  (let ((data (with-current-buffer buf
		(let* ((beg (point))
		       (end (if (use-region-p) (mark) (point)))
		       (fr (log-view-current-tag beg))
		       (to (log-view-current-tag end)))
		  (when (string-equal fr to)
		    (save-excursion
		      (goto-char end)
		      (log-view-msg-next)
		      (setq to (log-view-current-tag))))
		  (cons
                   ;; The first revision has to be the one at point, for
                   ;; operations that only take one revision
                   ;; (e.g. cvs-mode-edit).
		   (cons (log-view-current-file) fr)
		   (cons (log-view-current-file) to))))))
    (let ((cvs-branch-prefix (cdar data))
	  (cvs-secondary-branch-prefix (and (cdar data) (cddr data)))
	  (cvs-minor-current-files
	   (cons (caar data)
		 (when (and (cadr data) (not (equal (caar data) (cadr data))))
		   (list (cadr data)))))
	  ;; FIXME:  I need to force because the fileinfos are UNKNOWN
	  (cvs-force-command "/F"))
      (funcall f))))

(defun log-view-find-revision (pos)
  "Visit the version at POS.
If called interactively, visit the version at point."
  (interactive "d")
  (unless log-view-per-file-logs
    (when (or (> (length log-view-vc-fileset) 1)
              (null (car log-view-vc-fileset))
              (file-directory-p (car log-view-vc-fileset)))
      (user-error "Multiple files shown in this buffer, cannot use this command here")))
  (save-excursion
    (goto-char pos)
    (switch-to-buffer (vc-find-revision (if log-view-per-file-logs
					    (log-view-current-file)
					  (car log-view-vc-fileset))
					(log-view-current-tag)
                                        log-view-vc-backend))))


(defun log-view-extract-comment ()
  "Parse comment from around the current point in the log."
  (save-excursion
    (let (st en (backend (vc-backend (log-view-current-file))))
      (log-view-end-of-defun)
      (cond ((eq backend 'SVN)
	     (forward-line -1)))
      (setq en (point))
      (or (log-view-current-entry nil t)
          (throw 'beginning-of-buffer nil))
      (cond ((memq backend '(SCCS RCS CVS SVN))
	     (forward-line 2))
	    ((eq backend 'Hg)
	     (forward-line 4)
	     (re-search-forward "summary: *" nil t)))
      (setq st (point))
      (buffer-substring st en))))

(declare-function vc-modify-change-comment "vc" (files rev oldcomment))

(defun log-view-modify-change-comment ()
  "Edit the change comment displayed at point."
  (interactive)
  (let* ((files (list (if log-view-per-file-logs
			  (log-view-current-file)
		        (car log-view-vc-fileset))))
         (rev (log-view-current-tag))
         ;; `log-view-extract-comment' is the legacy code for this; the
         ;; `get-change-comment' backend action is the new way to do it.
         ;;
         ;; FIXME: Eventually the older backends should have
         ;; implementations of `get-change-comment' because that ought
         ;; to be more robust than the approach taken by
         ;; `log-view-extract-comment'.  Then we can delete the latter.
         ;; See discussion in bug#64055.  --spwhitton
         ;;
         ;; FIXME: We should implement backend actions
         ;; `get-change-comment' and `modify-change-comment' for bzr and
         ;; Hg, so that this command works for those backends.
         ;; As discussed in bug#64055, `get-change-comment' is required,
         ;; and parsing the old comment out of the Log View buffer will
         ;; not do.  This is because for these backends there are
         ;; `vc-*-log-switches' variables which can change what gets put
         ;; in the Log View buffers and break any Lisp parsing attempt.
         (comment (condition-case _
                      (vc-call-backend log-view-vc-backend
                                       'get-change-comment files rev)
                    (vc-not-supported (log-view-extract-comment)))))
    (when (memq 'log-edit-insert-message-template log-edit-hook)
      (let* ((first-newline (string-match "\n" comment))
             (summary (substring comment 0 first-newline))
             (rest (and first-newline
                        (substring comment (1+ first-newline)))))
        (setq comment
              ;; As we are part of the VC subsystem I think we are
              ;; entitled to call a \\`log-edit--' function.
              ;; --spwhitton
              (concat (log-edit--make-header-line "Summary" summary)
                      (if (length> rest 0) rest "\n")))))
    (vc-modify-change-comment files rev comment)))

(defun log-view-annotate-version (pos)
  "Annotate the version at POS.
If called interactively, annotate the version at point."
  (interactive "d")
  (unless log-view-per-file-logs
    (when (or (> (length log-view-vc-fileset) 1)
              (null (car log-view-vc-fileset))
              (file-directory-p (car log-view-vc-fileset)))
      (user-error "Multiple files shown in this buffer, cannot use this command here")))
  (save-excursion
    (goto-char pos)
    (vc-annotate (if log-view-per-file-logs
		     (log-view-current-file)
		   (car log-view-vc-fileset))
		 (log-view-current-tag)
                 nil nil nil log-view-vc-backend)))

;;;;
;;;; Cherry-picks and reverts
;;;;

(defvar vc-parent-buffer-name)
(defvar vc-log-short-style)
(declare-function vc-print-log-internal "vc")

(defun log-view--pick-or-revert
    (directory no-comment reverse interactive delete)
  "Copy changes from revision at point or all marked revisions.
DIRECTORY is the destination, the root of the target working tree.
NO-COMMENT non-nil means use the log messages of the revisions
unmodified, instead of using the backend's default cherry-pick comment
for that revision.
NO-COMMENT non-nil with zero or one revisions marked also means don't
prompt to edit the log message.
REVERSE non-nil means to undo the effects of the revisions, instead.
INTERACTIVE and DELETE are passed on to `vc--pick-or-revert', except
additionally if INTERACTIVE is non-nil and `vc--pick-or-revert' returns
`deleted', pass `no-confirm' to subsequent calls to that function."
  (let ((default-directory directory)
        (marked (log-view-get-marked))
        (buf (current-buffer)))
    (if (length> marked 1)
        (let ((deleted 0))
          (save-excursion
            (dolist (rev (if reverse (reverse marked) marked))
              ;; Unmark each revision *before* copying it.
              ;; Then if there is a conflict such that a cherry-pick
              ;; fails, after resolving that conflict and committing the
              ;; cherry-pick, the right revisions will be marked to
              ;; resume the original multiple cherry-pick operation.
              ;; FIXME: This doesn't work so long as the backend
              ;; functions just give up completely if there is a
              ;; conflict (which behavior is also a FIXME).  Then in
              ;; fact the user has to mark the revision again.
              (log-view-goto-rev rev)
              (log-view-unmark-entry 1)
              (cl-case
                  (vc--pick-or-revert rev reverse interactive delete
                                      (if no-comment
                                          (vc-call-backend log-view-vc-backend
                                                           'get-change-comment
                                                           nil rev)
                                        t)
                                      nil
                                      log-view-vc-backend)
                (deleted (incf deleted)
                         (when interactive
                           (setq interactive 'no-confirm))))))
          (let ((new-commits (- (length marked) deleted)))
            (when (and (plusp new-commits)
                       (vc-find-backend-function log-view-vc-backend
                                                 'modify-change-comment))
              (let (vc-log-short-style)
                (vc-print-log-internal log-view-vc-backend
                                       (list default-directory)
                                       nil nil new-commits))
              (setq-local vc-log-short-style nil ; For \\`g'.
                          vc-parent-buffer-name nil)
              (message (substitute-command-keys "Use \
\\[log-view-modify-change-comment] to modify any of these messages")))))
      (let ((rev (or (car marked) (log-view-current-tag))))
        (vc--pick-or-revert rev reverse interactive delete
                            (and no-comment
                                 (vc-call-backend log-view-vc-backend
                                                  'get-change-comment
                                                  nil rev))
                            nil
                            log-view-vc-backend)))
    (when (eq (current-buffer) buf)
      (revert-buffer))))

(defun log-view-cherry-pick (directory &optional no-comment)
  "Copy changes from revision at point to current branch.
If there are marked revisions, use those instead of the revision at point.

When called interactively, prompts for the target working tree to which
to copy the revision(s); the current working tree is the default choice.
When called from Lisp, DIRECTORY is the root of the target working tree.

When copying a single revision, prompts for editing the log message for
the new commit, except with optional argument NO-COMMENT non-nil
(interactively, with a prefix argument).
When copying multiple revisions, never prompts to edit log messages.

Normally a log message for each new commit is generated by the backend,
including references to the source commits so that the copy can be
traced.  With optional argument NO-COMMENT non-nil (interactively, with
a prefix argument), use the log messages from the source revisions
unmodified.

See also `vc-cherry-pick'."
  (interactive
   (list (vc--prompt-other-working-tree log-view-vc-backend
                                        "Cherry-pick to working tree"
                                        'allow-empty)
         current-prefix-arg))
  (log-view--pick-or-revert directory no-comment nil nil nil))

(defun log-view-revert-or-delete-revisions
    (directory &optional interactive delete)
  "Undo the effects of the revision at point.
When revisions are marked, undo the effects of each of them.
When called interactively, prompts for the target working tree in which
to revert; the current working tree is the default choice.
When called from Lisp, DIRECTORY is the root of the target working tree.

When called interactively (or with optional argument INTERACTIVE
non-nil), then if the underlying VCS is distributed, offer to entirely
delete revisions that have not been pushed.
This is instead of creating new commits undoing their effects.

With a prefix argument (or with optional argument DELETE non-nil),
always delete revisions and never create new commits.
In this case INTERACTIVE is ignored.
This works only for unpublished commits, unless you have customized
`vc-allow-rewriting-published-history' to a non-nil value.

When reverting a single revision, prompts for editing the log message
for the new commit.
When reverting multiple revisions, never prompts to edit log messages.

See also `vc-revert-or-delete-revision'."
  (interactive (list (vc--prompt-other-working-tree
                      (vc-responsible-backend default-directory)
                      (if current-prefix-arg "Delete in working tree"
                        "Revert in working tree")
                      'allow-empty)
                     t current-prefix-arg))
  (log-view--pick-or-revert directory nil t interactive delete))

(defun log-view-uncommit-revisions-from-end ()
  "Uncommit revisions newer than the revision at point.
The revision at point must be on the current branch.  The newer
revisions are deleted from the revision history but the changes made by
those revisions to files in the working tree are not undone.

To delete revisions from the revision history and also undo the changes
in the working tree, see `log-edit-delete-revisions-from-end'."
  (interactive)
  (vc--remove-revisions-from-end (log-view-current-tag)
                                 nil t log-view-vc-backend)
  (revert-buffer))

(defun log-view-delete-revisions-from-end (&optional discard)
  "Delete revisions newer than the revision at point.
The revision at point must be on the current branch.  The newer
revisions are deleted from the revision history and the changes made by
those revisions to files in the working tree are undone.
If the are uncommitted changes, prompts to discard them.
With a prefix argument (when called from Lisp, with optional argument
DISCARD non-nil), discard any uncommitted changes without prompting.

To delete revisions from the revision history without undoing the
changes in the working tree, see `log-edit-uncommit-revisions-from-end'."
  (interactive "P")
  (vc--remove-revisions-from-end (log-view-current-tag)
                                 (if discard 'discard t)
                                 t log-view-vc-backend)
  (revert-buffer))

;; These are left unbound by default.  A user who doesn't like the DWIM
;; behavior of `log-view-revert-or-delete-revisions' can unbind that and
;; bind these two commands instead.

(defun log-view-revert-revisions (directory)
  "Make a commit undoing the effects of the revision at point.
When revisions are marked, make such a commit for each of them.
When called interactively, prompts for the target working tree in which
to make new commits; the current working tree is the default choice.
When called from Lisp, DIRECTORY is the root of the target working tree.

This is like `log-view-revert-or-delete-revisions' except that it only
ever makes new commits undoing the effects of revisions, instead of
considering VCS-specific alternative mechanisms to undo the effects of
revisions.

When reverting a single revision, prompts for editing the log message
for the new commit.
When reverting multiple revisions, never prompts to edit log messages.

See also `vc-revert-revision'."
  (interactive (list (vc--prompt-other-working-tree
                      (vc-responsible-backend default-directory)
                      "Revert in working tree"
                      'allow-empty)))
  (log-view--pick-or-revert directory nil t nil 'never))

(defun log-view-delete-revisions (directory)
  "Delete the revision at point from the revision history.
When revisions are marked, delete all of them.
When called interactively, prompts for the target working tree in which
to delete revisions; the current working tree is the default choice.
When called from Lisp, DIRECTORY is the root of the target working tree.

This works only for unpublished commits, unless you have customized
`vc-allow-rewriting-published-history' to a non-nil value.

This is the same as `log-view-revert-or-delete-revisions' invoked
interactively with a prefix argument.  See also `vc-delete-revision'."
  (interactive (list (vc--prompt-other-working-tree
                      (vc-responsible-backend default-directory)
                      "Delete in working tree"
                      'allow-empty)))
  (log-view--pick-or-revert directory nil t nil t))

;;;;
;;;; diff
;;;;

(defun log-view-diff (beg end)
  "Get the diff between two revisions.
If the region is inactive or the mark is on the revision at
point, get the diff between the revision at point and its
previous revision.  Otherwise, get the diff between the revisions
where the region starts and ends.

Unlike `log-view-diff-changeset', this function only shows the
part of the changeset which affected the currently considered
file(s)."
  (interactive
   (list (if (use-region-p) (region-beginning) (point))
         (if (use-region-p) (region-end) (point))))
  (log-view-diff-common beg end))

(defun log-view-diff-changeset (beg end)
  "Get the diff between two revisions.
If the region is inactive or the mark is on the revision at
point, get the diff between the revision at point and its
previous revision.  Otherwise, get the diff between the revisions
where the region starts and ends.

Unlike `log-view-diff' this function shows the whole changeset,
including changes affecting other files than the currently
considered file(s)."
  (interactive
   (list (if (use-region-p) (region-beginning) (point))
         (if (use-region-p) (region-end) (point))))
  (when (eq (vc-call-backend log-view-vc-backend 'revision-granularity) 'file)
    (error "The %s backend does not support changeset diffs" log-view-vc-backend))
  (let ((default-directory (vc-root-dir)))
    (log-view-diff-common beg end t)))

(defun log-view-diff-common (beg end &optional whole-changeset)
  (defvar vc-allow-async-diff)
  (let* ((to (log-view-current-tag beg))
         (fr-entry (log-view-current-entry end))
         (fr (cadr fr-entry)))
    ;; When TO and FR are the same, or when point is on a line after
    ;; the last entry, look at the previous revision.
    (when (or (string-equal fr to)
              (>= end
                  (save-excursion
                    (goto-char end)
                    (log-view-end-of-defun)
                    (point))))
      (setq fr (vc-call-backend log-view-vc-backend 'previous-revision nil fr)))
    (vc-diff-internal
     vc-allow-async-diff
     (list log-view-vc-backend
           ;; The value passed here should follow what
           ;; `vc-deduce-fileset' returns.  If we want to see the
           ;; diff for all the files in the changeset, pass NIL for
           ;; the file list.
           (unless whole-changeset
             (if log-view-per-file-logs
                 (list (log-view-current-file))
               log-view-vc-fileset)))
     fr to)))

(defun log-view-copy-revision-as-kill ()
  "Copy the ID of the revision at point to the kill ring.
If there are marked revisions, copy the IDs of those, separated by spaces."
  (interactive)
  (let ((revisions (log-view-get-marked)))
    (if (length> revisions 1)
        (let ((found (string-join revisions " ")))
          (kill-new found)
          (message "%s" found))
      (if-let* ((rev (or (car revisions) (log-view-current-tag))))
          (progn (kill-new rev)
                 (message "%s" rev))
        (user-error "No revision at point")))))

(provide 'log-view)

;;; log-view.el ends here
