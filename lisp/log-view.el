;;; log-view.el --- Major mode for browsing RCS/CVS/SCCS log output

;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: rcs sccs cvs log version-control

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

;; Major mode to browse revision log histories.
;; Currently supports the format output by:
;;  RCS, SCCS, CVS, Subversion, and DaRCS.

;; Examples of log output:

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

;;;; SCCS:

;;;; Subversion:

;; ------------------------------------------------------------------------
;; r4622 | ckuethe | 2007-12-23 18:18:01 -0500 (Sun, 23 Dec 2007) | 2 lines
;; 
;; uBlox AEK-4T in binary mode. Added to unstable because it breaks gpsfake
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

(eval-when-compile (require 'cl))
(require 'pcvs-util)
(autoload 'vc-find-revision "vc")
(autoload 'vc-version-diff "vc")

(defvar cvs-minor-wrap-function)

(defgroup log-view nil
  "Major mode for browsing log output of RCS/CVS/SCCS."
  :group 'pcl-cvs
  :prefix "log-view-")

(easy-mmode-defmap log-view-mode-map
  '(("q" . quit-window)
    ("z" . kill-this-buffer)
    ("m" . log-view-toggle-mark-entry)
    ("e" . log-view-modify-change-comment)
    ("d" . log-view-diff)
    ("a" . log-view-annotate-version)
    ("f" . log-view-find-revision)
    ("n" . log-view-msg-next)
    ("p" . log-view-msg-prev)
    ("\t" . log-view-msg-next)
    ([backtab] . log-view-msg-prev)
    ("N" . log-view-file-next)
    ("P" . log-view-file-prev)
    ("\M-n" . log-view-file-next)
    ("\M-p" . log-view-file-prev))
  "Log-View's keymap."
  :group 'log-view
  ;; Here I really need either buffer-local keymap-inheritance
  ;; or a minor-mode-map with lower precedence than the local map.
  :inherit (if (boundp 'cvs-mode-map) cvs-mode-map))

(easy-menu-define log-view-mode-menu log-view-mode-map
  "Log-View Display Menu"
  `("Log-View"
    ;; XXX Do we need menu entries for these?
    ;; ["Quit"  quit-window]
    ;; ["Kill This Buffer"  kill-this-buffer]
    ["Mark Log Entry for Diff"  set-mark-command
     :help ""]
    ["Diff Revisions"  log-view-diff
     :help "Get the diff between two revisions"]
    ["Visit Version"  log-view-find-revision
     :help "Visit the version at point"]
    ["Annotate Version"  log-view-annotate-version
     :help "Annotate the version at point"]
    ["Modify Log Comment" log-view-modify-change-comment
     :help "Edit the change comment displayed at point"]
    "-----"
    ["Next Log Entry"  log-view-msg-next
     :help "Go to the next count'th log message"]
    ["Previous Log Entry"  log-view-msg-prev
     :help "Go to the previous count'th log message"]
    ["Next File"  log-view-file-next
     :help "Go to the next count'th file"]
    ["Previous File"  log-view-file-prev
     :help "Go to the previous count'th file"]))

(defvar log-view-mode-hook nil
  "Hook run at the end of `log-view-mode'.")

(defface log-view-file
  '((((class color) (background light))
     (:background "grey70" :weight bold))
    (t (:weight bold)))
  "Face for the file header line in `log-view-mode'."
  :group 'log-view)
;; backward-compatibility alias
(put 'log-view-file-face 'face-alias 'log-view-file)
(defvar log-view-file-face 'log-view-file)

(defface log-view-message
  '((((class color) (background light))
     (:background "grey85"))
    (t (:weight bold)))
  "Face for the message header line in `log-view-mode'."
  :group 'log-view)
;; backward-compatibility alias
(put 'log-view-message-face 'face-alias 'log-view-message)
(defvar log-view-message-face 'log-view-message)

(defvar log-view-file-re
  (concat "^\\(?:Working file: \\(?1:.+\\)"                ;RCS and CVS.
          ;; Subversion has no such thing??
          "\\|\\(?:SCCS/s\\.\\|Changes to \\)\\(?1:.+\\):" ;SCCS and Darcs.
	  "\\)\n")                    ;Include the \n for font-lock reasons.
  "Regexp matching the text identifying the file.
The match group number 1 should match the file name itself.")

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
              (0 log-view-file-face append)))
    (eval . `(,log-view-message-re . log-view-message-face))))

(defconst log-view-font-lock-defaults
  '(log-view-font-lock-keywords t nil nil nil))

;;;;
;;;; Actual code
;;;;

;;;###autoload
(define-derived-mode log-view-mode fundamental-mode "Log-View"
  "Major mode for browsing CVS log output."
  (setq buffer-read-only t)
  (set (make-local-variable 'font-lock-defaults) log-view-font-lock-defaults)
  (set (make-local-variable 'beginning-of-defun-function) 
       'log-view-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 
       'log-view-end-of-defun)
  (set (make-local-variable 'cvs-minor-wrap-function) 'log-view-minor-wrap))

;;;;
;;;; Navigation
;;;;

;; define log-view-{msg,file}-{next,prev}
(easy-mmode-define-navigation log-view-msg log-view-message-re "log message")
(easy-mmode-define-navigation log-view-file log-view-file-re "file")

(defun log-view-goto-rev (rev)
  (goto-char (point-min))
  (ignore-errors
    (while (not (equal rev (log-view-current-tag)))
      (log-view-msg-next))
    t))

;;;;
;;;; Linkage to PCL-CVS (mostly copied from cvs-status.el)
;;;;

(defconst log-view-dir-re "^cvs[.ex]* [a-z]+: Logging \\(.+\\)$")

(defun log-view-current-file ()
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

(defun log-view-current-tag (&optional where)
  (save-excursion
    (when where (goto-char where))
    (forward-line 1)
    (let ((pt (point)))
      (when (re-search-backward log-view-message-re nil t)
	(let ((rev (match-string-no-properties 1)))
	  (unless (re-search-forward log-view-file-re pt t)
	    rev))))))

(defun log-view-toggle-mark-entry ()
  "Toggle the marked state for the log entry at point.
Individual log entries can be marked and unmarked. The marked
entries are denoted by changing their background color.
`log-view-get-marked' returns the list of tags for the marked
log entries."
  (interactive)
  (save-excursion
    (forward-line 1)
    (let ((pt (point)))
      (when (re-search-backward log-view-message-re nil t)
	(let ((beg (match-beginning 0))
	      end ov ovlist found tag)
	  (unless (re-search-forward log-view-file-re pt t)
	    ;; Look to see if the current entry is marked.
	    (setq found (get-char-property (point) 'log-view-self))
	    (if found
		(delete-overlay found)
	      ;; Create an overlay that covers this entry and change
	      ;; its color.
	      (setq tag (log-view-current-tag (point)))
	      (forward-line 1)
	      (setq end
		    (if (re-search-forward log-view-message-re nil t)
			(match-beginning 0)
		      (point-max)))
	      (setq ov (make-overlay beg end))
	      (overlay-put ov 'face 'log-view-file)
	      ;; This is used to check if the overlay is present.
	      (overlay-put ov 'log-view-self ov)
	      (overlay-put ov 'log-view-marked tag))))))))

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

(defun log-view-beginning-of-defun ()
  ;; This assumes that a log entry starts with a line matching
  ;; `log-view-message-re'.  Modes that derive from `log-view-mode'
  ;; for which this assumption is not valid will have to provide
  ;; another implementation of this function.  `log-view-msg-prev'
  ;; does a similar job to this function, we can't use it here
  ;; directly because it prints messages that are not appropriate in
  ;; this context and it does not move to the beginning of the buffer
  ;; when the point is before the first log entry.

  ;; `log-view-beginning-of-defun' and `log-view-end-of-defun' have
  ;; been checked to work with logs produced by RCS, CVS, git,
  ;; mercurial and subversion.

  (re-search-backward log-view-message-re nil 'move))

(defun log-view-end-of-defun ()
  ;; The idea in this function is to search for the beginning of the
  ;; next log entry using `log-view-message-re' and then go back one
  ;; line when finding it.  Modes that derive from `log-view-mode' for
  ;; which this assumption is not valid will have to provide another
  ;; implementation of this function.

  ;; Look back and if there is no entry there it means we are before
  ;; the first log entry, so go forward until finding one.
  (unless (save-excursion (re-search-backward log-view-message-re nil t))
    (re-search-forward log-view-message-re nil t))

  ;; In case we are at the end of log entry going forward a line will
  ;; make us find the next entry when searching. If we are inside of
  ;; an entry going forward a line will still keep the point inside
  ;; the same entry.
  (forward-line 1)

  ;; In case we are at the beginning of an entry, move past it.
  (when (looking-at log-view-message-re)
    (goto-char (match-end 0))
    (forward-line 1))

  ;; Search for the start of the next log entry.  Go to the end of the
  ;; buffer if we could not find a next entry.
  (when (re-search-forward log-view-message-re nil 'move)
    (goto-char (match-beginning 0))
    (forward-line -1)))

(defvar cvs-minor-current-files)
(defvar cvs-branch-prefix)
(defvar cvs-secondary-branch-prefix)

(defun log-view-minor-wrap (buf f)
  (let ((data (with-current-buffer buf
		(let* ((beg (point))
		       (end (if mark-active (mark) (point)))
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
  "Visit the version at point."
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (switch-to-buffer (vc-find-revision (log-view-current-file)
                                       (log-view-current-tag)))))


(defun log-view-extract-comment ()
  "Parse comment from around the current point in the log."
  (save-excursion
    (let (st en (backend (vc-backend (log-view-current-file))))
      (log-view-end-of-defun)
      (cond ((eq backend 'SVN)
	     (forward-line -1)))
      (setq en (point))
      (log-view-beginning-of-defun)
      (cond ((memq backend '(SCCS RCS CVS MCVS SVN))
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
  (vc-modify-change-comment (list (log-view-current-file))
			  (log-view-current-tag)
			  (log-view-extract-comment)))

(defun log-view-annotate-version (pos)
  "Annotate the version at point."
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (switch-to-buffer (vc-annotate (log-view-current-file)
				   (log-view-current-tag)))))

;;
;; diff
;;

(defun log-view-diff (beg end)
  "Get the diff between two revisions.
If the mark is not active or the mark is on the revision at point,
get the diff between the revision at point and its previous revision.
Otherwise, get the diff between the revisions where the region starts
and ends."
  (interactive
   (list (if mark-active (region-beginning) (point))
         (if mark-active (region-end) (point))))
  (let ((fr (log-view-current-tag beg))
        (to (log-view-current-tag end)))
    (when (string-equal fr to)
      (save-excursion
        (goto-char end)
        (log-view-msg-next)
        (setq to (log-view-current-tag))))
    (vc-version-diff (list (log-view-current-file)) to fr)))

(provide 'log-view)

;; arch-tag: 0d64220b-ce7e-4f62-9c2a-6b04c2f81f4f
;;; log-view.el ends here
