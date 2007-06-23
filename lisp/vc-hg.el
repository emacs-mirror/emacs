;;; vc-hg.el --- VC backend for the mercurial version control system

;; Copyright (C) 2006, 2007 Free Software Foundation, Inc.

;; Author: Ivan Kanis
;; Keywords: tools
;; Version: 1889

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;; This is a mercurial version control backend

;;; Thanks:

;;; Bugs:

;;; Installation:

;;; Todo:

;; Implement the rest of the vc interface:
;; - dired
;; - snapshot?

;; Implement Stefan Monnier's advice:
;; vc-hg-registered and vc-hg-state
;; Both of those functions should be super extra careful to fail gracefully in
;; unexpected circumstances.  The most important such case is when the `hg'
;; executable is not available.  The reason this is important is that any error
;; there will prevent the user from even looking at the file :-(
;; Ideally, just like in vc-arch and vc-cvs, checking that the file is under
;; mercurial's control and extracting the current revision should be done
;; without even using `hg' (this way even if you don't have `hg' installed,
;; Emacs is able to tell you this file is under mercurial's control).

;;; History:
;;

;;; Code:

(eval-when-compile
  (require 'vc))

;; XXX This should be moved to vc-hooks when we can be sure that vc-state
;; and friends are always harmless.
(add-to-list 'vc-handled-backends 'HG)

;;; Customization options

(defcustom vc-hg-global-switches nil
  "*Global switches to pass to any Hg command."
  :type '(choice (const :tag "None" nil)
         (string :tag "Argument String")
         (repeat :tag "Argument List"
             :value ("")
             string))
;;  :version "22.2"
  :group 'vc)

;;; State querying functions

;;;###autoload (defun vc-hg-registered (file)
;;;###autoload   "Return non-nil if FILE is registered with hg."
;;;###autoload   (if (vc-find-root file ".hg")       ; short cut
;;;###autoload       (progn
;;;###autoload         (load "vc-hg")
;;;###autoload         (vc-hg-registered file))))

;; Modelled after the similar function in vc-bzr.el
(defun vc-hg-registered (file)
  "Return non-nil if FILE is registered with hg."
  (if (vc-find-root file ".hg")       ; short cut
      (vc-hg-state file)))            ; expensive

(defun vc-hg-state (file)
  "Hg-specific version of `vc-state'."
  (let ((out (vc-hg-internal-status file)))
    (if (eq 0 (length out)) 'up-to-date
      (let ((state (aref out 0)))
        (cond
         ((eq state ?M) 'edited)
         ((eq state ?A) 'edited)
         ((eq state ?P) 'needs-patch)
	 ((eq state ??) nil)
         (t 'up-to-date))))))

(defun vc-hg-workfile-version (file)
  "Hg-specific version of `vc-workfile-version'."
  (let ((out (vc-hg-internal-log file)))
    (if (string-match "changeset: *\\([0-9]*\\)" out)
        (match-string 1 out)
      "0")))

;;; History functions

(defun vc-hg-print-log(file &optional buffer)
  "Get change log associated with FILE."
  ;; `log-view-mode' needs to have the file name in order to function
  ;; correctly. "hg log" does not print it, so we insert it here by
  ;; hand.

  ;; `vc-do-command' creates the buffer, but we need it before running
  ;; the command.
  (vc-setup-buffer buffer)
  ;; If the buffer exists from a previous invocation it might be
  ;; read-only.
  (let ((inhibit-read-only t))
    (with-current-buffer
	buffer
      (insert "File:        " (file-name-nondirectory file) "\n")))
  (vc-hg-command
   buffer
   (if (and (vc-stay-local-p file) (fboundp 'start-process)) 'async 0)
   file "log"))

(defvar log-view-message-re)
(defvar log-view-file-re)
(defvar log-view-font-lock-keywords)

(define-derived-mode vc-hg-log-view-mode log-view-mode "HG-Log-View"
  (require 'add-log) ;; we need the faces add-log
  ;; Don't have file markers, so use impossible regexp.
  (set (make-local-variable 'log-view-file-re) "^File:[ \t]+\\(.+\\)")
  (set (make-local-variable 'log-view-message-re)
       "^changeset:[ \t]*\\([0-9]+\\):\\(.+\\)")
  (set (make-local-variable 'log-view-font-lock-keywords)
       (append
        (copy-alist log-view-font-lock-keywords)
	;; Handle the case:
	;; user: foo@bar
	'(("^user:[ \t]+\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)"
	   (1 'change-log-email))
	  ;; Handle the case:
	  ;; user: FirstName LastName <foo@bar>
	  ("^user:[ \t]+\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]"
	   (1 'change-log-name)
	   (2 'change-log-email))
	  ("^date: \\(.+\\)" (1 'change-log-date))
	  ("^summary:[ \t]+\\(.+\\)" (1 'log-view-message)))))))

(defun vc-hg-diff (file &optional oldvers newvers buffer)
  "Get a difference report using hg between two versions of FILE."
  (let ((working (vc-workfile-version file)))
    (if (and (equal oldvers working) (not newvers))
	(setq oldvers nil))
    (if (and (not oldvers) newvers)
	(setq oldvers working))
    (apply 'call-process "hg" nil (or buffer "*vc-diff*") nil
	   "--cwd" (file-name-directory file) "diff"
	   (append
	    (if oldvers
		(if newvers
		    (list "-r" oldvers "-r" newvers)
		  (list "-r" oldvers))
	      (list ""))
            (list (file-name-nondirectory file))))))

(defun vc-hg-annotate-command (file buffer &optional version)
  "Execute \"hg annotate\" on FILE, inserting the contents in BUFFER.
Optional arg VERSION is a version to annotate from."
  (vc-hg-command buffer 0 file "annotate" "-d" "-n" (if version (concat "-r" version)))
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "^[0-9]")
    (delete-region (point-min) (1- (point)))))


;; The format for one line output by "hg annotate -d -n" looks like this:
;;215 Wed Jun 20 21:22:58 2007 -0700: CONTENTS
;; i.e: VERSION_NUMBER DATE: CONTENTS
(defconst vc-hg-annotate-re "^[ \t]*\\([0-9]+\\) \\(.\\{30\\}\\): ")

(defun vc-hg-annotate-time ()
  (when (looking-at vc-hg-annotate-re)
    (goto-char (match-end 0))
    (vc-annotate-convert-time
     (date-to-time (match-string-no-properties 2)))))

(defun vc-hg-annotate-extract-revision-at-line ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at vc-hg-annotate-re) (match-string-no-properties 1))))

(defun vc-hg-previous-version (file rev)
  (let ((newrev (1- (string-to-number rev))))
    (when (>= newrev 0)
      (number-to-string newrev))))

(defun vc-hg-register (file &optional rev comment)
  "Register FILE under hg.
REV is ignored.
COMMENT is ignored."
  (vc-hg-command nil nil file "add"))

(defun vc-hg-checkin (file rev comment)
  "HG-specific version of `vc-backend-checkin'.
REV is ignored."
  (vc-hg-command nil nil file  "commit" "-m" comment))

;; Modelled after the similar function in vc-bzr.el
(defun vc-hg-checkout (file &optional editable rev workfile)
  "Retrieve a revision of FILE into a WORKFILE.
EDITABLE is ignored.
REV is the revision to check out into WORKFILE."
  (unless workfile
    (setq workfile (vc-version-backup-file-name file rev)))
  (let ((coding-system-for-read 'binary)
        (coding-system-for-write 'binary))
  (with-temp-file workfile
    (if rev
        (vc-hg-command t nil file "cat" "-r" rev)
      (vc-hg-command t nil file "cat")))))

(defun vc-hg-checkout-model (file)
  'implicit)

;;; Internal functions

(defun vc-hg-command (buffer okstatus file &rest flags)
  "A wrapper around `vc-do-command' for use in vc-hg.el.
The difference to vc-do-command is that this function always invokes `hg',
and that it passes `vc-hg-global-switches' to it before FLAGS."
  (apply 'vc-do-command buffer okstatus "hg" file
         (if (stringp vc-hg-global-switches)
             (cons vc-hg-global-switches flags)
           (append vc-hg-global-switches
                   flags))))

(defun vc-hg-internal-log (file &optional buffer)
  "Return log of FILE."
  (with-output-to-string
    (with-current-buffer
        standard-output
      (call-process
       "hg" nil t nil "--cwd" (file-name-directory file)
       "log" "-l1" (file-name-nondirectory file)))))

(defun vc-hg-internal-status(file)
  "Return status of FILE."
  (with-output-to-string
    (with-current-buffer
        standard-output
      (call-process
       "hg" nil t nil "--cwd" (file-name-directory file)
       "status" (file-name-nondirectory file)))))

(provide 'vc-hg)

;;; vc-hg.el ends here
