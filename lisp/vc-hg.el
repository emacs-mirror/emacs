;;; vc-hg.el --- VC backend for the mercurial version control system

;; Copyright (C) 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Ivan Kanis
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

;; This is a mercurial version control backend

;;; Thanks:

;;; Bugs:

;;; Installation:

;;; Todo:

;; Implement the rest of the vc interface. See the comment at the
;; beginning of vc.el. The current status is:

;; FUNCTION NAME                               STATUS
;; BACKEND PROPERTIES
;; * revision-granularity                      OK
;; STATE-QUERYING FUNCTIONS
;; * registered (file)                         OK
;; * state (file)                              OK
;; - state-heuristic (file)                    ?? PROBABLY NOT NEEDED
;; - dir-state (dir)                           OK
;; * working-revision (file)                   OK
;; - latest-on-branch-p (file)                 ??
;; * checkout-model (file)                     OK
;; - workfile-unchanged-p (file)               OK
;; - mode-line-string (file)                   NOT NEEDED
;; - dired-state-info (file)                   OK
;; STATE-CHANGING FUNCTIONS
;; * register (files &optional rev comment)    OK
;; * create-repo ()                            OK
;; - init-revision ()                           NOT NEEDED
;; - responsible-p (file)                      OK
;; - could-register (file)                     OK
;; - receive-file (file rev)                   ?? PROBABLY NOT NEEDED
;; - unregister (file)                         COMMENTED OUT, MAY BE INCORRECT
;; * checkin (files rev comment)               OK
;; * find-revision (file rev buffer)            OK
;; * checkout (file &optional editable rev)    OK
;; * revert (file &optional contents-done)     OK
;; - rollback (files)                          ?? PROBABLY NOT NEEDED
;; - merge (file rev1 rev2)                    NEEDED
;; - merge-news (file)                         NEEDED
;; - steal-lock (file &optional revision)       NOT NEEDED
;; HISTORY FUNCTIONS
;; * print-log (files &optional buffer)        OK
;; - log-view-mode ()                          OK
;; - show-log-entry (revision)                  NOT NEEDED, DEFAULT IS GOOD
;; - wash-log (file)                           ??
;; - logentry-check ()                         NOT NEEDED
;; - comment-history (file)                    NOT NEEDED
;; - update-changelog (files)                  NOT NEEDED
;; * diff (files &optional rev1 rev2 buffer)   OK
;; - revision-completion-table (files)         OK?
;; - annotate-command (file buf &optional rev) OK
;; - annotate-time ()                          OK
;; - annotate-current-time ()                  ?? NOT NEEDED
;; - annotate-extract-revision-at-line ()      OK
;; SNAPSHOT SYSTEM
;; - create-snapshot (dir name branchp)        NEEDED (probably branch?)
;; - assign-name (file name)                   NOT NEEDED
;; - retrieve-snapshot (dir name update)       ?? NEEDED??
;; MISCELLANEOUS
;; - make-version-backups-p (file)             ??
;; - repository-hostname (dirname)             ??
;; - previous-revision (file rev)               OK
;; - next-revision (file rev)                   OK
;; - check-headers ()                          ??
;; - clear-headers ()                          ??
;; - delete-file (file)                        TEST IT
;; - rename-file (old new)                     OK
;; - find-file-hook ()                         PROBABLY NOT NEEDED
;; - find-file-not-found-hook ()               PROBABLY NOT NEEDED

;; Implement Stefan Monnier's advice:
;; vc-hg-registered and vc-hg-state
;; Both of those functions should be super extra careful to fail gracefully in
;; unexpected circumstances. The reason this is important is that any error
;; there will prevent the user from even looking at the file :-(
;; Ideally, just like in vc-arch and vc-cvs, checking that the file is under
;; mercurial's control and extracting the current revision should be done
;; without even using `hg' (this way even if you don't have `hg' installed,
;; Emacs is able to tell you this file is under mercurial's control).

;;; History:
;;

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'vc))

;;; Customization options

(defcustom vc-hg-global-switches nil
  "*Global switches to pass to any Hg command."
  :type '(choice (const :tag "None" nil)
         (string :tag "Argument String")
         (repeat :tag "Argument List"
             :value ("")
             string))
  :version "22.2"
  :group 'vc)


;;; Properties of the backend

(defun vc-hg-revision-granularity ()
     'repository)

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
  (when (vc-hg-root file)           ; short cut
    (let ((state (vc-hg-state file)))  ; expensive
      (vc-file-setprop file 'vc-state state)
      (and state (not (memq state '(ignored unregistered)))))))

(defun vc-hg-state (file)
  "Hg-specific version of `vc-state'."
  (let*
      ((status nil)
       (out
	(with-output-to-string
	  (with-current-buffer
	      standard-output
	    (setq status
		  (condition-case nil
		      ;; Ignore all errors.
		      (call-process
		       "hg" nil t nil "--cwd" (file-name-directory file)
		       "status" "-A" (file-name-nondirectory file))
		    ;; Some problem happened.  E.g. We can't find an `hg'
		    ;; executable.
		    (error nil)))))))
    (when (eq 0 status)
	(when (null (string-match ".*: No such file or directory$" out))
	  (let ((state (aref out 0)))
	    (cond
	     ((eq state ?=) 'up-to-date)
	     ((eq state ?A) 'edited)
	     ((eq state ?M) 'edited)
	     ((eq state ?I) 'ignored)
	     ((eq state ?R) 'unregistered)
	     ((eq state ??) 'unregistered)
	     ((eq state ?C) 'up-to-date) ;; Older mercurials use this
	     (t 'up-to-date)))))))

(defun vc-hg-dir-state (dir)
  (with-temp-buffer
    (buffer-disable-undo)		;; Because these buffers can get huge
    (vc-hg-command (current-buffer) nil dir "status" "-A")
    (goto-char (point-min))
    (let ((status-char nil)
	  (file nil))
      (while (not (eobp))
	(setq status-char (char-after))
	(setq file
	      (expand-file-name
	       (buffer-substring-no-properties (+ (point) 2)
					       (line-end-position))))
	(cond
	 ;; State flag for a clean file is now C, might change to =.
	 ;; The rest of the possible states in "hg status" output:
	 ;; 	 ! = deleted, but still tracked
	 ;; should not show up in vc-dired, so don't deal with them
	 ;; here.

	 ;; Mercurial up to 0.9.5 used C, = is used now.
 	 ((or (eq status-char ?=) (eq status-char ?C))
	  (vc-file-setprop file 'vc-backend 'Hg)
 	  (vc-file-setprop file 'vc-state 'up-to-date))
	 ((eq status-char ?A)
	  (vc-file-setprop file 'vc-backend 'Hg)
	  (vc-file-setprop file 'vc-working-revision "0")
	  (vc-file-setprop file 'vc-state 'added))
	 ((eq status-char ?R)
	  (vc-file-setprop file 'vc-backend 'Hg)
	  (vc-file-setprop file 'vc-state 'removed))
	 ((eq status-char ?M)
	  (vc-file-setprop file 'vc-backend 'Hg)
	  (vc-file-setprop file 'vc-state 'edited))
	 ((eq status-char ?I)
	  (vc-file-setprop file 'vc-backend 'Hg)
	  (vc-file-setprop file 'vc-state 'ignored))
	 ((eq status-char ??)
	  (vc-file-setprop file 'vc-backend 'none)
	  (vc-file-setprop file 'vc-state 'unregistered))
	 ((eq status-char ?!)
	  nil)
	 (t	;; Presently C, might change to = in 0.9.6
	  (vc-file-setprop file 'vc-backend 'Hg)
	  (vc-file-setprop file 'vc-state 'up-to-date)))
	(forward-line)))))

(defun vc-hg-working-revision (file)
  "Hg-specific version of `vc-working-revision'."
  (let*
      ((status nil)
       (out
	(with-output-to-string
	  (with-current-buffer
	      standard-output
	    (setq status
		  (condition-case nil
		      ;; Ignore all errors.
		      (call-process
		       "hg" nil t nil "--cwd" (file-name-directory file)
		       "log" "-l1" (file-name-nondirectory file))
		    ;; Some problem happened.  E.g. We can't find an `hg'
		    ;; executable.
		    (error nil)))))))
    (when (eq 0 status)
      (if (string-match "changeset: *\\([0-9]*\\)" out)
	  (match-string 1 out)
	"0"))))

;;; History functions

(defun vc-hg-print-log (files &optional buffer)
  "Get change log associated with FILES."
  ;; `log-view-mode' needs to have the file names in order to function
  ;; correctly. "hg log" does not print it, so we insert it here by
  ;; hand.

  ;; `vc-do-command' creates the buffer, but we need it before running
  ;; the command.
  (vc-setup-buffer buffer)
  ;; If the buffer exists from a previous invocation it might be
  ;; read-only.
  (let ((inhibit-read-only t))
    ;; We need to loop and call "hg log" on each file separately.
    ;; "hg log" with multiple file arguments mashes all the logs
    ;; together.  Ironically enough, this puts us back near CVS
    ;; which can't generate proper fileset logs either.
    (dolist (file files)
      (with-current-buffer
	  buffer
	(insert "Working file: " file "\n"))	;; Like RCS/CVS.
      (vc-hg-command buffer 0 file "log"))))

(defvar log-view-message-re)
(defvar log-view-file-re)
(defvar log-view-font-lock-keywords)

(define-derived-mode vc-hg-log-view-mode log-view-mode "Hg-Log-View"
  (require 'add-log) ;; we need the add-log faces
  (set (make-local-variable 'log-view-file-re) "^Working file:[ \t]+\\(.+\\)")
  (set (make-local-variable 'log-view-message-re)
       "^changeset:[ \t]*\\([0-9]+\\):\\(.+\\)")
  (set (make-local-variable 'log-view-font-lock-keywords)
       (append
	log-view-font-lock-keywords
	'(
	  ;; Handle the case:
	  ;; user: FirstName LastName <foo@bar>
	  ("^user:[ \t]+\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]"
	   (1 'change-log-name)
	   (2 'change-log-email))
	  ;; Handle the cases:
	  ;; user: foo@bar
	  ;; and
	  ;; user: foo
	  ("^user:[ \t]+\\([A-Za-z0-9_.+-]+\\(?:@[A-Za-z0-9_.-]+\\)?\\)"
	   (1 'change-log-email))
	  ("^date: \\(.+\\)" (1 'change-log-date))
	  ("^summary:[ \t]+\\(.+\\)" (1 'log-view-message))))))

(defun vc-hg-diff (files &optional oldvers newvers buffer)
  "Get a difference report using hg between two revisions of FILES."
  (let ((working (vc-working-revision (car files))))
    (if (and (equal oldvers working) (not newvers))
	(setq oldvers nil))
    (if (and (not oldvers) newvers)
	(setq oldvers working))
    (apply #'vc-hg-command (or buffer "*vc-diff*") nil
	   (mapcar (lambda (file) (file-name-nondirectory file)) files)
	   "--cwd" (file-name-directory (car files))
	   "diff"
	   (append
	    (if oldvers
		(if newvers
		    (list "-r" oldvers "-r" newvers)
		  (list "-r" oldvers)))))))

(defun vc-hg-revision-table (files)
  (let ((default-directory (file-name-directory (car files))))
    (with-temp-buffer
      (vc-hg-command t nil files "log" "--template" "{rev} ")
      (split-string
       (buffer-substring-no-properties (point-min) (point-max))))))

;; Modelled after the similar function in vc-cvs.el
(defun vc-hg-revision-completion-table (files)
  (lexical-let ((files files)
                table)
    (setq table (lazy-completion-table
                 table (lambda () (vc-hg-revision-table files))))
    table))

(defun vc-hg-annotate-command (file buffer &optional revision)
  "Execute \"hg annotate\" on FILE, inserting the contents in BUFFER.
Optional arg REVISION is a revision to annotate from."
  (vc-hg-command buffer 0 file "annotate" "-d" "-n" (if revision (concat "-r" revision)))
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

(defun vc-hg-previous-revision (file rev)
  (let ((newrev (1- (string-to-number rev))))
    (when (>= newrev 0)
      (number-to-string newrev))))

(defun vc-hg-next-revision (file rev)
  (let ((newrev (1+ (string-to-number rev)))
	(tip-revision
	 (with-temp-buffer
	   (vc-hg-command t 0 nil "tip")
	   (goto-char (point-min))
	   (re-search-forward "^changeset:[ \t]*\\([0-9]+\\):")
	   (string-to-number (match-string-no-properties 1)))))
    ;; We don't want to exceed the maximum possible revision number, ie
    ;; the tip revision.
    (when (<= newrev tip-revision)
      (number-to-string newrev))))

;; Modelled after the similar function in vc-bzr.el
(defun vc-hg-delete-file (file)
  "Delete FILE and delete it in the hg repository."
  (condition-case ()
      (delete-file file)
    (file-error nil))
  (vc-hg-command nil 0 file "remove" "--after" "--force"))

;; Modelled after the similar function in vc-bzr.el
(defun vc-hg-rename-file (old new)
  "Rename file from OLD to NEW using `hg mv'."
  (vc-hg-command nil 0 new old "mv"))

(defun vc-hg-register (files &optional rev comment)
  "Register FILES under hg.
REV is ignored.
COMMENT is ignored."
  (vc-hg-command nil 0 files "add"))

(defun vc-hg-create-repo ()
  "Create a new Mercurial repository."
  (vc-hg-command nil 0 nil "init"))

(defalias 'vc-hg-responsible-p 'vc-hg-root)

;; Modelled after the similar function in vc-bzr.el
(defun vc-hg-could-register (file)
  "Return non-nil if FILE could be registered under hg."
  (and (vc-hg-responsible-p file)      ; shortcut
       (condition-case ()
           (with-temp-buffer
             (vc-hg-command t nil file "add" "--dry-run"))
             ;; The command succeeds with no output if file is
             ;; registered.
         (error))))

;; XXX This would remove the file. Is that correct?
;; (defun vc-hg-unregister (file)
;;   "Unregister FILE from hg."
;;   (vc-hg-command nil nil file "remove"))

(defun vc-hg-checkin (files rev comment)
  "Hg-specific version of `vc-backend-checkin'.
REV is ignored."
  (vc-hg-command nil 0 files  "commit" "-m" comment))

(defun vc-hg-find-revision (file rev buffer)
  (let ((coding-system-for-read 'binary)
        (coding-system-for-write 'binary))
    (if rev
	(vc-hg-command buffer 0 file "cat" "-r" rev)
      (vc-hg-command buffer 0 file "cat"))))

;; Modelled after the similar function in vc-bzr.el
(defun vc-hg-checkout (file &optional editable rev)
  "Retrieve a revision of FILE.
EDITABLE is ignored.
REV is the revision to check out into WORKFILE."
  (let ((coding-system-for-read 'binary)
        (coding-system-for-write 'binary))
  (with-current-buffer (or (get-file-buffer file) (current-buffer))
    (if rev
        (vc-hg-command t 0 file "cat" "-r" rev)
      (vc-hg-command t 0 file "cat")))))

(defun vc-hg-checkout-model (file)
  'implicit)

;; Modelled after the similar function in vc-bzr.el
(defun vc-hg-workfile-unchanged-p (file)
  (eq 'up-to-date (vc-hg-state file)))

(defun vc-hg-dired-state-info (file)
  "Hg-specific version of `vc-dired-state-info'."
  (let ((hg-state (vc-state file)))
    (if (eq hg-state 'edited)
	(if (equal (vc-working-revision file) "0")
	    "(added)" "(modified)")
      ;; fall back to the default VC representation
      (vc-default-dired-state-info 'Hg file))))

;; Modelled after the similar function in vc-bzr.el
(defun vc-hg-revert (file &optional contents-done)
  (unless contents-done
    (with-temp-buffer (vc-hg-command t 0 file "revert"))))

;;; Hg specific functionality.

;;; XXX This functionality is experimental/work in progress. It might
;;; change without notice.
(defvar vc-hg-extra-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [incoming] '(menu-item "Show incoming" vc-hg-incoming))
    (define-key map [outgoing] '(menu-item "Show outgoing" vc-hg-outgoing))
    map))

(defun vc-hg-extra-menu () vc-hg-extra-menu-map)

(defun vc-hg-extra-status-menu ()
  '(["Show incoming" vc-hg-incoming]
    ["Show outgoing" vc-hg-outgoing]))

(define-derived-mode vc-hg-outgoing-mode vc-hg-log-view-mode "Hg-Outgoing")

(define-derived-mode vc-hg-incoming-mode vc-hg-log-view-mode "Hg-Incoming")

;; XXX Experimental function for the vc-dired replacement.
(defun vc-hg-after-dir-status (update-function buff)
  (let ((status-char nil)
	(file nil)
	(translation '((?= . up-to-date)
		       (?C . up-to-date)
		       (?A . added)
		       (?R . removed)
		       (?M . edited)
		       (?I . ignored)
		       (?! . deleted)
		       (?? . unregistered)))
	(translated nil)
	  (result nil))
      (goto-char (point-min))
      (while (not (eobp))
	(setq status-char (char-after))
	(setq file
	      (buffer-substring-no-properties (+ (point) 2)
					      (line-end-position)))
	(setq translated (assoc status-char translation))
	(when (and translated (not (eq (cdr translated) 'up-to-date)))
	  (push (cons file (cdr translated)) result))
	(forward-line))
      (funcall update-function result buff)))

;; XXX Experimental function for the vc-dired replacement.
(defun vc-hg-dir-status (dir update-function status-buffer)
  "Return a list of conses (file . state) for DIR."
  (with-current-buffer
      (get-buffer-create
       (expand-file-name " *VC-hg* tmp status" dir))
    (erase-buffer)
    (vc-hg-command (current-buffer) 'async dir "status")
    (vc-exec-after
     `(vc-hg-after-dir-status (quote ,update-function) ,status-buffer))
    (current-buffer)))

;; XXX this adds another top level menu, instead figure out how to
;; replace the Log-View menu.
(easy-menu-define log-view-mode-menu vc-hg-outgoing-mode-map
  "Hg-outgoing Display Menu"
  `("Hg-outgoing"
    ["Push selected"  vc-hg-push]))

(easy-menu-define log-view-mode-menu vc-hg-incoming-mode-map
  "Hg-incoming Display Menu"
  `("Hg-incoming"
    ["Pull selected"  vc-hg-pull]))

(defun vc-hg-outgoing ()
  (interactive)
  (let ((bname "*Hg outgoing*"))
    (vc-hg-command bname 0 nil "outgoing" "-n")
    (pop-to-buffer bname)
    (vc-hg-outgoing-mode)))

(defun vc-hg-incoming ()
  (interactive)
  (let ((bname "*Hg incoming*"))
    (vc-hg-command bname 0 nil "incoming" "-n")
    (pop-to-buffer bname)
    (vc-hg-incoming-mode)))

(declare-function log-view-get-marked "log-view" ())

;; XXX maybe also add key bindings for these functions.
(defun vc-hg-push ()
  (interactive)
  (let ((marked-list (log-view-get-marked)))
    (if marked-list
	(vc-hg-command
	 nil 0 nil
	 (cons "push"
	       (apply 'nconc
		      (mapcar (lambda (arg) (list "-r" arg)) marked-list))))
	 (error "No log entries selected for push"))))

(defun vc-hg-pull ()
  (interactive)
  (let ((marked-list (log-view-get-marked)))
    (if marked-list
	(vc-hg-command
	 nil 0 nil
	 (cons "pull"
	       (apply 'nconc
		      (mapcar (lambda (arg) (list "-r" arg)) marked-list))))
      (error "No log entries selected for pull"))))

;;; Internal functions

(defun vc-hg-command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-hg.el.
The difference to vc-do-command is that this function always invokes `hg',
and that it passes `vc-hg-global-switches' to it before FLAGS."
  (apply 'vc-do-command buffer okstatus "hg" file-or-list
         (if (stringp vc-hg-global-switches)
             (cons vc-hg-global-switches flags)
           (append vc-hg-global-switches
                   flags))))

(defun vc-hg-root (file)
  (vc-find-root file ".hg"))

(provide 'vc-hg)

;; arch-tag: bd094dc5-715a-434f-a331-37b9fb7cd954
;;; vc-hg.el ends here
