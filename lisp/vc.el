;;; vc.el --- drive a version-control system from within Emacs

;; Copyright (C) 1992, 93, 94, 95, 96 Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Modified by:
;;   ttn@netcom.com
;;   Per Cederqvist <ceder@lysator.liu.edu>
;;   Andre Spiegel <spiegel@berlin.informatik.uni-stuttgart.de>

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This mode is fully documented in the Emacs user's manual.
;;
;; This was designed and implemented by Eric Raymond <esr@snark.thyrsus.com>.
;; Paul Eggert <eggert@twinsun.com>, Sebastian Kremer <sk@thp.uni-koeln.de>,
;; and Richard Stallman contributed valuable criticism, support, and testing.
;; CVS support was added by Per Cederqvist <ceder@lysator.liu.se>
;; in Jan-Feb 1994.
;;
;; Supported version-control systems presently include SCCS, RCS, and CVS.
;;
;; Some features will not work with old RCS versions.  Where
;; appropriate, VC finds out which version you have, and allows or
;; disallows those features (stealing locks, for example, works only 
;; from 5.6.2 onwards).
;; Even initial checkins will fail if your RCS version is so old that ci
;; doesn't understand -t-; this has been known to happen to people running
;; NExTSTEP 3.0. 
;;
;; You can support the RCS -x option by adding pairs to the 
;; vc-master-templates list.
;;
;; Proper function of the SCCS diff commands requires the shellscript vcdiff
;; to be installed somewhere on Emacs's path for executables.
;;
;; If your site uses the ChangeLog convention supported by Emacs, the
;; function vc-comment-to-change-log should prove a useful checkin hook.
;;
;; This code depends on call-process passing back the subprocess exit
;; status.  Thus, you need Emacs 18.58 or later to run it.  For the
;; vc-directory command to work properly as documented, you need 19.
;; You also need Emacs 19's ring.el.
;;
;; The vc code maintains some internal state in order to reduce expensive
;; version-control operations to a minimum.  Some names are only computed
;; once.  If you perform version control operations with RCS/SCCS/CVS while
;; vc's back is turned, or move/rename master files while vc is running,
;; vc may get seriously confused.  Don't do these things!
;;
;; Developer's notes on some concurrency issues are included at the end of
;; the file.

;;; Code:

(require 'vc-hooks)
(require 'ring)
(eval-when-compile (require 'dired))	; for dired-map-over-marks macro

(if (not (assoc 'vc-parent-buffer minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(vc-parent-buffer vc-parent-buffer-name)
		minor-mode-alist)))

;; To implement support for a new version-control system, add another
;; branch to the vc-backend-dispatch macro and fill it in in each
;; call.  The variable vc-master-templates in vc-hooks.el will also
;; have to change.

(defmacro vc-backend-dispatch (f s r c)
  "Execute FORM1, FORM2 or FORM3 for SCCS, RCS or CVS respectively.
If FORM3 is `RCS', use FORM2 for CVS as well as RCS.
\(CVS shares some code with RCS)."
  (list 'let (list (list 'type (list 'vc-backend f)))
	(list 'cond
	      (list (list 'eq 'type (quote 'SCCS)) s)	;; SCCS
	      (list (list 'eq 'type (quote 'RCS)) r)	;; RCS
	      (list (list 'eq 'type (quote 'CVS)) 	;; CVS
		    (if (eq c 'RCS) r c))
	      )))

;; General customization

(defvar vc-suppress-confirm nil
  "*If non-nil, treat user as expert; suppress yes-no prompts on some things.")
(defvar vc-initial-comment nil
  "*If non-nil, prompt for initial comment when a file is registered.")
(defvar vc-command-messages nil
  "*If non-nil, display run messages from back-end commands.")
(defvar vc-checkin-switches nil
  "*A string or list of strings specifying extra switches passed 
to the checkin program by \\[vc-checkin].")
(defvar vc-checkout-switches nil
  "*A string or list of strings specifying extra switches passed 
to the checkout program by \\[vc-checkout].")
(defvar vc-directory-exclusion-list '("SCCS" "RCS" "CVS")
  "*A list of directory names ignored by functions that recursively 
walk file trees.")

(defconst vc-maximum-comment-ring-size 32
  "Maximum number of saved comments in the comment ring.")

;;; This is duplicated in diff.el.
(defvar diff-switches "-c"
  "*A string or list of strings specifying switches to be be passed to diff.")

;;;###autoload
(defvar vc-checkin-hook nil
  "*List of functions called after a checkin is done.  See `run-hooks'.")

(defvar vc-make-buffer-writable-hook nil
  "*List of functions called when a buffer is made writable.  See `run-hooks.'
This hook is only used when the version control system is CVS.  It
might be useful for sites who uses locking with CVS, or who uses link
farms to gold trees.")

;; Header-insertion hair

(defvar vc-header-alist
  '((SCCS "\%W\%") (RCS "\$Id\$") (CVS "\$Id\$"))
  "*Header keywords to be inserted by `vc-insert-headers'.
Must be a list of two-element lists, the first element of each must
be `RCS', `CVS', or `SCCS'.  The second element is the string to
be inserted for this particular backend.")
(defvar vc-static-header-alist
  '(("\\.c$" .
     "\n#ifndef lint\nstatic char vcid[] = \"\%s\";\n#endif /* lint */\n"))
  "*Associate static header string templates with file types.  A \%s in the
template is replaced with the first string associated with the file's
version-control type in `vc-header-alist'.")

(defvar vc-comment-alist
  '((nroff-mode ".\\\"" ""))
  "*Special comment delimiters to be used in generating vc headers only.
Add an entry in this list if you need to override the normal comment-start
and comment-end variables.  This will only be necessary if the mode language
is sensitive to blank lines.")

;; Default is to be extra careful for super-user.
(defvar vc-checkout-carefully (= (user-uid) 0)
  "*Non-nil means be extra-careful in checkout.
Verify that the file really is not locked
and that its contents match what the master file says.")

(defvar vc-rcs-release nil
  "*The release number of your RCS installation, as a string.
If nil, VC itself computes this value when it is first needed.")

(defvar vc-sccs-release nil
  "*The release number of your SCCS installation, as a string.
If nil, VC itself computes this value when it is first needed.")

(defvar vc-cvs-release nil
  "*The release number of your CVS installation, as a string.
If nil, VC itself computes this value when it is first needed.")

;; Variables the user doesn't need to know about.
(defvar vc-log-entry-mode nil)
(defvar vc-log-operation nil)
(defvar vc-log-after-operation-hook nil)
(defvar vc-checkout-writable-buffer-hook 'vc-checkout-writable-buffer)
;; In a log entry buffer, this is a local variable
;; that points to the buffer for which it was made
;; (either a file, or a VC dired buffer).
(defvar vc-parent-buffer nil)
(defvar vc-parent-buffer-name nil)

(defvar vc-log-file)
(defvar vc-log-version)

(defconst vc-name-assoc-file "VC-names")

(defvar vc-dired-mode nil)
(make-variable-buffer-local 'vc-dired-mode)

(defvar vc-comment-ring (make-ring vc-maximum-comment-ring-size))
(defvar vc-comment-ring-index nil)
(defvar vc-last-comment-match nil)

;; Back-portability to Emacs 18

(defun file-executable-p-18 (f)
  (let ((modes (file-modes f)))
    (and modes (not (zerop (logand 292))))))

(defun file-regular-p-18 (f)
  (let ((attributes (file-attributes f)))
    (and attributes (not (car attributes)))))

; Conditionally rebind some things for Emacs 18 compatibility
(if (not (boundp 'minor-mode-map-alist))
    (progn
      (setq compilation-old-error-list nil)
      (fset 'file-executable-p 'file-executable-p-18)
      (fset 'shrink-window-if-larger-than-buffer 'beginning-of-buffer)
      ))

(if (not (fboundp 'file-regular-p))
    (fset 'file-regular-p 'file-regular-p-18))

;;; Find and compare backend releases

(defun vc-backend-release (backend)
  ;; Returns which backend release is installed on this system.
  (cond
   ((eq backend 'RCS)
    (or vc-rcs-release
	(and (zerop (vc-do-command nil 2 "rcs" nil nil "-V"))
	     (save-excursion
	       (set-buffer (get-buffer "*vc*"))
	       (setq vc-rcs-release
		     (car (vc-parse-buffer
			   '(("^RCS version \\([0-9.]+ *.*\\)" 1)))))))
	(setq vc-rcs-release 'unknown)))
   ((eq backend 'CVS)
    (or vc-cvs-release
	(and (zerop (vc-do-command nil 1 "cvs" nil nil "-v"))
	     (save-excursion
	       (set-buffer (get-buffer "*vc*"))
	       (setq vc-cvs-release
		     (car (vc-parse-buffer
			   '(("^Concurrent Versions System (CVS) \\([0-9.]+\\)"
			      1)))))))
	(setq vc-cvs-release 'unknown)))
     ((eq backend 'SCCS)
      vc-sccs-release)))

(defun vc-release-greater-or-equal (r1 r2)
  ;; Compare release numbers, represented as strings.
  ;; Release components are assumed cardinal numbers, not decimal
  ;; fractions (5.10 is a higher release than 5.9).  Omitted fields
  ;; are considered lower (5.6.7 is earlier than 5.6.7.1).
  ;; Comparison runs till the end of the string is found, or a
  ;; non-numeric component shows up (5.6.7 is earlier than "5.6.7 beta",
  ;; which is probably not what you want in some cases).
  ;;   This code is suitable for existing RCS release numbers.  
  ;; CVS releases are handled reasonably, too (1.3 < 1.4* < 1.5).
  (let (v1 v2 i1 i2)
    (catch 'done
      (or (and (string-match "^\\.?\\([0-9]+\\)" r1)
	       (setq i1 (match-end 0))
	       (setq v1 (string-to-number (match-string 1 r1)))
	       (or (and (string-match "^\\.?\\([0-9]+\\)" r2)
			(setq i2 (match-end 0))
			(setq v2 (string-to-number (match-string 1 r2)))
			(if (> v1 v2) (throw 'done t)
			  (if (< v1 v2) (throw 'done nil)
			    (throw 'done
				   (vc-release-greater-or-equal
				    (substring r1 i1)
				    (substring r2 i2)))))))
		   (throw 'done t)))
	  (or (and (string-match "^\\.?\\([0-9]+\\)" r2)
		   (throw 'done nil))
	      (throw 'done t)))))

(defun vc-backend-release-p (backend release)
  ;; Return t if we have RELEASE of BACKEND or better
  (let (i r (ri 0) (ii 0) is rs (installation (vc-backend-release backend)))
    (if (not (eq installation 'unknown))
	(cond
	 ((or (eq backend 'RCS) (eq backend 'CVS))
	  (vc-release-greater-or-equal installation release))))))

;;; functions that operate on RCS revision numbers

(defun vc-trunk-p (rev)
  ;; return t if REV is a revision on the trunk
  (not (eq nil (string-match "\\`[0-9]+\\.[0-9]+\\'" rev))))

(defun vc-branch-part (rev)
  ;; return the branch part of a revision number REV
  (substring rev 0 (string-match "\\.[0-9]+\\'" rev)))

;; File property caching

(defun vc-clear-context ()
  "Clear all cached file properties and the comment ring."
  (interactive)
  (fillarray vc-file-prop-obarray nil)
  ;; Note: there is potential for minor lossage here if there is an open
  ;; log buffer with a nonzero local value of vc-comment-ring-index.
  (setq vc-comment-ring (make-ring vc-maximum-comment-ring-size)))

(defun vc-file-clear-masterprops (file)
  ;; clear all properties of FILE that were retrieved
  ;; from the master file
  (vc-file-setprop file 'vc-latest-version nil)
  (vc-file-setprop file 'vc-your-latest-version nil)
  (vc-backend-dispatch file
     (progn   ;; SCCS
       (vc-file-setprop file 'vc-master-locks nil))
     (progn   ;; RCS
       (vc-file-setprop file 'vc-default-branch nil)
       (vc-file-setprop file 'vc-head-version nil)
       (vc-file-setprop file 'vc-master-workfile-version nil)
       (vc-file-setprop file 'vc-master-locks nil))
     (progn
       (vc-file-setprop file 'vc-cvs-status nil))))

(defun vc-head-version (file)
  ;; Return the RCS head version of FILE 
  (cond ((vc-file-getprop file 'vc-head-version))
	(t (vc-fetch-master-properties file)
	   (vc-file-getprop file 'vc-head-version))))

;; Random helper functions

(defun vc-latest-on-branch-p (file)
  ;; return t iff the current workfile version of FILE is
  ;; the latest on its branch.
  (vc-backend-dispatch file
     ;; SCCS
     (string= (vc-workfile-version file) (vc-latest-version file)) 
     ;; RCS
     (let ((workfile-version (vc-workfile-version file)) tip-version)
       (if (vc-trunk-p workfile-version) 
	   (progn 
	     ;; Re-fetch the head version number.  This is to make
             ;; sure that no-one has checked in a new version behind
	     ;; our back.
	     (vc-fetch-master-properties file)
	     (string= (vc-file-getprop file 'vc-head-version)
		      workfile-version))
	 ;; If we are not on the trunk, we need to examine the
	 ;; whole current branch.  (vc-master-workfile-version 
         ;; is not what we need.)
	 (save-excursion
	   (set-buffer (get-buffer-create "*vc-info*"))
	   (vc-insert-file (vc-name file) "^desc")
	   (setq tip-version (car (vc-parse-buffer (list (list 
             (concat "^\\(" (regexp-quote (vc-branch-part workfile-version))
		     "\\.[0-9]+\\)\ndate[ \t]+\\([0-9.]+\\);") 1 2)))))
	   (if (get-buffer "*vc-info*") 
	       (kill-buffer (get-buffer "*vc-info*")))
	   (string= tip-version workfile-version))))
     ;; CVS
     t))

(defun vc-registration-error (file)
  (if file
      (error "File %s is not under version control" file)
    (error "Buffer %s is not associated with a file" (buffer-name))))

(defvar vc-binary-assoc nil)

(defun vc-find-binary (name)
  "Look for a command anywhere on the subprocess-command search path."
  (or (cdr (assoc name vc-binary-assoc))
      (catch 'found
	(mapcar
	 (function 
	  (lambda (s)
	    (if s
		(let ((full (concat s "/" name)))
		  (if (file-executable-p full)
		      (progn
			(setq vc-binary-assoc
			      (cons (cons name full) vc-binary-assoc))
			(throw 'found full)))))))
	 exec-path)
	nil)))

(defun vc-do-command (buffer okstatus command file last &rest flags)
  "Execute a version-control command, notifying user and checking for errors.
Output from COMMAND goes to BUFFER, or *vc* if BUFFER is nil.  
The command is successful if its exit status does not exceed OKSTATUS.
The last argument of the command is the master name of FILE if LAST is 
`MASTER', or the workfile of FILE if LAST is `WORKFILE'; this is appended 
to an optional list of FLAGS."
  (and file (setq file (expand-file-name file)))
  (if (not buffer) (setq buffer "*vc*"))
  (if vc-command-messages
      (message "Running %s on %s..." command file))
  (let ((obuf (current-buffer)) (camefrom (current-buffer))
	(squeezed nil)
	(vc-file (and file (vc-name file)))
	(olddir default-directory)
	status)
    (set-buffer (get-buffer-create buffer))
    (set (make-local-variable 'vc-parent-buffer) camefrom)
    (set (make-local-variable 'vc-parent-buffer-name)
	 (concat " from " (buffer-name camefrom)))
    (setq default-directory olddir)
    
    (erase-buffer)

    (mapcar
     (function (lambda (s) (and s (setq squeezed (append squeezed (list s))))))
     flags)
    (if (and vc-file (eq last 'MASTER))
	(setq squeezed (append squeezed (list vc-file))))
    (if (eq last 'WORKFILE)
	(progn
	  (let* ((pwd (expand-file-name default-directory))
		 (preflen (length pwd)))
	    (if (string= (substring file 0 preflen) pwd)
		(setq file (substring file preflen))))
	  (setq squeezed (append squeezed (list file)))))
    (let ((exec-path (append vc-path exec-path))
	  ;; Add vc-path to PATH for the execution of this command.
	  (process-environment
	   (cons (concat "PATH=" (getenv "PATH")
			 path-separator
			 (mapconcat 'identity vc-path path-separator))
		 process-environment))
	  (win32-quote-process-args t))
      (setq status (apply 'call-process command nil t nil squeezed)))
    (goto-char (point-max))
    (set-buffer-modified-p nil)
    (forward-line -1)
    (if (or (not (integerp status)) (< okstatus status))
	(progn
	  (pop-to-buffer buffer)
	  (goto-char (point-min))
	  (shrink-window-if-larger-than-buffer)
	  (error "Running %s...FAILED (%s)" command
		 (if (integerp status)
		     (format "status %d" status)
		   status))
	  )
      (if vc-command-messages
	  (message "Running %s...OK" command))
      )
    (set-buffer obuf)
    status)
  )

;;; Save a bit of the text around POSN in the current buffer, to help
;;; us find the corresponding position again later.  This works even
;;; if all markers are destroyed or corrupted.
;;; A lot of this was shamelessly lifted from Sebastian Kremer's rcs.el mode.
(defun vc-position-context (posn)
  (list posn
	(buffer-size)
	(buffer-substring posn
			  (min (point-max) (+ posn 100)))))

;;; Return the position of CONTEXT in the current buffer, or nil if we
;;; couldn't find it.
(defun vc-find-position-by-context (context)
  (let ((context-string (nth 2 context)))
    (if (equal "" context-string)
	(point-max)
      (save-excursion
	(let ((diff (- (nth 1 context) (buffer-size))))
	  (if (< diff 0) (setq diff (- diff)))
	  (goto-char (nth 0 context))
	  (if (or (search-forward context-string nil t)
		  ;; Can't use search-backward since the match may continue
		  ;; after point.
		  (progn (goto-char (- (point) diff (length context-string)))
			 ;; goto-char doesn't signal an error at
			 ;; beginning of buffer like backward-char would
			 (search-forward context-string nil t)))
	      ;; to beginning of OSTRING
	      (- (point) (length context-string))))))))

(defun vc-buffer-context ()
  ;; Return a list '(point-context mark-context reparse); from which
  ;; vc-restore-buffer-context can later restore the context.
  (let ((point-context (vc-position-context (point)))
	;; Use mark-marker to avoid confusion in transient-mark-mode.
	(mark-context  (if (eq (marker-buffer (mark-marker)) (current-buffer))
			   (vc-position-context (mark-marker))))
	;; Make the right thing happen in transient-mark-mode.
	(mark-active nil)
	;; We may want to reparse the compilation buffer after revert
	(reparse (and (boundp 'compilation-error-list) ;compile loaded
		      (let ((curbuf (current-buffer)))
			;; Construct a list; each elt is nil or a buffer
			;; iff that buffer is a compilation output buffer
			;; that contains markers into the current buffer.
			(save-excursion
			  (mapcar (function
				   (lambda (buffer)
				    (set-buffer buffer)
				    (let ((errors (or
						   compilation-old-error-list
						   compilation-error-list))
					  (buffer-error-marked-p nil))
				      (while (and (consp errors)
						  (not buffer-error-marked-p))
					(and (markerp (cdr (car errors)))
					     (eq buffer
						 (marker-buffer
						  (cdr (car errors))))
					     (setq buffer-error-marked-p t))
					(setq errors (cdr errors)))
				      (if buffer-error-marked-p buffer))))
				  (buffer-list)))))))
    (list point-context mark-context reparse)))

(defun vc-restore-buffer-context (context)
  ;; Restore point/mark, and reparse any affected compilation buffers.
  ;; CONTEXT is that which vc-buffer-context returns.
  (let ((point-context (nth 0 context))
	(mark-context (nth 1 context))
	(reparse (nth 2 context)))
    ;; Reparse affected compilation buffers.
    (while reparse
      (if (car reparse)
	  (save-excursion
	    (set-buffer (car reparse))
	    (let ((compilation-last-buffer (current-buffer)) ;select buffer
		  ;; Record the position in the compilation buffer of
		  ;; the last error next-error went to.
		  (error-pos (marker-position
			      (car (car-safe compilation-error-list)))))
	      ;; Reparse the error messages as far as they were parsed before.
	      (compile-reinitialize-errors '(4) compilation-parsing-end)
	      ;; Move the pointer up to find the error we were at before
	      ;; reparsing.  Now next-error should properly go to the next one.
	      (while (and compilation-error-list
			  (/= error-pos (car (car compilation-error-list))))
		(setq compilation-error-list (cdr compilation-error-list))))))
      (setq reparse (cdr reparse)))

    ;; Restore point and mark
    (let ((new-point (vc-find-position-by-context point-context)))
      (if new-point (goto-char new-point)))
    (if mark-context
	(let ((new-mark (vc-find-position-by-context mark-context)))
	  (if new-mark (set-mark new-mark))))))

(defun vc-revert-buffer1 (&optional arg no-confirm)
  ;; Revert buffer, try to keep point and mark where user expects them in spite
  ;; of changes because of expanded version-control key words.
  ;; This is quite important since otherwise typeahead won't work as expected.
  (interactive "P")
  (widen)
  (let ((context (vc-buffer-context)))
    ;; t means don't call normal-mode; that's to preserve various minor modes.
    (revert-buffer arg no-confirm t)
    (vc-restore-buffer-context context)))


(defun vc-buffer-sync (&optional not-urgent)
  ;; Make sure the current buffer and its working file are in sync
  ;; NOT-URGENT means it is ok to continue if the user says not to save.
  (if (buffer-modified-p)
      (if (or vc-suppress-confirm
	      (y-or-n-p (format "Buffer %s modified; save it? " (buffer-name))))
	  (save-buffer)
	(if not-urgent
	    nil
	  (error "Aborted")))))


(defun vc-workfile-unchanged-p (file &optional want-differences-if-changed)
  ;; Has the given workfile changed since last checkout?
  (let ((checkout-time (vc-file-getprop file 'vc-checkout-time))
	(lastmod (nth 5 (file-attributes file))))
    (or (equal checkout-time lastmod)
	(and (or (not checkout-time) want-differences-if-changed)
	     (let ((unchanged (zerop (vc-backend-diff file nil nil
					  (not want-differences-if-changed)))))
	       ;; 0 stands for an unknown time; it can't match any mod time.
	       (vc-file-setprop file 'vc-checkout-time (if unchanged lastmod 0))
	       unchanged)))))

(defun vc-next-action-on-file (file verbose &optional comment)
  ;;; If comment is specified, it will be used as an admin or checkin comment.
  (let ((vc-file (vc-name file))
	(vc-type (vc-backend file))
	owner version buffer)
    (cond

     ;; if there is no master file corresponding, create one
     ((not vc-file)
      (vc-register verbose comment)
      (if vc-initial-comment
	  (setq vc-log-after-operation-hook
		'vc-checkout-writable-buffer-hook)
	(vc-checkout-writable-buffer file)))

     ;; CVS: changes to the master file need to be 
     ;; merged back into the working file
     ((and (eq vc-type 'CVS)
	   (or (eq (vc-cvs-status file) 'needs-checkout)
	       (eq (vc-cvs-status file) 'needs-merge)))
      (if (or vc-dired-mode
	      (yes-or-no-p 
	       (format "%s is not up-to-date.  Merge in changes now? "
		       (buffer-name))))
	  (progn
	    (if vc-dired-mode
		(and (setq buffer (get-file-buffer file))
		     (buffer-modified-p buffer)
		     (switch-to-buffer-other-window buffer)
		     (vc-buffer-sync t))
	      (setq buffer (current-buffer))
	      (vc-buffer-sync t))
	    (if (and buffer (buffer-modified-p buffer)
		     (not (yes-or-no-p 
			   (format 
			    "Buffer %s modified; merge file on disc anyhow? " 
			    (buffer-name buffer)))))
		(error "Merge aborted"))
	    (if (not (zerop (vc-backend-merge-news file)))
		;; Overlaps detected - what now?  Should use some
		;; fancy RCS conflict resolving package, or maybe
		;; emerge, but for now, simply warn the user with a
		;; message.
		(message "Conflicts detected!"))
	    (and buffer
		 (vc-resynch-buffer file t (not (buffer-modified-p buffer)))))
	(error "%s needs update" (buffer-name))))

     ;; if there is no lock on the file, assert one and get it
     ((not (setq owner (vc-locking-user file)))
      (if (and vc-checkout-carefully
	       (not (vc-workfile-unchanged-p file t)))
	  (if (save-window-excursion
		(pop-to-buffer "*vc-diff*")
		(goto-char (point-min))
		(insert-string (format "Changes to %s since last lock:\n\n"
				       file))
		(not (beep))
		(yes-or-no-p
		      (concat "File has unlocked changes, "
		       "claim lock retaining changes? ")))
	      (progn (vc-backend-steal file)
		     (vc-mode-line file))
	    (if (not (yes-or-no-p "Revert to checked-in version, instead? "))
		(error "Checkout aborted")
	      (vc-revert-buffer1 t t)
	      (vc-checkout-writable-buffer file))
	    )
	(if verbose 
	    (if (not (eq vc-type 'SCCS))
		(vc-checkout file nil 
		   (read-string "Branch or version to move to: "))
	      (error "Sorry, this is not implemented for SCCS"))
	  (if (vc-latest-on-branch-p file)
	      (vc-checkout-writable-buffer file)
	    (if (yes-or-no-p 
		 "This is not the latest version.  Really lock it?  ")
		(vc-checkout-writable-buffer file)
	      (if (yes-or-no-p "Lock the latest version instead? ")
		  (vc-checkout-writable-buffer file
		     (if (vc-trunk-p (vc-workfile-version file)) 
                         ""  ;; this means check out latest on trunk
                       (vc-branch-part (vc-workfile-version file)))))))
	  )))

     ;; a checked-out version exists, but the user may not own the lock
     ((and (not (eq vc-type 'CVS))
	   (not (string-equal owner (user-login-name))))
      (if comment
	  (error "Sorry, you can't steal the lock on %s this way" file))
      (and (eq vc-type 'RCS)
	   (not (vc-backend-release-p 'RCS "5.6.2"))
	   (error "File is locked by %s" owner))
      (vc-steal-lock
       file
       (if verbose (read-string "Version to steal: ")
	 (vc-workfile-version file))
       owner))

     ;; OK, user owns the lock on the file
     (t
	  (if vc-dired-mode 
	      (find-file-other-window file) 
	    (find-file file))

	  ;; give luser a chance to save before checking in.
	  (vc-buffer-sync)

	  ;; Revert if file is unchanged and buffer is too.
	  ;; If buffer is modified, that means the user just said no
	  ;; to saving it; in that case, don't revert,
	  ;; because the user might intend to save
	  ;; after finishing the log entry.
	  (if (and (vc-workfile-unchanged-p file) 
		   (not (buffer-modified-p)))
	       ;; DO NOT revert the file without asking the user!
	      (cond 
	       ((yes-or-no-p "Revert to master version? ")
		(vc-backend-revert file)
		(vc-resynch-window file t t)))

	    ;; user may want to set nonstandard parameters
	    (if verbose
		(setq version (read-string "New version level: ")))

	    ;; OK, let's do the checkin
	    (vc-checkin file version comment)
	    )))))

(defun vc-next-action-dired (file rev comment)
  ;; Do a vc-next-action-on-file on all the marked files, possibly 
  ;; passing on the log comment we've just entered.
  (let ((configuration (current-window-configuration))
	(dired-buffer (current-buffer))
	(dired-dir default-directory))
    (dired-map-over-marks
     (let ((file (dired-get-filename)) p
	   (default-directory default-directory))
       (message "Processing %s..." file)
       ;; Adjust the default directory so that checkouts
       ;; go to the right place.
       (setq default-directory (file-name-directory file))
       (vc-next-action-on-file file nil comment)
       (set-buffer dired-buffer)
       (setq default-directory dired-dir)
       (vc-dired-update-line file)
       (set-window-configuration configuration)
       (message "Processing %s...done" file))
    nil t)))

;; Here's the major entry point.

;;;###autoload
(defun vc-next-action (verbose)
  "Do the next logical checkin or checkout operation on the current file.
   If you call this from within a VC dired buffer with no files marked,
it will operate on the file in the current line.
   If you call this from within a VC dired buffer, and one or more
files are marked, it will accept a log message and then operate on
each one.  The log message will be used as a comment for any register
or checkin operations, but ignored when doing checkouts.  Attempted
lock steals will raise an error.
   A prefix argument lets you specify the version number to use.

For RCS and SCCS files:
   If the file is not already registered, this registers it for version
control and then retrieves a writable, locked copy for editing.
   If the file is registered and not locked by anyone, this checks out
a writable and locked file ready for editing.
   If the file is checked out and locked by the calling user, this
first checks to see if the file has changed since checkout.  If not,
it performs a revert.
   If the file has been changed, this pops up a buffer for entry
of a log message; when the message has been entered, it checks in the
resulting changes along with the log message as change commentary.  If
the variable `vc-keep-workfiles' is non-nil (which is its default), a
read-only copy of the changed file is left in place afterwards.
   If the file is registered and locked by someone else, you are given
the option to steal the lock.

For CVS files:
   If the file is not already registered, this registers it for version
control.  This does a \"cvs add\", but no \"cvs commit\".
   If the file is added but not committed, it is committed.
   If your working file is changed, but the repository file is
unchanged, this pops up a buffer for entry of a log message; when the
message has been entered, it checks in the resulting changes along
with the logmessage as change commentary.  A writable file is retained.
   If the repository file is changed, you are asked if you want to
merge in the changes into your working copy."

  (interactive "P")
  (catch 'nogo
    (if vc-dired-mode
	(let ((files (dired-get-marked-files)))
	  (if (string= "" 
		 (mapconcat
	             (function (lambda (f)
			 (if (eq (vc-backend f) 'CVS)
			     (if (or (eq (vc-cvs-status f) 'locally-modified)
				     (eq (vc-cvs-status f) 'locally-added))
				 "@" "")
			   (if (vc-locking-user f) "@" ""))))
		     files ""))
		(vc-next-action-dired nil nil "dummy")
	      (vc-start-entry nil nil nil
			      "Enter a change comment for the marked files."
			      'vc-next-action-dired))
	    (throw 'nogo nil)))
    (while vc-parent-buffer
      (pop-to-buffer vc-parent-buffer))
    (if buffer-file-name
	(vc-next-action-on-file buffer-file-name verbose)
      (vc-registration-error nil))))

;;; These functions help the vc-next-action entry point

(defun vc-checkout-writable-buffer (&optional file rev)
  "Retrieve a writable copy of the latest version of the current buffer's file."
  (vc-checkout (or file (buffer-file-name)) t rev)
  )

;;;###autoload
(defun vc-register (&optional override comment)
  "Register the current file into your version-control system."
  (interactive "P")
  (or buffer-file-name
      (error "No visited file"))
  (let ((master (vc-name buffer-file-name)))
    (and master (file-exists-p master)
	 (error "This file is already registered"))
    (and master
	 (not (y-or-n-p "Previous master file has vanished.  Make a new one? "))
	 (error "This file is already registered")))
  ;; Watch out for new buffers of size 0: the corresponding file
  ;; does not exist yet, even though buffer-modified-p is nil.
  (if (and (not (buffer-modified-p))
	   (zerop (buffer-size))
	   (not (file-exists-p buffer-file-name)))
      (set-buffer-modified-p t))
  (vc-buffer-sync)
  (cond ((not vc-make-backup-files)
	 ;; inhibit backup for this buffer
	 (make-local-variable 'backup-inhibited)
	 (setq backup-inhibited t)))
  (vc-admin
   buffer-file-name
   (and override
	(read-string
	 (format "Initial version level for %s: " buffer-file-name))))
  )

(defun vc-resynch-window (file &optional keep noquery)
  ;; If the given file is in the current buffer,
  ;; either revert on it so we see expanded keywords,
  ;; or unvisit it (depending on vc-keep-workfiles)
  ;; NOQUERY if non-nil inhibits confirmation for reverting.
  ;; NOQUERY should be t *only* if it is known the only difference
  ;; between the buffer and the file is due to RCS rather than user editing!
  (and (string= buffer-file-name file)
       (if keep
	   (progn
	     ;; temporarily remove vc-find-file-hook, so that
             ;; we don't lose the properties
	     (remove-hook 'find-file-hooks 'vc-find-file-hook)
	     (vc-revert-buffer1 t noquery)
	     (add-hook 'find-file-hooks 'vc-find-file-hook)
	     (vc-mode-line buffer-file-name))
	 (kill-buffer (current-buffer)))))

(defun vc-resynch-buffer (file &optional keep noquery)
  ;; if FILE is currently visited, resynch its buffer
  (let ((buffer (get-file-buffer file)))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (vc-resynch-window file keep noquery)))))

(defun vc-start-entry (file rev comment msg action &optional after-hook)
  ;; Accept a comment for an operation on FILE revision REV.  If COMMENT
  ;; is nil, pop up a VC-log buffer, emit MSG, and set the
  ;; action on close to ACTION; otherwise, do action immediately.
  ;; Remember the file's buffer in vc-parent-buffer (current one if no file).
  ;; AFTER-HOOK specifies the local value for vc-log-operation-hook.
  (let ((parent (if file (find-file-noselect file) (current-buffer))))
    (if comment
	(set-buffer (get-buffer-create "*VC-log*"))
      (pop-to-buffer (get-buffer-create "*VC-log*")))
    (set (make-local-variable 'vc-parent-buffer) parent)
    (set (make-local-variable 'vc-parent-buffer-name)
	 (concat " from " (buffer-name vc-parent-buffer)))
    (if file (vc-mode-line file))
    (vc-log-mode)
    (make-local-variable 'vc-log-after-operation-hook)
    (if after-hook
	(setq vc-log-after-operation-hook after-hook))
    (setq vc-log-operation action)
    (setq vc-log-file file)
    (setq vc-log-version rev)
    (if comment
	(progn
	  (erase-buffer)
	  (if (eq comment t)
	      (vc-finish-logentry t)
	    (insert comment)
	    (vc-finish-logentry nil)))
      (message "%s  Type C-c C-c when done." msg))))

(defun vc-admin (file rev &optional comment)
  "Check a file into your version-control system.
FILE is the unmodified name of the file.  REV should be the base version
level to check it in under.  COMMENT, if specified, is the checkin comment."
  (vc-start-entry file rev
		  (or comment (not vc-initial-comment))
		  "Enter initial comment." 'vc-backend-admin
		  nil))

(defun vc-checkout (file &optional writable rev)
  "Retrieve a copy of the latest version of the given file."
  ;; If ftp is on this system and the name matches the ange-ftp format
  ;; for a remote file, the user is trying something that won't work.
  (if (and (string-match "^/[^/:]+:" file) (vc-find-binary "ftp"))
      (error "Sorry, you can't check out files over FTP"))
  (vc-backend-checkout file writable rev)
  (vc-resynch-buffer file t t))

(defun vc-steal-lock (file rev &optional owner)
  "Steal the lock on the current workfile."
  (let (file-description)
    (if (not owner)
	(setq owner (vc-locking-user file)))
    (if rev
	(setq file-description (format "%s:%s" file rev))
      (setq file-description file))
    (if (not (y-or-n-p (format "Take the lock on %s from %s? "
			       file-description owner)))
	(error "Steal cancelled"))
    (pop-to-buffer (get-buffer-create "*VC-mail*"))
    (setq default-directory (expand-file-name "~/"))
    (auto-save-mode auto-save-default)
    (mail-mode)
    (erase-buffer)
    (mail-setup owner (format "Stolen lock on %s" file-description) nil nil nil
		(list (list 'vc-finish-steal file rev)))
    (goto-char (point-max))
    (insert
     (format "I stole the lock on %s, " file-description)
     (current-time-string)
     ".\n")
    (message "Please explain why you stole the lock.  Type C-c C-c when done.")))

;; This is called when the notification has been sent.
(defun vc-finish-steal (file version)
  (vc-backend-steal file version)
  (if (get-file-buffer file)
      (save-excursion
	(set-buffer (get-file-buffer file))
	(vc-resynch-window file t t))))

(defun vc-checkin (file &optional rev comment)
  "Check in the file specified by FILE.
The optional argument REV may be a string specifying the new version level
\(if nil increment the current level).  The file is either retained with write
permissions zeroed, or deleted (according to the value of `vc-keep-workfiles').
If the back-end is CVS, a writable workfile is always kept.
COMMENT is a comment string; if omitted, a buffer is
popped up to accept a comment."
  (vc-start-entry file rev comment
		  "Enter a change comment." 'vc-backend-checkin
		  'vc-checkin-hook))

;;; Here is a checkin hook that may prove useful to sites using the
;;; ChangeLog facility supported by Emacs.
(defun vc-comment-to-change-log (&optional whoami file-name)
  "Enter last VC comment into change log file for current buffer's file.
Optional arg (interactive prefix) non-nil means prompt for user name and site.
Second arg is file name of change log.  \
If nil, uses `change-log-default-name'."
  (interactive (if current-prefix-arg
		   (list current-prefix-arg
			 (prompt-for-change-log-name))))
  ;; Make sure the defvar for add-log-current-defun-function has been executed
  ;; before binding it.
  (require 'add-log)
  (let (;; Extract the comment first so we get any error before doing anything.
	(comment (ring-ref vc-comment-ring 0))
	;; Don't let add-change-log-entry insert a defun name.
	(add-log-current-defun-function 'ignore)
	end)
    ;; Call add-log to do half the work.
    (add-change-log-entry whoami file-name t t)
    ;; Insert the VC comment, leaving point before it.
    (setq end (save-excursion (insert comment) (point-marker)))
    (if (looking-at "\\s *\\s(")
	;; It starts with an open-paren, as in "(foo): Frobbed."
	;; So remove the ": " add-log inserted.
	(delete-char -2))
    ;; Canonicalize the white space between the file name and comment.
    (just-one-space)
    ;; Indent rest of the text the same way add-log indented the first line.
    (let ((indentation (current-indentation)))
      (save-excursion
	(while (< (point) end)
	  (forward-line 1)
	  (indent-to indentation))
	(setq end (point))))
    ;; Fill the inserted text, preserving open-parens at bol.
    (let ((paragraph-separate (concat paragraph-separate "\\|\\s *\\s("))
	  (paragraph-start (concat paragraph-start "\\|\\s *\\s(")))
      (beginning-of-line)
      (fill-region (point) end))
    ;; Canonicalize the white space at the end of the entry so it is
    ;; separated from the next entry by a single blank line.
    (skip-syntax-forward " " end)
    (delete-char (- (skip-syntax-backward " ")))
    (or (eobp) (looking-at "\n\n")
	(insert "\n"))))


(defun vc-finish-logentry (&optional nocomment)
  "Complete the operation implied by the current log entry."
  (interactive)
  ;; Check and record the comment, if any.
  (if (not nocomment)
      (progn
	(goto-char (point-max))
	(if (not (bolp))
	    (newline))
	;; Comment too long?
	(vc-backend-logentry-check vc-log-file)
	;; Record the comment in the comment ring
	(ring-insert vc-comment-ring (buffer-string))
	))
  ;; Sync parent buffer in case the user modified it while editing the comment.
  ;; But not if it is a vc-dired buffer.
  (save-excursion
    (set-buffer vc-parent-buffer)
    (or vc-dired-mode
	(vc-buffer-sync)))
  (if (not vc-log-operation) (error "No log operation is pending"))
  ;; save the parameters held in buffer-local variables
  (let ((log-operation vc-log-operation)
	(log-file vc-log-file)
	(log-version vc-log-version)
	(log-entry (buffer-string))
	(after-hook vc-log-after-operation-hook))
    ;; Return to "parent" buffer of this checkin and remove checkin window
    (pop-to-buffer vc-parent-buffer)
    (let ((logbuf (get-buffer "*VC-log*")))
      (delete-windows-on logbuf)
      (kill-buffer logbuf))
    ;; OK, do it to it
    (save-excursion
      (funcall log-operation 
	       log-file
	       log-version
	       log-entry))
    ;; Now make sure we see the expanded headers
    (if buffer-file-name
	(vc-resynch-window buffer-file-name vc-keep-workfiles t))
    (run-hooks after-hook)))

;; Code for access to the comment ring

(defun vc-previous-comment (arg)
  "Cycle backwards through comment history."
  (interactive "*p")
  (let ((len (ring-length vc-comment-ring)))
    (cond ((<= len 0)
	   (message "Empty comment ring")
	   (ding))
	  (t
	   (erase-buffer)
	   ;; Initialize the index on the first use of this command
	   ;; so that the first M-p gets index 0, and the first M-n gets
	   ;; index -1.
	   (if (null vc-comment-ring-index)
	       (setq vc-comment-ring-index
		     (if (> arg 0) -1
			 (if (< arg 0) 1 0))))
	   (setq vc-comment-ring-index
		 (mod (+ vc-comment-ring-index arg) len))
	   (message "%d" (1+ vc-comment-ring-index))
	   (insert (ring-ref vc-comment-ring vc-comment-ring-index))))))

(defun vc-next-comment (arg)
  "Cycle forwards through comment history."
  (interactive "*p")
  (vc-previous-comment (- arg)))

(defun vc-comment-search-reverse (str)
  "Searches backwards through comment history for substring match."
  (interactive "sComment substring: ")
  (if (string= str "")
      (setq str vc-last-comment-match)
    (setq vc-last-comment-match str))
  (if (null vc-comment-ring-index)
      (setq vc-comment-ring-index -1))
  (let ((str (regexp-quote str))
        (len (ring-length vc-comment-ring))
	(n (1+ vc-comment-ring-index)))
    (while (and (< n len) (not (string-match str (ring-ref vc-comment-ring n))))
      (setq n (+ n 1)))
    (cond ((< n len)
	   (vc-previous-comment (- n vc-comment-ring-index)))
	  (t (error "Not found")))))

(defun vc-comment-search-forward (str)
  "Searches forwards through comment history for substring match."
  (interactive "sComment substring: ")
  (if (string= str "")
      (setq str vc-last-comment-match)
    (setq vc-last-comment-match str))
  (if (null vc-comment-ring-index)
      (setq vc-comment-ring-index 0))
  (let ((str (regexp-quote str))
        (len (ring-length vc-comment-ring))
	(n vc-comment-ring-index))
    (while (and (>= n 0) (not (string-match str (ring-ref vc-comment-ring n))))
      (setq n (- n 1)))
    (cond ((>= n 0)
	   (vc-next-comment (- n vc-comment-ring-index)))
	  (t (error "Not found")))))

;; Additional entry points for examining version histories

;;;###autoload
(defun vc-diff (historic &optional not-urgent)
  "Display diffs between file versions.
Normally this compares the current file and buffer with the most recent 
checked in version of that file.  This uses no arguments.
With a prefix argument, it reads the file name to use
and two version designators specifying which versions to compare."
  (interactive (list current-prefix-arg t))
  (if vc-dired-mode
      (set-buffer (find-file-noselect (dired-get-filename))))
  (while vc-parent-buffer
      (pop-to-buffer vc-parent-buffer))
  (if historic
      (call-interactively 'vc-version-diff)
    (if (or (null buffer-file-name) (null (vc-name buffer-file-name)))
	(error
	 "There is no version-control master associated with this buffer"))
    (let ((file buffer-file-name)
	  unchanged)
      (or (and file (vc-name file))
	  (vc-registration-error file))
      (vc-buffer-sync not-urgent)
      (setq unchanged (vc-workfile-unchanged-p buffer-file-name))
      (if unchanged
	  (message "No changes to %s since latest version" file)
	(vc-backend-diff file)
	;; Ideally, we'd like at this point to parse the diff so that
	;; the buffer effectively goes into compilation mode and we
	;; can visit the old and new change locations via next-error.
	;; Unfortunately, this is just too painful to do.  The basic
	;; problem is that the `old' file doesn't exist to be
	;; visited.  This plays hell with numerous assumptions in
	;; the diff.el and compile.el machinery.
	(set-buffer "*vc-diff*")
	(setq default-directory (file-name-directory file))
	(if (= 0 (buffer-size))
	    (progn
	      (setq unchanged t)
	      (message "No changes to %s since latest version" file))
          (pop-to-buffer "*vc-diff*")
	  (goto-char (point-min))
	  (shrink-window-if-larger-than-buffer)))
      (not unchanged))))

(defun vc-version-diff (file rel1 rel2)
  "For FILE, report diffs between two stored versions REL1 and REL2 of it.
If FILE is a directory, generate diffs between versions for all registered
files in or below it."
  (interactive "FFile or directory to diff: \nsOlder version: \nsNewer version: ")
  (if (string-equal rel1 "") (setq rel1 nil))
  (if (string-equal rel2 "") (setq rel2 nil))
  (if (file-directory-p file)
      (let ((camefrom (current-buffer)))
	(set-buffer (get-buffer-create "*vc-status*"))
	(set (make-local-variable 'vc-parent-buffer) camefrom)
	(set (make-local-variable 'vc-parent-buffer-name)
	     (concat " from " (buffer-name camefrom)))
	(erase-buffer)
	(insert "Diffs between "
		(or rel1 "last version checked in")
		" and "
		(or rel2 "current workfile(s)")
		":\n\n")
	(set-buffer (get-buffer-create "*vc-diff*"))
	(cd file)
	(vc-file-tree-walk
	 default-directory
	 (function (lambda (f)
		     (message "Looking at %s" f)
		     (and
		      (not (file-directory-p f))
		      (vc-registered f)
		      (vc-backend-diff f rel1 rel2)
		      (append-to-buffer "*vc-status*" (point-min) (point-max)))
		     )))
	(pop-to-buffer "*vc-status*")
	(insert "\nEnd of diffs.\n")
	(goto-char (point-min))
	(set-buffer-modified-p nil)
	)
    (if (zerop (vc-backend-diff file rel1 rel2))
	(message "No changes to %s between %s and %s." file rel1 rel2)
      (pop-to-buffer "*vc-diff*"))))

;;;###autoload
(defun vc-version-other-window (rev)
  "Visit version REV of the current buffer in another window.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created."
  (interactive "sVersion to visit (default is latest version): ")
  (if vc-dired-mode
      (set-buffer (find-file-noselect (dired-get-filename))))
  (while vc-parent-buffer
      (pop-to-buffer vc-parent-buffer))
  (if (and buffer-file-name (vc-name buffer-file-name))
      (let* ((version (if (string-equal rev "")
			  (vc-latest-version buffer-file-name)
			rev))
	     (filename (concat buffer-file-name ".~" version "~")))
	 (or (file-exists-p filename)
	     (vc-backend-checkout buffer-file-name nil version filename))
	 (find-file-other-window filename))
    (vc-registration-error buffer-file-name)))

;; Header-insertion code

;;;###autoload
(defun vc-insert-headers ()
  "Insert headers in a file for use with your version-control system.
Headers desired are inserted at the start of the buffer, and are pulled from
the variable `vc-header-alist'."
  (interactive)
  (if vc-dired-mode
      (find-file-other-window (dired-get-filename)))
  (while vc-parent-buffer
      (pop-to-buffer vc-parent-buffer))
  (save-excursion
    (save-restriction
      (widen)
      (if (or (not (vc-check-headers))
	      (y-or-n-p "Version headers already exist.  Insert another set? "))
	  (progn
	    (let* ((delims (cdr (assq major-mode vc-comment-alist)))
		   (comment-start-vc (or (car delims) comment-start "#"))
		   (comment-end-vc (or (car (cdr delims)) comment-end ""))
		   (hdstrings (cdr (assoc (vc-backend (buffer-file-name)) vc-header-alist))))
	      (mapcar (function (lambda (s)
				  (insert comment-start-vc "\t" s "\t"
					  comment-end-vc "\n")))
		      hdstrings)
	      (if vc-static-header-alist
		  (mapcar (function (lambda (f)
				      (if (string-match (car f) buffer-file-name)
					  (insert (format (cdr f) (car hdstrings))))))
			  vc-static-header-alist))
	      )
	    )))))

(defun vc-clear-headers ()
  ;; Clear all version headers in the current buffer, i.e. reset them 
  ;; to the nonexpanded form.  Only implemented for RCS, yet.
  ;; Don't lose point and mark during this.
  (let ((context (vc-buffer-context)))
    (goto-char (point-min))
    (while (re-search-forward "\\$\\([A-Za-z]+\\): [^\\$]+\\$" nil t)
      (replace-match "$\\1$"))
    (vc-restore-buffer-context context)))

;; The VC directory major mode.  Coopt Dired for this.
;; All VC commands get mapped into logical equivalents.

(define-derived-mode vc-dired-mode dired-mode "Dired under VC"
  "The major mode used in VC directory buffers.  It is derived from Dired.
All Dired commands operate normally.  Users currently locking listed files
are listed in place of the file's owner and group.
Keystrokes bound to VC commands will execute as though they had been called
on a buffer attached to the file named in the current Dired buffer line."
  (setq vc-dired-mode t))

(define-key vc-dired-mode-map "\C-xv" vc-prefix-map)
(define-key vc-dired-mode-map "g" 'vc-dired-update)
(define-key vc-dired-mode-map "=" 'vc-diff)

(defun vc-dired-state-info (file)
  ;; Return the string that indicates the version control status
  ;; on a VC dired line.
  (let ((cvs-state (and (eq (vc-backend file) 'CVS)
			(vc-cvs-status file))))
    (if cvs-state
	(cond ((eq cvs-state 'up-to-date) nil)
	      ((eq cvs-state 'needs-checkout)      "patch")
	      ((eq cvs-state 'locally-modified)    "modified")
	      ((eq cvs-state 'needs-merge)         "merge")
	      ((eq cvs-state 'unresolved-conflict) "conflict")
	      ((eq cvs-state 'locally-added)       "added"))
      (vc-locking-user file))))

(defun vc-dired-reformat-line (x)
  ;; Hack a directory-listing line, plugging in locking-user info in
  ;; place of the user and group info.  Should have the beneficial
  ;; side-effect of shortening the listing line.  Each call starts with
  ;; point immediately following the dired mark area on the line to be
  ;; hacked.
  ;;
  ;; Simplest possible one:
  ;; (insert (concat x "\t")))
  ;;
  ;; This code, like dired, assumes UNIX -l format.
  (let ((pos (point)) limit perm owner date-and-file)
    (end-of-line)
    (setq limit (point))
    (goto-char pos)
    (cond
     ((or
       (re-search-forward  ;; owner and group
"\\([drwxlts-]+ \\) *[0-9]+ \\([^ ]+\\) +[^ ]+ +[0-9]+\\( [^ 0-9]+ [0-9 ][0-9] .*\\)"
	  limit t)       
       (re-search-forward  ;; only owner displayed
"\\([drwxlts-]+ \\) *[0-9]+ \\([^ ]+\\) +[0-9]+\\( [^ 0-9]+ [0-9 ][0-9] .*\\)" 
	  limit t))
      (setq perm          (match-string 1)
	    owner         (match-string 2)
	    date-and-file (match-string 3)))
     ((re-search-forward  ;; OS/2 -l format, no links, owner, group
"\\([drwxlts-]+ \\) *[0-9]+\\( [^ 0-9]+ [0-9 ][0-9] .*\\)"
         limit t)
      (setq perm          (match-string 1)
	    date-and-file (match-string 2))))
    (if (numberp x) (setq x (or owner (number-to-string x))))
    (if x (setq x (concat "(" x ")")))
    (let ((rep (substring (concat x "                 ") 0 10)))
      (replace-match (concat perm rep date-and-file)))))
       
(defun vc-dired-update-line (file)
  ;; Update the vc-dired listing line of file -- it is assumed 
  ;; that point is already on this line.  Don't use dired-do-redisplay
  ;; for this, because it cannot handle the way vc-dired deals with 
  ;; subdirectories.
  (beginning-of-line)
  (forward-char 2)
  (let ((start (point)))
    (forward-line 1)
    (beginning-of-line)
    (delete-region start (point))
    (insert-directory file dired-listing-switches)
    (forward-line -1)
    (end-of-line)
    (delete-char (- (length file)))
    (insert (substring file (length (expand-file-name default-directory))))
    (goto-char start))
  (vc-dired-reformat-line (vc-dired-state-info file)))

(defun vc-dired-update (verbose)
  (interactive "P")
  (vc-directory default-directory verbose))

;;; Note in Emacs 18 the following defun gets overridden
;;; with the symbol 'vc-directory-18.  See below.
;;;###autoload
(defun vc-directory (dirname verbose)
  "Show version-control status of the current directory and subdirectories.
Normally it creates a Dired buffer that lists only the locked files
in all these directories.  With a prefix argument, it lists all files."
  (interactive "DDired under VC (directory): \nP")
  (require 'dired)
  (setq dirname (expand-file-name dirname))
  ;; force a trailing slash
  (if (not (eq (elt dirname (1- (length dirname))) ?/))
      (setq dirname (concat dirname "/")))
  (let (nonempty
	(dl (length dirname))
	(filelist nil) (statelist nil)
	(old-dir default-directory)
	dired-buf
	dired-buf-mod-count)
    (vc-file-tree-walk
     dirname
     (function 
      (lambda (f)
	(if (vc-registered f)
	    (let ((state (vc-dired-state-info f)))
	      (and (or verbose state)
		   (setq filelist (cons (substring f dl) filelist))
		   (setq statelist (cons state statelist))))))))
    (save-window-excursion
      (save-excursion
	;; This uses a semi-documented feature of dired; giving a switch
	;; argument forces the buffer to refresh each time.
	(setq dired-buf
	      (dired-internal-noselect
	       (cons dirname (nreverse filelist))
	       dired-listing-switches 'vc-dired-mode))
	(setq nonempty (not (eq 0 (length filelist))))))
    (switch-to-buffer dired-buf)
    ;; Make a few modifications to the header
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (forward-line 1)	      ;; Skip header line
    (let ((start (point)))    ;; Erase (but don't remove) the 
      (end-of-line)           ;; "wildcard" line.
      (delete-region start (point)))
    (beginning-of-line)
    (if nonempty
	(progn
	  ;; Plug the version information into the individual lines
	  (mapcar
	   (function
	    (lambda (x)
	     (forward-char 2)	;; skip dired's mark area
	     (vc-dired-reformat-line x)
	     (forward-line 1)))	;; go to next line
	   (nreverse statelist))
	  (setq buffer-read-only t)
	  (goto-char (point-min))
	  (dired-next-line 2)
	  )
      (dired-next-line 1) 
      (insert "  ")
      (setq buffer-read-only t)
      (message "No files are currently %s under %s"
	       (if verbose "registered" "locked") dirname))
    ))

;; Emacs 18 version
(defun vc-directory-18 (verbose)
  "Show version-control status of all files under the current directory."
  (interactive "P")
  (let (nonempty (dir default-directory))
    (save-excursion
      (set-buffer (get-buffer-create "*vc-status*"))
      (erase-buffer)
      (cd dir)
      (vc-file-tree-walk
       default-directory
       (function (lambda (f)
		   (if (vc-registered f)
		       (let ((user (vc-locking-user f)))
			 (if (or user verbose)
			     (insert (format
				      "%s	%s\n"
				      (concat user) f))))))))
      (setq nonempty (not (zerop (buffer-size)))))

    (if nonempty
	(progn
	  (pop-to-buffer "*vc-status*" t)
	  (goto-char (point-min))
	  (shrink-window-if-larger-than-buffer)))
      (message "No files are currently %s under %s"
	       (if verbose "registered" "locked") default-directory))
    )

(or (boundp 'minor-mode-map-alist)
    (fset 'vc-directory 'vc-directory-18))

;; Named-configuration support for SCCS

(defun vc-add-triple (name file rev)
  (save-excursion
    (find-file (expand-file-name
		vc-name-assoc-file
		(file-name-as-directory
		 (expand-file-name (vc-backend-subdirectory-name file) 
				   (file-name-directory file)))))
    (goto-char (point-max))
    (insert name "\t:\t" file "\t" rev "\n")
    (basic-save-buffer)
    (kill-buffer (current-buffer))
    ))

(defun vc-record-rename (file newname)
  (save-excursion
    (find-file
     (expand-file-name
      vc-name-assoc-file
      (file-name-as-directory
       (expand-file-name (vc-backend-subdirectory-name file) 
			 (file-name-directory file)))))
    (goto-char (point-min))
    ;; (replace-regexp (concat ":" (regexp-quote file) "$") (concat ":" newname))
    (while (re-search-forward (concat ":" (regexp-quote file) "$") nil t)
      (replace-match (concat ":" newname) nil nil))
    (basic-save-buffer)
    (kill-buffer (current-buffer))
    ))

(defun vc-lookup-triple (file name)
  ;; Return the numeric version corresponding to a named snapshot of file
  ;; If name is nil or a version number string it's just passed through
  (cond ((null name) name)
	((let ((firstchar (aref name 0)))
	   (and (>= firstchar ?0) (<= firstchar ?9)))
	 name)
	(t
	 (save-excursion
	   (set-buffer (get-buffer-create "*vc-info*"))
	   (vc-insert-file
	    (expand-file-name
	     vc-name-assoc-file
	     (file-name-as-directory
	      (expand-file-name (vc-backend-subdirectory-name file) 
				(file-name-directory file)))))
	   (prog1
	       (car (vc-parse-buffer
		     (list (list (concat name "\t:\t" file "\t\\(.+\\)") 1))))
	     (kill-buffer "*vc-info*"))))
	 ))

;; Named-configuration entry points

(defun vc-snapshot-precondition ()
  ;; Scan the tree below the current directory.
  ;; If any files are locked, return the name of the first such file.
  ;; (This means, neither snapshot creation nor retrieval is allowed.)
  ;; If one or more of the files are currently visited, return `visited'.
  ;; Otherwise, return nil.
  (let ((status nil))
    (catch 'vc-locked-example
      (vc-file-tree-walk
       default-directory
       (function (lambda (f)
		   (and (vc-registered f)
			(if (vc-locking-user f) (throw 'vc-locked-example f)
			  (if (get-file-buffer f) (setq status 'visited)))))))
      status)))

;;;###autoload
(defun vc-create-snapshot (name)
  "Make a snapshot called NAME.
The snapshot is made from all registered files at or below the current
directory.  For each file, the version level of its latest
version becomes part of the named configuration."
  (interactive "sNew snapshot name: ")
  (let ((result (vc-snapshot-precondition)))
    (if (stringp result)
	(error "File %s is locked" result)
      (vc-file-tree-walk
       default-directory
       (function (lambda (f) (and
			      (vc-name f)
			      (vc-backend-assign-name f name)))))
      )))

;;;###autoload
(defun vc-retrieve-snapshot (name)
  "Retrieve the snapshot called NAME.
This function fails if any files are locked at or below the current directory
Otherwise, all registered files are checked out (unlocked) at their version
levels in the snapshot."
  (interactive "sSnapshot name to retrieve: ")
  (let ((result (vc-snapshot-precondition))
	(update nil))
    (if (stringp result)
	(error "File %s is locked" result)
      (if (eq result 'visited)
	  (setq update (yes-or-no-p "Update the affected buffers? ")))
      (vc-file-tree-walk
       default-directory
       (function (lambda (f) (and
			      (vc-name f)
			      (vc-error-occurred
			       (vc-backend-checkout f nil name)
			       (if update (vc-resynch-buffer f t t)))))))
      )))

;; Miscellaneous other entry points

;;;###autoload
(defun vc-print-log ()
  "List the change log of the current buffer in a window."
  (interactive)
  (if vc-dired-mode
      (set-buffer (find-file-noselect (dired-get-filename))))
  (while vc-parent-buffer
      (pop-to-buffer vc-parent-buffer))
  (if (and buffer-file-name (vc-name buffer-file-name))
      (let ((file buffer-file-name))
	(vc-backend-print-log file)
	(pop-to-buffer (get-buffer-create "*vc*"))
	(setq default-directory (file-name-directory file))
	(while (looking-at "=*\n")
	  (delete-char (- (match-end 0) (match-beginning 0)))
	  (forward-line -1))
	(goto-char (point-min))
	(if (looking-at "[\b\t\n\v\f\r ]+")
	    (delete-char (- (match-end 0) (match-beginning 0))))
	(shrink-window-if-larger-than-buffer)
	)
    (vc-registration-error buffer-file-name)
    )
  )

;;;###autoload
(defun vc-revert-buffer ()
  "Revert the current buffer's file back to the latest checked-in version.
This asks for confirmation if the buffer contents are not identical
to that version.
If the back-end is CVS, this will give you the most recent revision of
the file on the branch you are editing."
  (interactive)
  (if vc-dired-mode
      (find-file-other-window (dired-get-filename)))
  (while vc-parent-buffer
      (pop-to-buffer vc-parent-buffer))
  (let ((file buffer-file-name)
	;; This operation should always ask for confirmation.
	(vc-suppress-confirm nil)
	(obuf (current-buffer)) (changed (vc-diff nil t)))
    (if (and changed (not (yes-or-no-p "Discard changes? ")))
	(progn
	  (if (and (window-dedicated-p (selected-window))
		   (one-window-p t 'selected-frame))
	      (make-frame-invisible (selected-frame))
	    (delete-window))
	  (error "Revert cancelled"))
      (set-buffer obuf))
    (if changed
	(if (and (window-dedicated-p (selected-window))
		 (one-window-p t 'selected-frame))
	    (make-frame-invisible (selected-frame))
	  (delete-window)))
    (vc-backend-revert file)
    (vc-resynch-window file t t)
    )
  )

;;;###autoload
(defun vc-cancel-version (norevert)
  "Get rid of most recently checked in version of this file.
A prefix argument means do not revert the buffer afterwards."
  (interactive "P")
  (if vc-dired-mode
      (find-file-other-window (dired-get-filename)))
  (while vc-parent-buffer
    (pop-to-buffer vc-parent-buffer))
  (cond 
   ((not (vc-registered (buffer-file-name)))
    (vc-registration-error (buffer-file-name)))
   ((eq (vc-backend (buffer-file-name)) 'CVS)
    (error "Unchecking files under CVS is dangerous and not supported in VC"))
   ((vc-locking-user (buffer-file-name))
    (error "This version is locked; use vc-revert-buffer to discard changes"))
   ((not (vc-latest-on-branch-p (buffer-file-name)))
    (error "This is not the latest version--VC cannot cancel it")))
  (let* ((target (vc-workfile-version (buffer-file-name)))
         (recent (if (vc-trunk-p target) "" (vc-branch-part target)))
         (config (current-window-configuration)) done)
    (if (null (yes-or-no-p (format "Remove version %s from master? " target)))
	nil
      (setq norevert (or norevert (not 
           (yes-or-no-p "Revert buffer to most recent remaining version? "))))
      (vc-backend-uncheck (buffer-file-name) target)
      ;; Check out the most recent remaining version.  If it fails, because
      ;; the whole branch got deleted, do a double-take and check out the
      ;; version where the branch started.
      (while (not done)
        (condition-case err
            (progn
              (if norevert
                  ;; Check out locked, but only to disc, and keep 
                  ;; modifications in the buffer.
                  (vc-backend-checkout (buffer-file-name) t recent)
                ;; Check out unlocked, and revert buffer.
                (vc-checkout (buffer-file-name) nil recent))
              (setq done t))
          ;; If the checkout fails, vc-do-command signals an error.
          ;; We catch this error, check the reason, correct the
          ;; version number, and try a second time.
          (error (set-buffer "*vc*")
                 (goto-char (point-min))
                 (if (search-forward "no side branches present for" nil t)
                     (progn (setq recent (vc-branch-part recent))
                            ;; vc-do-command popped up a window with
                            ;; the error message.  Get rid of it, by
                            ;; restoring the old window configuration.
                            (set-window-configuration config))
                   ;; No, it was some other error: re-signal it.
                   (signal (car err) (cdr err))))))
      ;; If norevert, clear version headers and mark the buffer modified.
      (if norevert
          (progn
            (set-visited-file-name (buffer-file-name))
            (if (not vc-make-backup-files)
                ;; inhibit backup for this buffer
                (progn (make-local-variable 'backup-inhibited)
                       (setq backup-inhibited t)))
            (if (eq (vc-backend (buffer-file-name)) 'RCS)
                (progn (setq buffer-read-only nil)
                       (vc-clear-headers)))
            (vc-mode-line (buffer-file-name))))
      (message "Version %s has been removed from the master" target)
      )))

;;;###autoload
(defun vc-rename-file (old new)
  "Rename file OLD to NEW, and rename its master file likewise."
  (interactive "fVC rename file: \nFRename to: ")
  ;; There are several ways of renaming files under CVS 1.3, but they all
  ;; have serious disadvantages.  See the FAQ (available from think.com in
  ;; pub/cvs/).  I'd rather send the user an error, than do something he might
  ;; consider to be wrong.  When the famous, long-awaited rename database is
  ;; implemented things might change for the better.  This is unlikely to occur
  ;; until CVS 2.0 is released.  --ceder 1994-01-23 21:27:51
  (if (eq (vc-backend old) 'CVS)
      (error "Renaming files under CVS is dangerous and not supported in VC"))
  (let ((oldbuf (get-file-buffer old)))
    (if (and oldbuf (buffer-modified-p oldbuf))
	(error "Please save files before moving them"))
    (if (get-file-buffer new)
	(error "Already editing new file name"))
    (if (file-exists-p new)
	(error "New file already exists"))
    (let ((oldmaster (vc-name old)))
      (if oldmaster
	  (progn
	    (if (vc-locking-user old)
		(error "Please check in files before moving them"))
	    (if (or (file-symlink-p oldmaster)
		    ;; This had FILE, I changed it to OLD. -- rms.
		    (file-symlink-p (vc-backend-subdirectory-name old)))
		(error "This is not a safe thing to do in the presence of symbolic links"))
	    (rename-file
	     oldmaster
	     (let ((backend (vc-backend old))
		   (newdir (or (file-name-directory new) ""))
		   (newbase (file-name-nondirectory new)))
	       (catch 'found
		 (mapcar
		  (function
		   (lambda (s)
		     (if (eq backend (cdr s))
			 (let* ((newmaster (format (car s) newdir newbase))
				(newmasterdir (file-name-directory newmaster)))
			   (if (or (not newmasterdir)
				   (file-directory-p newmasterdir))
			       (throw 'found newmaster))))))
		  vc-master-templates)
		 (error "New file lacks a version control directory"))))))
      (if (or (not oldmaster) (file-exists-p old))
	  (rename-file old new)))
; ?? Renaming a file might change its contents due to keyword expansion.
; We should really check out a new copy if the old copy was precisely equal
; to some checked in version.  However, testing for this is tricky....
    (if oldbuf
	(save-excursion
	  (set-buffer oldbuf)
	  (let ((buffer-read-only buffer-read-only))
	    (set-visited-file-name new))
	  (vc-backend new)
	  (vc-mode-line new)
	  (set-buffer-modified-p nil))))
  ;; This had FILE, I changed it to OLD. -- rms.
  (vc-backend-dispatch old
		       (vc-record-rename old new) ;SCCS
		       nil		;RCS
		       nil		;CVS
		       )
  )

;;;###autoload
(defun vc-update-change-log (&rest args)
  "Find change log file and add entries from recent RCS logs.
The mark is left at the end of the text prepended to the change log.
With prefix arg of C-u, only find log entries for the current buffer's file.
With any numeric prefix arg, find log entries for all files currently visited.
Otherwise, find log entries for all registered files in the default directory.
From a program, any arguments are passed to the `rcs2log' script."
  (interactive
   (cond ((consp current-prefix-arg)	;C-u
	  (list buffer-file-name))
	 (current-prefix-arg		;Numeric argument.
	  (let ((files nil)
		(buffers (buffer-list))
		file)
	    (while buffers
	      (setq file (buffer-file-name (car buffers)))
	      (and file (vc-backend file)
		   (setq files (cons file files)))
	      (setq buffers (cdr buffers)))
	    files))
	 (t
	  (let ((RCS (concat default-directory "RCS")))
	    (and (file-directory-p RCS)
		 (mapcar (function
			  (lambda (f)
			    (if (string-match "\\(.*\\),v$" f)
				(substring f 0 (match-end 1))
			      f)))
			 (directory-files RCS nil "...\\|^[^.]\\|^.[^.]")))))))
  (let ((odefault default-directory)
	(full-name (or add-log-full-name
		       (user-full-name)))
	(mailing-address (or add-log-mailing-address
			     user-mail-address)))
    (find-file-other-window (find-change-log))
    (barf-if-buffer-read-only)
    (vc-buffer-sync)
    (undo-boundary)
    (goto-char (point-min))
    (push-mark)
    (message "Computing change log entries...")
    (message "Computing change log entries... %s"
	     (if (or (null args)
		     (eq 0 (apply 'call-process "rcs2log" nil t nil
				  "-u"
				  (concat (user-login-name)
					  "\t"
					  full-name
					  "\t"
					  mailing-address)
				  (mapcar (function
					   (lambda (f)
					     (file-relative-name
					      (if (file-name-absolute-p f)
						  f
						(concat odefault f)))))
					  args))))
		 "done" "failed"))))

;; Collect back-end-dependent stuff here

(defun vc-backend-admin (file &optional rev comment)
  ;; Register a file into the version-control system
  ;; Automatically retrieves a read-only version of the file with
  ;; keywords expanded if vc-keep-workfiles is non-nil, otherwise
  ;; it deletes the workfile.
  (vc-file-clearprops file)
  (or vc-default-back-end
      (setq vc-default-back-end (if (vc-find-binary "rcs") 'RCS 'SCCS)))
  (message "Registering %s..." file)
  (let ((backend
	 (cond
	  ((file-exists-p (vc-backend-subdirectory-name)) vc-default-back-end)
	  ((file-exists-p "RCS") 'RCS)
	  ((file-exists-p "SCCS") 'SCCS)
	  ((file-exists-p "CVS") 'CVS)
	  (t vc-default-back-end))))
    (cond ((eq backend 'SCCS)
	   (vc-do-command nil 0 "admin" file 'MASTER	;; SCCS
			  (and rev (concat "-r" rev))
			  "-fb"
			  (concat "-i" file)
			  (and comment (concat "-y" comment))
			  (format
			   (car (rassq 'SCCS vc-master-templates))
			   (or (file-name-directory file) "")
			   (file-name-nondirectory file)))
	   (delete-file file)
	   (if vc-keep-workfiles
	       (vc-do-command nil 0 "get" file 'MASTER)))
	  ((eq backend 'RCS)
	   (vc-do-command nil 0 "ci" file 'MASTER	;; RCS
                          ;; if available, use the secure registering option
			  (and (vc-backend-release-p 'RCS "5.6.4") "-i")
			  (concat (if vc-keep-workfiles "-u" "-r") rev)
			  (and comment (concat "-t-" comment))
			  file))
	  ((eq backend 'CVS)
	   (vc-do-command nil 0 "cvs" file 'WORKFILE ;; CVS
			  "add"
			  (and comment (string-match "[^\t\n ]" comment)
			       (concat "-m" comment)))
	   )))
  (message "Registering %s...done" file)
  )

(defun vc-backend-checkout (file &optional writable rev workfile)
  ;; Retrieve a copy of a saved version into a workfile
  (let ((filename (or workfile file))
	(file-buffer (get-file-buffer file))
	switches)
    (message "Checking out %s..." filename)
    (save-excursion
      ;; Change buffers to get local value of vc-checkout-switches.
      (if file-buffer (set-buffer file-buffer))
      (setq switches (if (stringp vc-checkout-switches)
			 (list vc-checkout-switches)
		       vc-checkout-switches))
      ;; Save this buffer's default-directory
      ;; and use save-excursion to make sure it is restored
      ;; in the same buffer it was saved in.
      (let ((default-directory default-directory))
	(save-excursion
	  ;; Adjust the default-directory so that the check-out creates 
	  ;; the file in the right place.
	  (setq default-directory (file-name-directory filename))
	  (vc-backend-dispatch file
	    (progn  ;; SCCS
	      (and rev (string= rev "") (setq rev nil))
	      (if workfile  
		  ;; Some SCCS implementations allow checking out directly to a
		  ;; file using the -G option, but then some don't so use the
		  ;; least common denominator approach and use the -p option
		  ;; ala RCS.
		  (let ((vc-modes (logior (file-modes (vc-name file))
					  (if writable 128 0)))
			(failed t))
		    (unwind-protect
			(progn
			  (apply 'vc-do-command
				 nil 0 "/bin/sh" file 'MASTER "-c"
				 ;; Some shells make the "" dummy argument into $0
				 ;; while others use the shell's name as $0 and
				 ;; use the "" as $1.  The if-statement
				 ;; converts the latter case to the former.
				 (format "if [ x\"$1\" = x ]; then shift; fi; \
			       umask %o; exec >\"$1\" || exit; \
			       shift; umask %o; exec get \"$@\""
				       (logand 511 (lognot vc-modes))
				       (logand 511 (lognot (default-file-modes))))
				 ""		; dummy argument for shell's $0
				 filename 
				 (if writable "-e")
				 "-p" 
				 (and rev
				      (concat "-r" (vc-lookup-triple file rev)))
				 switches)
			  (setq failed nil))
		      (and failed (file-exists-p filename) 
			   (delete-file filename))))
		(apply 'vc-do-command nil 0 "get" file 'MASTER   ;; SCCS
		       (if writable "-e")
		       (and rev (concat "-r" (vc-lookup-triple file rev)))
		       switches)
		(vc-file-setprop file 'vc-workfile-version nil)))
	    (if workfile  ;; RCS
		;; RCS doesn't let us check out into arbitrary file names directly.
		;; Use `co -p' and make stdout point to the correct file.
		(let ((vc-modes (logior (file-modes (vc-name file))
					(if writable 128 0)))
		      (failed t))
		  (unwind-protect
		      (progn
			(apply 'vc-do-command
			       nil 0 "/bin/sh" file 'MASTER "-c"
			       ;; See the SCCS case, above, regarding the
			       ;; if-statement.
			       (format "if [ x\"$1\" = x ]; then shift; fi; \
			       umask %o; exec >\"$1\" || exit; \
			       shift; umask %o; exec co \"$@\""
				       (logand 511 (lognot vc-modes))
				       (logand 511 (lognot (default-file-modes))))
			       ""		; dummy argument for shell's $0
			       filename
			       (if writable "-l")
			       (concat "-p" rev)
			       switches)
			(setq failed nil))
		    (and failed (file-exists-p filename) (delete-file filename))))
	      (let (new-version)
		;; if we should go to the head of the trunk, 
		;; clear the default branch first
		(and rev (string= rev "") 
		     (vc-do-command nil 0 "rcs" file 'MASTER "-b"))
		;; now do the checkout
		(apply 'vc-do-command
		       nil 0 "co" file 'MASTER
		       ;; If locking is not strict, force to overwrite
		       ;; the writable workfile.
		       (if (eq (vc-checkout-model file) 'implicit) "-f")
		       (if writable "-l")
		       (if rev (concat "-r" rev)
			 ;; if no explicit revision was specified,
			 ;; check out that of the working file
			 (let ((workrev (vc-workfile-version file)))
			   (if workrev (concat "-r" workrev)
			     nil)))
		       switches)
		;; determine the new workfile version
		(save-excursion
		  (set-buffer "*vc*")
		  (goto-char (point-min))
		  (setq new-version 
			(if (re-search-forward "^revision \\([0-9.]+\\).*\n" nil t)
			    (buffer-substring (match-beginning 1) (match-end 1)))))
		(vc-file-setprop file 'vc-workfile-version new-version)
		;; if necessary, adjust the default branch
		(and rev (not (string= rev ""))
		     (vc-do-command nil 0 "rcs" file 'MASTER 
			(concat "-b" (if (vc-latest-on-branch-p file)
					 (if (vc-trunk-p new-version) nil
					   (vc-branch-part new-version))
				       new-version))))))
	    (if workfile  ;; CVS
		;; CVS is much like RCS
		(let ((failed t))
		  (unwind-protect
		      (progn
			(apply 'vc-do-command
			       nil 0 "/bin/sh" file 'WORKFILE "-c"
			       "exec >\"$1\" || exit; shift; exec cvs update \"$@\""
			       ""		; dummy argument for shell's $0
			       workfile
			       (concat "-r" rev)
			       "-p"
			       switches)
			(setq failed nil))
		    (and failed (file-exists-p filename) (delete-file filename))))
	      ;; default for verbose checkout: clear the sticky tag
	      ;; so that the actual update will get the head of the trunk
	      (and rev (string= rev "")
		   (vc-do-command nil 0 "cvs" file 'WORKFILE "update" "-A"))
	      ;; If a revision was specified, check that out.
	      (if rev
		  (apply 'vc-do-command nil 0 "cvs" file 'WORKFILE 
			 (and writable (eq (vc-checkout-model file) 'manual) "-w")
			 "update"
			 (and rev (not (string= rev ""))
			      (concat "-r" rev))
			 switches)
		;; If no revision was specified, simply make the file writable.
		(and writable 
		     (or (eq (vc-checkout-model file) 'manual)
			 (zerop (logand 128 (file-modes file))))
		     (set-file-modes file (logior 128 (file-modes file)))))
	      (if rev (vc-file-setprop file 'vc-workfile-version nil))))
	  (cond 
	   ((not workfile)
	    (vc-file-clear-masterprops file)
	    (if writable 
		(vc-file-setprop file 'vc-locking-user (user-login-name)))
	    (vc-file-setprop file
			     'vc-checkout-time (nth 5 (file-attributes file)))))
	  (message "Checking out %s...done" filename))))))

(defun vc-backend-logentry-check (file)
  (vc-backend-dispatch file
   (if (>= (buffer-size) 512)	;; SCCS
       (progn
	 (goto-char 512)
	 (error
	  "Log must be less than 512 characters; point is now at pos 512")))
   nil    ;; RCS
   nil)   ;; CVS
  )

(defun vc-backend-checkin (file rev comment)
  ;; Register changes to FILE as level REV with explanatory COMMENT.
  ;; Automatically retrieves a read-only version of the file with
  ;; keywords expanded if vc-keep-workfiles is non-nil, otherwise
  ;; it deletes the workfile.
  ;;   Adaptation for RCS branch support: if this is an explicit checkin,
  ;; or if the checkin creates a new branch, set the master file branch
  ;; accordingly.
  (message "Checking in %s..." file)
  ;; "This log message intentionally left almost blank".
  ;; RCS 5.7 gripes about white-space-only comments too.
  (or (and comment (string-match "[^\t\n ]" comment))
      (setq comment "*** empty log message ***"))
  (save-excursion
    ;; Change buffers to get local value of vc-checkin-switches.
    (set-buffer (or (get-file-buffer file) (current-buffer)))
    (let ((switches
	   (if (stringp vc-checkin-switches)
	       (list vc-checkin-switches)
	     vc-checkin-switches)))
      ;; Clear the master-properties.  Do that here, not at the
      ;; end, because if the check-in fails we want them to get
      ;; re-computed before the next try.
      (vc-file-clear-masterprops file)
      (vc-backend-dispatch file
	;; SCCS
	(progn
	  (apply 'vc-do-command nil 0 "delta" file 'MASTER
		 (if rev (concat "-r" rev))
		 (concat "-y" comment)
		 switches)
	  (vc-file-setprop file 'vc-locking-user 'none)
	  (vc-file-setprop file 'vc-workfile-version nil)
	  (if vc-keep-workfiles
	      (vc-do-command nil 0 "get" file 'MASTER))
	  )
	;; RCS
	(let ((old-version (vc-workfile-version file)) new-version)
	  (apply 'vc-do-command nil 0 "ci" file 'MASTER
		 ;; if available, use the secure check-in option
		 (and (vc-backend-release-p 'RCS "5.6.4") "-j")
		 (concat (if vc-keep-workfiles "-u" "-r") rev)
		 (concat "-m" comment)
		 switches)
	  (vc-file-setprop file 'vc-locking-user 'none)
	  (vc-file-setprop file 'vc-workfile-version nil)

	  ;; determine the new workfile version
	  (set-buffer "*vc*")
	  (goto-char (point-min))
	  (if (or (re-search-forward 
		   "new revision: \\([0-9.]+\\);" nil t)
		  (re-search-forward 
		   "reverting to previous revision \\([0-9.]+\\)" nil t))
	      (progn (setq new-version (buffer-substring (match-beginning 1)
							 (match-end 1)))
		     (vc-file-setprop file 'vc-workfile-version new-version)))

	  ;; if we got to a different branch, adjust the default
	  ;; branch accordingly
	  (cond 
	   ((and old-version new-version
		 (not (string= (vc-branch-part old-version)
			       (vc-branch-part new-version))))
	    (vc-do-command nil 0 "rcs" file 'MASTER 
			   (if (vc-trunk-p new-version) "-b"
			     (concat "-b" (vc-branch-part new-version))))
	    ;; If this is an old RCS release, we might have 
	    ;; to remove a remaining lock.
	    (if (not (vc-backend-release-p 'RCS "5.6.2"))
		;; exit status of 1 is also accepted.
		;; It means that the lock was removed before.
		(vc-do-command nil 1 "rcs" file 'MASTER 
			       (concat "-u" old-version))))))
	;; CVS
	(progn
	  ;; explicit check-in to the trunk requires a 
	  ;; double check-in (first unexplicit) (CVS-1.3)
	  (condition-case nil
	      (progn
		(if (and rev (vc-trunk-p rev))
		    (apply 'vc-do-command nil 0 "cvs" file 'WORKFILE 
			   "ci" "-m" "intermediate"
			   switches))
		(apply 'vc-do-command nil 0 "cvs" file 'WORKFILE 
		       "ci" (if rev (concat "-r" rev))
		       (concat "-m" comment)
		       switches))
	    (error (if (eq (vc-cvs-status file) 'needs-merge)
		       ;; The CVS output will be on top of this message.
		       (error "Type C-x 0 C-x C-q to merge in changes")
		     (error "Check-in failed"))))
	  ;; determine and store the new workfile version
	  (set-buffer "*vc*")
	  (goto-char (point-min))
	  (if (re-search-forward 
	       "^\\(new\\|initial\\) revision: \\([0-9.]+\\)" nil t)
	      (vc-file-setprop file 'vc-workfile-version 
			       (buffer-substring (match-beginning 2)
						 (match-end 2)))
	    (vc-file-setprop file 'vc-workfile-version nil))
	  ;; if this was an explicit check-in, remove the sticky tag
	  (if rev
	      (vc-do-command nil 0 "cvs" file 'WORKFILE "update" "-A"))
	  (vc-file-setprop file 'vc-locking-user 'none)
	  (vc-file-setprop file 'vc-checkout-time 
			   (nth 5 (file-attributes file)))))))
  (message "Checking in %s...done" file))

(defun vc-backend-revert (file)
  ;; Revert file to latest checked-in version.
  ;; (for RCS, to workfile version)
  (message "Reverting %s..." file)
  (vc-file-clear-masterprops file)
  (vc-backend-dispatch
   file
   ;; SCCS
   (progn
     (vc-do-command nil 0 "unget" file 'MASTER nil)
     (vc-do-command nil 0 "get" file 'MASTER nil))
   ;; RCS
   (vc-do-command nil 0 "co" file 'MASTER
		  "-f" (concat "-u" (vc-workfile-version file)))
   ;; CVS
   (progn
     (delete-file file)
     (vc-do-command nil 0 "cvs" file 'WORKFILE "update")))
  (vc-file-setprop file 'vc-locking-user 'none)
  (vc-file-setprop file 'vc-checkout-time (nth 5 (file-attributes file)))
  (message "Reverting %s...done" file)
  )

(defun vc-backend-steal (file &optional rev)
  ;; Steal the lock on the current workfile.  Needs RCS 5.6.2 or later for -M.
  (message "Stealing lock on %s..." file)
  (vc-backend-dispatch file
   (progn				;SCCS
     (vc-do-command nil 0 "unget" file 'MASTER "-n" (if rev (concat "-r" rev)))
     (vc-do-command nil 0 "get" file 'MASTER "-g" (if rev (concat "-r" rev)))
     )
   (vc-do-command nil 0 "rcs" file 'MASTER	;RCS
		  "-M" (concat "-u" rev) (concat "-l" rev))
   (error "You cannot steal a CVS lock; there are no CVS locks to steal") ;CVS
   )
  (vc-file-setprop file 'vc-locking-user (user-login-name))
  (message "Stealing lock on %s...done" file)
  )  

(defun vc-backend-uncheck (file target)
  ;; Undo the latest checkin.
  (message "Removing last change from %s..." file)
  (vc-backend-dispatch file
   (vc-do-command nil 0 "rmdel" file 'MASTER (concat "-r" target))
   (vc-do-command nil 0 "rcs" file 'MASTER (concat "-o" target))
   nil  ;; this is never reached under CVS
   )
  (message "Removing last change from %s...done" file)
  )

(defun vc-backend-print-log (file)
  ;; Get change log associated with FILE.
  (vc-backend-dispatch 
   file
   (vc-do-command nil 0 "prs" file 'MASTER)
   (vc-do-command nil 0 "rlog" file 'MASTER)
   (vc-do-command nil 0 "cvs" file 'WORKFILE "rlog")))

(defun vc-backend-assign-name (file name)
  ;; Assign to a FILE's latest version a given NAME.
  (vc-backend-dispatch file
   (vc-add-triple name file (vc-latest-version file))              ;; SCCS
   (vc-do-command nil 0 "rcs" file 'MASTER (concat "-n" name ":")) ;; RCS
   (vc-do-command nil 0 "cvs" file 'WORKFILE "tag" name)	   ;; CVS
   )
  )

(defun vc-backend-diff (file &optional oldvers newvers cmp)
  ;; Get a difference report between two versions of FILE.
  ;; Get only a brief comparison report if CMP, a difference report otherwise.
  (let ((backend (vc-backend file)))
    (cond
     ((eq backend 'SCCS)
      (setq oldvers (vc-lookup-triple file oldvers))
      (setq newvers (vc-lookup-triple file newvers)))
     ((eq backend 'RCS)
      (if (not oldvers) (setq oldvers (vc-workfile-version file)))
      ;; If we know that --brief is not supported, don't try it.
      (setq cmp (and cmp (not (eq vc-rcsdiff-knows-brief 'no))))))
     ;; SCCS and RCS shares a lot of code.
    (cond
     ((or (eq backend 'SCCS) (eq backend 'RCS))
      (let* ((command (if (eq backend 'SCCS) "vcdiff" "rcsdiff"))
	     (mode (if (eq backend 'RCS) 'WORKFILE 'MASTER))
	     (options (append (list (and cmp "--brief")
				    "-q"
				    (and oldvers (concat "-r" oldvers))
				    (and newvers (concat "-r" newvers)))
			      (and (not cmp)
				   (if (listp diff-switches)
				       diff-switches
				     (list diff-switches)))))
	     (status (apply 'vc-do-command "*vc-diff*" 2 
			    command file mode options)))
	;; If --brief didn't work, do a double-take and remember it 
        ;; for the future.
	(if (eq status 2)
            (prog1
                (apply 'vc-do-command "*vc-diff*" 1 command file 'WORKFILE
                       (if cmp (cdr options) options))
              (if cmp (setq vc-rcsdiff-knows-brief 'no)))
          ;; If --brief DID work, remember that, too.
	  (and cmp (not vc-rcsdiff-knows-brief)
               (setq vc-rcsdiff-knows-brief 'yes))
          status)))
     ;; CVS is different.  
     ((eq backend 'CVS)
      (if (string= (vc-workfile-version file) "0") ;CVS
	  ;; This file is added but not yet committed; there is no master file.
	  (if (or oldvers newvers)
	      (error "No revisions of %s exist" file)
	    (if cmp 1 ;; file is added but not committed, 
	              ;; we regard this as "changed".
	      ;; diff it against /dev/null.
	      (apply 'vc-do-command
		     "*vc-diff*" 1 "diff" file 'WORKFILE
		     (append (if (listp diff-switches) 
				 diff-switches
			       (list diff-switches)) '("/dev/null")))))
	;; cmp is not yet implemented -- we always do a full diff.
	(apply 'vc-do-command
	       "*vc-diff*" 1 "cvs" file 'WORKFILE "diff"
	       (and oldvers (concat "-r" oldvers))
	       (and newvers (concat "-r" newvers))
	       (if (listp diff-switches)
		   diff-switches
		 (list diff-switches)))))
     (t
      (vc-registration-error file)))))

(defun vc-backend-merge-news (file)
  ;; Merge in any new changes made to FILE.
  (message "Merging changes into %s..." file)
  (prog1
      (vc-backend-dispatch 
       file
       (error "vc-backend-merge-news not meaningful for SCCS files") ;SCCS
       (error "vc-backend-merge-news not meaningful for RCS files")	;RCS
       (save-excursion  ; CVS
	 (vc-file-clear-masterprops file)
	 (vc-file-setprop file 'vc-workfile-version nil)
	 (vc-file-setprop file 'vc-locking-user nil)
	 (vc-do-command nil 0 "cvs" file 'WORKFILE "update")
	 ;; CVS doesn't return an error code if conflicts are detected.
	 ;; Since we want to warn the user about it (and possibly start
	 ;; emerge later), scan the output and see if this occurred.
	 (set-buffer (get-buffer "*vc*"))
	 (goto-char (point-min))
	 (if (re-search-forward "^cvs update: conflicts found in .*" nil t)
	     1  ;; error code for caller
	   0  ;; no conflict detected
	   )))
    (message "Merging changes into %s...done" file)))

(defun vc-check-headers ()
  "Check if the current file has any headers in it."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (vc-backend-dispatch buffer-file-name
     (re-search-forward  "%[MIRLBSDHTEGUYFPQCZWA]%" nil t)	;; SCCS
     (re-search-forward "\\$[A-Za-z\300-\326\330-\366\370-\377]+\\(: [\t -#%-\176\240-\377]*\\)?\\$" nil t)		;; RCS
     'RCS				;; CVS works like RCS in this regard.
     )
    ))

;; Back-end-dependent stuff ends here.

;; Set up key bindings for use while editing log messages

(defun vc-log-mode ()
  "Minor mode for driving version-control tools.
These bindings are added to the global keymap when you enter this mode:
\\[vc-next-action]		perform next logical version-control operation on current file
\\[vc-register]			register current file
\\[vc-toggle-read-only]		like next-action, but won't register files
\\[vc-insert-headers]		insert version-control headers in current file
\\[vc-print-log]		display change history of current file
\\[vc-revert-buffer]		revert buffer to latest version
\\[vc-cancel-version]		undo latest checkin
\\[vc-diff]		show diffs between file versions
\\[vc-version-other-window]		visit old version in another window
\\[vc-directory]		show all files locked by any user in or below .
\\[vc-update-change-log]		add change log entry from recent checkins

While you are entering a change log message for a version, the following
additional bindings will be in effect.

\\[vc-finish-logentry]	proceed with check in, ending log message entry

Whenever you do a checkin, your log comment is added to a ring of
saved comments.  These can be recalled as follows:

\\[vc-next-comment]	replace region with next message in comment ring
\\[vc-previous-comment]	replace region with previous message in comment ring
\\[vc-comment-search-reverse]	search backward for regexp in the comment ring
\\[vc-comment-search-forward]	search backward for regexp in the comment ring

Entry to the change-log submode calls the value of text-mode-hook, then
the value of vc-log-mode-hook.

Global user options:
	vc-initial-comment	If non-nil, require user to enter a change
				comment upon first checkin of the file.

	vc-keep-workfiles	Non-nil value prevents workfiles from being
				deleted when changes are checked in

        vc-suppress-confirm     Suppresses some confirmation prompts,
				notably for reversions.

	vc-header-alist		Which keywords to insert when adding headers
				with \\[vc-insert-headers].  Defaults to
				'(\"\%\W\%\") under SCCS, '(\"\$Id\$\") under 
				RCS and CVS.

	vc-static-header-alist	By default, version headers inserted in C files
				get stuffed in a static string area so that
				ident(RCS/CVS) or what(SCCS) can see them in
				the compiled object code.  You can override
				this by setting this variable to nil, or change
				the header template by changing it.

	vc-command-messages	if non-nil, display run messages from the
				actual version-control utilities (this is
				intended primarily for people hacking vc
				itself).
"
  (interactive)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map vc-log-entry-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'vc-log-mode)
  (setq mode-name "VC-Log")
  (make-local-variable 'vc-log-file)
  (make-local-variable 'vc-log-version)
  (make-local-variable 'vc-comment-ring-index)
  (set-buffer-modified-p nil)
  (setq buffer-file-name nil)
  (run-hooks 'text-mode-hook 'vc-log-mode-hook)
)

;; Initialization code, to be done just once at load-time
(if vc-log-entry-mode
    nil
  (setq vc-log-entry-mode (make-sparse-keymap))
  (define-key vc-log-entry-mode "\M-n" 'vc-next-comment)
  (define-key vc-log-entry-mode "\M-p" 'vc-previous-comment)
  (define-key vc-log-entry-mode "\M-r" 'vc-comment-search-reverse)
  (define-key vc-log-entry-mode "\M-s" 'vc-comment-search-forward)
  (define-key vc-log-entry-mode "\C-c\C-c" 'vc-finish-logentry)
  )

;;; These things should probably be generally available

(defun vc-file-tree-walk (dirname func &rest args)
  "Walk recursively through DIRNAME.
Invoke FUNC f ARGS on each non-directory file f underneath it."
  (vc-file-tree-walk-internal (expand-file-name dirname) func args)
  (message "Traversing directory %s...done" dirname))

(defun vc-file-tree-walk-internal (file func args)
  (if (not (file-directory-p file))
      (apply func file args)
    (message "Traversing directory %s..." (abbreviate-file-name file))
    (let ((dir (file-name-as-directory file)))
      (mapcar
       (function
	(lambda (f) (or
		     (string-equal f ".")
		     (string-equal f "..")
		     (member f vc-directory-exclusion-list)
		     (let ((dirf (concat dir f)))
			(or
			 (file-symlink-p dirf) ;; Avoid possible loops
			 (vc-file-tree-walk-internal dirf func args))))))
       (directory-files dir)))))

(provide 'vc)

;;; DEVELOPER'S NOTES ON CONCURRENCY PROBLEMS IN THIS CODE
;;;
;;; These may be useful to anyone who has to debug or extend the package.
;;; (Note that this information corresponds to versions 5.x. Some of it
;;; might have been invalidated by the additions to support branching
;;; and RCS keyword lookup. AS, 1995/03/24)
;;; 
;;; A fundamental problem in VC is that there are time windows between
;;; vc-next-action's computations of the file's version-control state and
;;; the actions that change it.  This is a window open to lossage in a
;;; multi-user environment; someone else could nip in and change the state
;;; of the master during it.
;;; 
;;; The performance problem is that rlog/prs calls are very expensive; we want
;;; to avoid them as much as possible.
;;; 
;;; ANALYSIS:
;;; 
;;; The performance problem, it turns out, simplifies in practice to the
;;; problem of making vc-locking-user fast.  The two other functions that call
;;; prs/rlog will not be so commonly used that the slowdown is a problem; one
;;; makes snapshots, the other deletes the calling user's last change in the
;;; master.
;;; 
;;; The race condition implies that we have to either (a) lock the master
;;; during the entire execution of vc-next-action, or (b) detect and
;;; recover from errors resulting from dispatch on an out-of-date state.
;;; 
;;; Alternative (a) appears to be infeasible.  The problem is that we can't
;;; guarantee that the lock will ever be removed.  Suppose a user starts a
;;; checkin, the change message buffer pops up, and the user, having wandered
;;; off to do something else, simply forgets about it?
;;; 
;;; Alternative (b), on the other hand, works well with a cheap way to speed up
;;; vc-locking-user.  Usually, if a file is registered, we can read its locked/
;;; unlocked state and its current owner from its permissions.
;;; 
;;; This shortcut will fail if someone has manually changed the workfile's
;;; permissions; also if developers are munging the workfile in several
;;; directories, with symlinks to a master (in this latter case, the
;;; permissions shortcut will fail to detect a lock asserted from another
;;; directory).
;;; 
;;; Note that these cases correspond exactly to the errors which could happen
;;; because of a competing checkin/checkout race in between two instances of
;;; vc-next-action.
;;; 
;;; For VC's purposes, a workfile/master pair may have the following states:
;;; 
;;; A. Unregistered.  There is a workfile, there is no master.
;;; 
;;; B. Registered and not locked by anyone.
;;; 
;;; C. Locked by calling user and unchanged.
;;; 
;;; D. Locked by the calling user and changed.
;;; 
;;; E. Locked by someone other than the calling user.
;;; 
;;; This makes for 25 states and 20 error conditions.  Here's the matrix:
;;; 
;;; VC's idea of state
;;;  |
;;;  V  Actual state   RCS action              SCCS action          Effect
;;;    A  B  C  D  E
;;;  A .  1  2  3  4   ci -u -t-          admin -fb -i<file>      initial admin
;;;  B 5  .  6  7  8   co -l              get -e                  checkout
;;;  C 9  10 .  11 12  co -u              unget; get              revert
;;;  D 13 14 15 .  16  ci -u -m<comment>  delta -y<comment>; get  checkin
;;;  E 17 18 19 20 .   rcs -u -M -l       unget -n ; get -g       steal lock
;;; 
;;; All commands take the master file name as a last argument (not shown).
;;; 
;;; In the discussion below, a "self-race" is a pathological situation in
;;; which VC operations are being attempted simultaneously by two or more
;;; Emacsen running under the same username.
;;; 
;;; The vc-next-action code has the following windows:
;;; 
;;; Window P:
;;;    Between the check for existence of a master file and the call to
;;; admin/checkin in vc-buffer-admin (apparent state A).  This window may
;;; never close if the initial-comment feature is on.
;;; 
;;; Window Q:
;;;    Between the call to vc-workfile-unchanged-p in and the immediately
;;; following revert (apparent state C).
;;; 
;;; Window R:
;;;    Between the call to vc-workfile-unchanged-p in and the following
;;; checkin (apparent state D).  This window may never close.
;;; 
;;; Window S:
;;;    Between the unlock and the immediately following checkout during a
;;; revert operation (apparent state C).  Included in window Q.
;;; 
;;; Window T:
;;;    Between vc-locking-user and the following checkout (apparent state B).
;;; 
;;; Window U:
;;;    Between vc-locking-user and the following revert (apparent state C).
;;; Includes windows Q and S.
;;; 
;;; Window V:
;;;    Between vc-locking-user and the following checkin (apparent state
;;; D).  This window may never be closed if the user fails to complete the
;;; checkin message.  Includes window R.
;;; 
;;; Window W:
;;;    Between vc-locking-user and the following steal-lock (apparent
;;; state E).  This window may never close if the user fails to complete
;;; the steal-lock message.  Includes window X.
;;; 
;;; Window X:
;;;    Between the unlock and the immediately following re-lock during a
;;; steal-lock operation (apparent state E).  This window may never cloce
;;; if the user fails to complete the steal-lock message.
;;; 
;;; Errors:
;;; 
;;; Apparent state A ---
;;;
;;; 1. File looked unregistered but is actually registered and not locked.
;;; 
;;;    Potential cause: someone else's admin during window P, with
;;; caller's admin happening before their checkout.
;;; 
;;;    RCS: Prior to version 5.6.4, ci fails with message
;;;         "no lock set by <user>".  From 5.6.4 onwards, VC uses the new
;;;         ci -i option and the message is "<file>,v: already exists".
;;;    SCCS: admin will fail with error (ad19).
;;; 
;;;    We can let these errors be passed up to the user.
;;; 
;;; 2. File looked unregistered but is actually locked by caller, unchanged.
;;; 
;;;    Potential cause: self-race during window P.
;;; 
;;;    RCS: Prior to version 5.6.4, reverts the file to the last saved
;;;         version and unlocks it.  From 5.6.4 onwards, VC uses the new
;;;         ci -i option, failing with message "<file>,v: already exists".
;;;    SCCS: will fail with error (ad19).
;;; 
;;;    Either of these consequences is acceptable.
;;; 
;;; 3. File looked unregistered but is actually locked by caller, changed.
;;; 
;;;    Potential cause: self-race during window P.
;;; 
;;;    RCS: Prior to version 5.6.4, VC registers the caller's workfile as 
;;;         a delta with a null change comment (the -t- switch will be 
;;;         ignored). From 5.6.4 onwards, VC uses the new ci -i option,
;;;         failing with message "<file>,v: already exists".
;;;    SCCS: will fail with error (ad19).
;;; 
;;; 4. File looked unregistered but is locked by someone else.
;;; 
;;;    Potential cause: someone else's admin during window P, with
;;; caller's admin happening *after* their checkout.
;;; 
;;;    RCS: Prior to version 5.6.4, ci fails with a 
;;;         "no lock set by <user>" message.  From 5.6.4 onwards, 
;;;         VC uses the new ci -i option, failing with message 
;;;         "<file>,v: already exists".
;;;    SCCS: will fail with error (ad19).
;;; 
;;;    We can let these errors be passed up to the user.
;;; 
;;; Apparent state B ---
;;;
;;; 5. File looked registered and not locked, but is actually unregistered.
;;; 
;;;    Potential cause: master file got nuked during window P.
;;; 
;;;    RCS: will fail with "RCS/<file>: No such file or directory"
;;;    SCCS: will fail with error ut4.
;;; 
;;;    We can let these errors be passed up to the user.
;;; 
;;; 6. File looked registered and not locked, but is actually locked by the
;;; calling user and unchanged.
;;; 
;;;    Potential cause: self-race during window T.
;;; 
;;;    RCS: in the same directory as the previous workfile, co -l will fail
;;; with "co error: writable foo exists; checkout aborted".  In any other
;;; directory, checkout will succeed.
;;;    SCCS: will fail with ge17.
;;; 
;;;    Either of these consequences is acceptable.
;;; 
;;; 7. File looked registered and not locked, but is actually locked by the
;;; calling user and changed.
;;; 
;;;    As case 6.
;;; 
;;; 8. File looked registered and not locked, but is actually locked by another
;;; user.
;;; 
;;;    Potential cause: someone else checks it out during window T.
;;; 
;;;    RCS: co error: revision 1.3 already locked by <user>
;;;    SCCS: fails with ge4 (in directory) or ut7 (outside it).
;;; 
;;;    We can let these errors be passed up to the user.
;;; 
;;; Apparent state C ---
;;;
;;; 9. File looks locked by calling user and unchanged, but is unregistered.
;;; 
;;;    As case 5.
;;; 
;;; 10. File looks locked by calling user and unchanged, but is actually not
;;; locked.
;;; 
;;;    Potential cause: a self-race in window U, or by the revert's
;;; landing during window X of some other user's steal-lock or window S
;;; of another user's revert.
;;; 
;;;    RCS: succeeds, refreshing the file from the identical version in
;;; the master.
;;;    SCCS: fails with error ut4 (p file nonexistent).
;;;
;;;    Either of these consequences is acceptable.
;;; 
;;; 11. File is locked by calling user.  It looks unchanged, but is actually
;;; changed.
;;; 
;;;    Potential cause: the file would have to be touched by a self-race
;;; during window Q.
;;; 
;;;    The revert will succeed, removing whatever changes came with
;;; the touch.  It is theoretically possible that work could be lost.
;;; 
;;; 12. File looks like it's locked by the calling user and unchanged, but
;;; it's actually locked by someone else.
;;; 
;;;    Potential cause: a steal-lock in window V.
;;; 
;;;    RCS: co error: revision <rev> locked by <user>; use co -r or rcs -u
;;;    SCCS: fails with error un2
;;; 
;;;    We can pass these errors up to the user.
;;; 
;;; Apparent state D ---
;;;
;;; 13. File looks like it's locked by the calling user and changed, but it's
;;; actually unregistered.
;;; 
;;;    Potential cause: master file got nuked during window P.
;;; 
;;;    RCS: Prior to version 5.6.4, checks in the user's version as an 
;;;         initial delta.  From 5.6.4 onwards, VC uses the new ci -j
;;;         option, failing with message "no such file or directory".
;;;    SCCS: will fail with error ut4.
;;;
;;;    This case is kind of nasty.  Under RCS prior to version 5.6.4,
;;; VC may fail to detect the loss of previous version information.
;;; 
;;; 14. File looks like it's locked by the calling user and changed, but it's
;;; actually unlocked.
;;; 
;;;    Potential cause: self-race in window V, or the checkin happening
;;; during the window X of someone else's steal-lock or window S of
;;; someone else's revert.
;;; 
;;;    RCS: ci will fail with "no lock set by <user>".
;;;    SCCS: delta will fail with error ut4.
;;; 
;;; 15. File looks like it's locked by the calling user and changed, but it's
;;; actually locked by the calling user and unchanged.
;;; 
;;;    Potential cause: another self-race --- a whole checkin/checkout
;;; sequence by the calling user would have to land in window R.
;;; 
;;;    SCCS: checks in a redundant delta and leaves the file unlocked as usual.
;;;    RCS: reverts to the file state as of the second user's checkin, leaving
;;; the file unlocked.
;;;
;;;    It is theoretically possible that work could be lost under RCS.
;;; 
;;; 16. File looks like it's locked by the calling user and changed, but it's
;;; actually locked by a different user.
;;; 
;;;    RCS: ci error: no lock set by <user>
;;;    SCCS: unget will fail with error un2
;;; 
;;;    We can pass these errors up to the user.
;;; 
;;; Apparent state E ---
;;;
;;; 17. File looks like it's locked by some other user, but it's actually
;;; unregistered.
;;; 
;;;    As case 13.
;;; 
;;; 18. File looks like it's locked by some other user, but it's actually
;;; unlocked.
;;; 
;;;    Potential cause: someone released a lock during window W.
;;; 
;;;    RCS: The calling user will get the lock on the file.
;;;    SCCS: unget -n will fail with cm4.
;;; 
;;;    Either of these consequences will be OK.
;;; 
;;; 19. File looks like it's locked by some other user, but it's actually
;;; locked by the calling user and unchanged.
;;; 
;;;    Potential cause: the other user relinquishing a lock followed by
;;; a self-race, both in window W.
;;; 
;;;     Under both RCS and SCCS, both unlock and lock will succeed, making
;;; the sequence a no-op.
;;; 
;;; 20. File looks like it's locked by some other user, but it's actually
;;; locked by the calling user and changed.
;;; 
;;;     As case 19.
;;; 
;;; PROBLEM CASES:
;;; 
;;;    In order of decreasing severity:
;;; 
;;;    Cases 11 and 15 are the only ones that potentially lose work.
;;; They would require a self-race for this to happen.
;;; 
;;;    Case 13 in RCS loses information about previous deltas, retaining
;;; only the information in the current workfile.  This can only happen
;;; if the master file gets nuked in window P.
;;; 
;;;    Case 3 in RCS and case 15 under SCCS insert a redundant delta with
;;; no change comment in the master.  This would require a self-race in
;;; window P or R respectively.
;;; 
;;;    Cases 2, 10, 19 and 20 do extra work, but make no changes.
;;; 
;;;    Unfortunately, it appears to me that no recovery is possible in these
;;; cases.  They don't yield error messages, so there's no way to tell that
;;; a race condition has occurred.
;;; 
;;;    All other cases don't change either the workfile or the master, and
;;; trigger command errors which the user will see.
;;; 
;;;    Thus, there is no explicit recovery code.

;;; vc.el ends here
