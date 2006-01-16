;;; tramp-vc.el --- Version control integration for TRAMP.el

;; Copyright (C) 2000, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Daniel Pittman <daniel@danann.net>
;; Keywords: comm, processes

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

;; See the main module, 'tramp.el' for discussion of the purpose of TRAMP.
;; This module provides integration between remote files accessed by TRAMP and
;; the Emacs version control system.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'vc)
;; Old VC defines vc-rcs-release in vc.el, new VC requires extra module.
(unless (boundp 'vc-rcs-release)
  (require 'vc-rcs))
(require 'tramp)

;; Avoid byte-compiler warnings if the byte-compiler supports this.
;; Currently, XEmacs supports this.
(eval-when-compile
  (when (fboundp 'byte-compiler-options)
    (let (unused-vars) ; Pacify Emacs byte-compiler
      (defalias 'warnings 'identity) ; Pacify Emacs byte-compiler
      (byte-compiler-options (warnings (- unused-vars))))))

;; -- vc --

;; This used to blow away the file-name-handler-alist and reinstall
;; TRAMP into it. This was intended to let VC work remotely. It didn't,
;; at least not in my XEmacs 21.2 install.
;;
;; In any case, tramp-run-real-handler now deals correctly with disabling
;; the things that should be, making this a no-op.
;;
;; I have removed it from the tramp-file-name-handler-alist because the
;; shortened version does nothing. This is for reference only now.
;;
;; Daniel Pittman <daniel@danann.net>
;;
;; (defun tramp-handle-vc-registered (file)
;;   "Like `vc-registered' for tramp files."
;;   (tramp-run-real-handler 'vc-registered (list file)))

;; `vc-do-command'
;; This function does not deal well with remote files, so we define
;; our own version and make a backup of the original function and
;; call our version for tramp files and the original version for
;; normal files.

;; The following function is pretty much copied from vc.el, but
;; the part that actually executes a command is changed.
;; CCC: this probably works for Emacs 21, too.
(defun tramp-vc-do-command (buffer okstatus command file last &rest flags)
  "Like `vc-do-command' but invoked for tramp files.
See `vc-do-command' for more information."
  (save-match-data
    (and file (setq file (expand-file-name file)))
    (if (not buffer) (setq buffer "*vc*"))
    (if vc-command-messages
	(message "Running `%s' on `%s'..." command file))
    (let ((obuf (current-buffer)) (camefrom (current-buffer))
	  (squeezed nil)
	  (olddir default-directory)
	  vc-file status)
      (let* ((v (tramp-dissect-file-name (expand-file-name file)))
	     (multi-method (tramp-file-name-multi-method v))
	     (method (tramp-file-name-method v))
	     (user (tramp-file-name-user v))
	     (host (tramp-file-name-host v))
	     (localname (tramp-file-name-localname v)))
	(set-buffer (get-buffer-create buffer))
	(set (make-local-variable 'vc-parent-buffer) camefrom)
	(set (make-local-variable 'vc-parent-buffer-name)
	     (concat " from " (buffer-name camefrom)))
	(setq default-directory olddir)

	(erase-buffer)

	(mapcar
	 (function
	  (lambda (s) (and s (setq squeezed (append squeezed (list s))))))
	 flags)
	(if (and (eq last 'MASTER) file
		 (setq vc-file (vc-name file)))
	    (setq squeezed
		  (append squeezed
			  (list (tramp-file-name-localname
				 (tramp-dissect-file-name vc-file))))))
	(if (and file (eq last 'WORKFILE))
	    (progn
	      (let* ((pwd (expand-file-name default-directory))
		     (preflen (length pwd)))
		(if (string= (substring file 0 preflen) pwd)
		    (setq file (substring file preflen))))
	      (setq squeezed (append squeezed (list file)))))
	;; Unless we (save-window-excursion) the layout of windows in
	;; the current frame changes. This is painful, at best.
	;;
	;; As a point of note, (save-excursion) is still here only because
	;; it preserves (point) in the current buffer. (save-window-excursion)
	;; does not, at least under XEmacs 21.2.
	;;
	;; I trust that the FSF support this as well. I can't find useful
	;; documentation to check :(
	;;
	;; Daniel Pittman <daniel@danann.net>
	(save-excursion
	  (save-window-excursion
	    ;; Actually execute remote command
	    ;; `shell-command' cannot be used; it isn't magic in XEmacs.
	    (tramp-handle-shell-command
	     (mapconcat 'tramp-shell-quote-argument
			(cons command squeezed) " ") t)
	    ;;(tramp-wait-for-output)
	    ;; Get status from command
	    (tramp-send-command multi-method method user host "echo $?")
	    (tramp-wait-for-output)
	    ;; Make sure to get status from last line of output.
	    (goto-char (point-max)) (forward-line -1)
	    (setq status (read (current-buffer)))
	    (message "Command %s returned status %d." command status)))
	(goto-char (point-max))
	(set-buffer-modified-p nil)
	(forward-line -1)
	(if (or (not (integerp status))
		(and (integerp okstatus) (< okstatus status)))
	    (progn
	      (pop-to-buffer buffer)
	      (goto-char (point-min))
	      (shrink-window-if-larger-than-buffer)
	      (error "Running `%s'...FAILED (%s)" command
		     (if (integerp status)
			 (format "status %d" status)
		       status))
	      )
	  (if vc-command-messages
	      (message "Running %s...OK" command))
	  )
	(set-buffer obuf)
	status))
    ))

;; Following code snarfed from Emacs 21 vc.el and slightly tweaked.
(defun tramp-vc-do-command-new (buffer okstatus command file &rest flags)
  "Like `vc-do-command' but for TRAMP files.
This function is for the new VC which comes with Emacs 21.
Since TRAMP doesn't do async commands yet, this function doesn't, either."
  (and file (setq file (expand-file-name file)))
  (if vc-command-messages
      (message "Running %s on %s..." command file))
  (save-current-buffer
    (unless (eq buffer t)
      ; Pacify byte-compiler
      (funcall (symbol-function 'vc-setup-buffer) buffer))
    (let ((squeezed nil)
	  (inhibit-read-only t)
	  (status 0))
      (let* ((v (when file (tramp-dissect-file-name file)))
             (multi-method (when file (tramp-file-name-multi-method v)))
             (method (when file (tramp-file-name-method v)))
             (user (when file (tramp-file-name-user v)))
             (host (when file (tramp-file-name-host v)))
             (localname (when file (tramp-file-name-localname v))))
      (setq squeezed (delq nil (copy-sequence flags)))
      (when file
	(setq squeezed (append squeezed (list (file-relative-name
					       file default-directory)))))
      (let ((w32-quote-process-args t))
        (when (eq okstatus 'async)
          (message "Tramp doesn't do async commands, running synchronously."))
	;; `shell-command' cannot be used; it isn't magic in XEmacs.
        (setq status (tramp-handle-shell-command
                      (mapconcat 'tramp-shell-quote-argument
                                 (cons command squeezed) " ") t))
        (when (or (not (integerp status))
		  (and (integerp okstatus) (< okstatus status)))
          (pop-to-buffer (current-buffer))
          (goto-char (point-min))
          (shrink-window-if-larger-than-buffer)
          (error "Running %s...FAILED (%s)" command
                 (if (integerp status) (format "status %d" status) status))))
      (if vc-command-messages
          (message "Running %s...OK" command))
      ; Pacify byte-compiler
      (funcall (symbol-function 'vc-exec-after)
	       `(run-hook-with-args
		 'vc-post-command-functions ',command ',localname ',flags))
      status))))


;; The context for a VC command is the current buffer.
;; That makes a test on the buffers file more reliable than a test on the
;; arguments.
;; This is needed to handle remote VC correctly - else we test against the
;; local VC system and get things wrong...
;; Daniel Pittman <daniel@danann.net>
;;-(if (fboundp 'vc-call-backend)
;;-    () ;; This is the new VC for which we don't have an appropriate advice yet
(unless (fboundp 'process-file)
(if (fboundp 'vc-call-backend)
    (defadvice vc-do-command
      (around tramp-advice-vc-do-command
              (buffer okstatus command file &rest flags)
              activate)
      "Invoke tramp-vc-do-command for tramp files."
      (let ((file (symbol-value 'file)))    ;pacify byte-compiler
        (if (or (and (stringp file)     (tramp-tramp-file-p file))
                (and (buffer-file-name) (tramp-tramp-file-p (buffer-file-name))))
            (setq ad-return-value
                  (apply 'tramp-vc-do-command-new buffer okstatus command
                         file ;(or file (buffer-file-name))
                         flags))
          ad-do-it)))
  (defadvice vc-do-command
    (around tramp-advice-vc-do-command
            (buffer okstatus command file last &rest flags)
            activate)
    "Invoke tramp-vc-do-command for tramp files."
    (let ((file (symbol-value 'file)))  ;pacify byte-compiler
      (if (or (and (stringp file)     (tramp-tramp-file-p file))
              (and (buffer-file-name) (tramp-tramp-file-p (buffer-file-name))))
          (setq ad-return-value
                (apply 'tramp-vc-do-command buffer okstatus command
                       (or file (buffer-file-name)) last flags))
        ad-do-it)))))
;;-)


;; XEmacs uses this to do some of its work. Like vc-do-command, we
;; need to enhance it to make VC work via TRAMP-mode.
;;
;; Like the previous function, this is a cut-and-paste job from the VC
;; file. It's based on the vc-do-command code.
;; CCC: this isn't used in Emacs 21, so do as before.
(defun tramp-vc-simple-command (okstatus command file &rest args)
  ;; Simple version of vc-do-command, for use in vc-hooks only.
  ;; Don't switch to the *vc-info* buffer before running the
  ;; command, because that would change its default directory
  (save-match-data
    (let* ((v (tramp-dissect-file-name (expand-file-name file)))
	   (multi-method (tramp-file-name-multi-method v))
	   (method (tramp-file-name-method v))
	   (user (tramp-file-name-user v))
	   (host (tramp-file-name-host v))
	   (localname (tramp-file-name-localname v)))
      (save-excursion (set-buffer (get-buffer-create "*vc-info*"))
		      (erase-buffer))
      (let ((exec-path (append vc-path exec-path)) exec-status
	    ;; Add vc-path to PATH for the execution of this command.
	    (process-environment
	     (cons (concat "PATH=" (getenv "PATH")
			   path-separator
			   (mapconcat 'identity vc-path path-separator))
		   process-environment)))
	;; Call the actual process. See tramp-vc-do-command for discussion of
	;; why this does both (save-window-excursion) and (save-excursion).
	;;
	;; As a note, I don't think that the process-environment stuff above
	;; has any effect on the remote system. This is a hard one though as
	;; there is no real reason to expect local and remote paths to be
	;; identical...
	;;
	;; Daniel Pittman <daniel@danann.net>
	(save-excursion
	  (save-window-excursion
	    ;; Actually execute remote command
	    ;; `shell-command' cannot be used; it isn't magic in XEmacs.
	    (tramp-handle-shell-command
	     (mapconcat 'tramp-shell-quote-argument
			(append (list command) args (list localname)) " ")
	     (get-buffer-create"*vc-info*"))
					;(tramp-wait-for-output)
	    ;; Get status from command
	    (tramp-send-command multi-method method user host "echo $?")
	    (tramp-wait-for-output)
	    (setq exec-status (read (current-buffer)))
	    (message "Command %s returned status %d." command exec-status)))

	;; Maybe okstatus can be `async' here.  But then, maybe the
	;; async thing is new in Emacs 21, but this function is only
	;; used in Emacs 20.
	(cond ((> exec-status okstatus)
	       (switch-to-buffer (get-file-buffer file))
	       (shrink-window-if-larger-than-buffer
		(display-buffer "*vc-info*"))
	       (error "Couldn't find version control information")))
	exec-status))))

;; This function does not exist any more in Emacs-21's VC
(defadvice vc-simple-command
  (around tramp-advice-vc-simple-command
	  (okstatus command file &rest args)
	  activate)
  "Invoke tramp-vc-simple-command for tramp files."
  (let ((file (symbol-value 'file)))    ;pacify byte-compiler
    (if (or (and (stringp file)     (tramp-tramp-file-p file))
            (and (buffer-file-name) (tramp-tramp-file-p (buffer-file-name))))
        (setq ad-return-value
              (apply 'tramp-vc-simple-command okstatus command
                     (or file (buffer-file-name)) args))
      ad-do-it)))


;; `vc-workfile-unchanged-p'
;; This function does not deal well with remote files, so we do the
;; same as for `vc-do-command'.

;; `vc-workfile-unchanged-p' checks the modification time, we cannot
;; do that for remote files, so here's a version which relies on diff.
;; CCC: this one probably works for Emacs 21, too.
(defun tramp-vc-workfile-unchanged-p
  (filename &optional want-differences-if-changed)
  (if (fboundp 'vc-backend-diff)
      ;; Old VC.  Call `vc-backend-diff'.
      (let ((status (funcall (symbol-function 'vc-backend-diff)
                             filename nil nil
                             (not want-differences-if-changed))))
        (zerop status))
    ;; New VC.  Call `vc-default-workfile-unchanged-p'.
      (funcall (symbol-function 'vc-default-workfile-unchanged-p)
	       (vc-backend filename) filename)))

(defadvice vc-workfile-unchanged-p
  (around tramp-advice-vc-workfile-unchanged-p
          (filename &optional want-differences-if-changed)
          activate)
  "Invoke tramp-vc-workfile-unchanged-p for tramp files."
  (if (and (stringp filename)
	   (tramp-tramp-file-p filename)
	   (not
	    (let ((v	(tramp-dissect-file-name filename)))
	      ;; The following check is probably to test whether
	      ;; file-attributes returns correct last modification
	      ;; times.  This check needs to be changed.
	      (tramp-get-remote-perl (tramp-file-name-multi-method v)
				   (tramp-file-name-method v)
				   (tramp-file-name-user v)
				   (tramp-file-name-host v)))))
      (setq ad-return-value
            (tramp-vc-workfile-unchanged-p filename want-differences-if-changed))
    ad-do-it))


;; Redefine a function from vc.el -- allow tramp files.
;; `save-match-data' seems not to be required -- it isn't in
;; the original version, either.
;; CCC: this might need some work -- how does the Emacs 21 version
;; work, anyway?  Does it work over ange-ftp?  Hm.
(if (not (fboundp 'vc-backend-checkout))
    () ;; our replacement won't work and is unnecessary anyway
(defun vc-checkout (filename &optional writable rev)
  "Retrieve a copy of the latest version of the given file."
  ;; If ftp is on this system and the name matches the ange-ftp format
  ;; for a remote file, the user is trying something that won't work.
  (funcall (symbol-function 'vc-backend-checkout) filename writable rev)
  (vc-resynch-buffer filename t t))
)


;; Do we need to advise the vc-user-login-name function anyway?
;; This will return the correct login name for the owner of a
;; file. It does not deal with the default remote user name...
;;
;; That is, when vc calls (vc-user-login-name), we return the
;; local login name, something that may be different to the remote
;; default.
;;
;; The remote VC operations will occur as the user that we logged
;; in with however - not always the same as the local user.
;;
;; In the end, I did advise the function. This is because, well,
;; the thing didn't work right otherwise ;)
;;
;; Daniel Pittman <daniel@danann.net>

(defun tramp-handle-vc-user-login-name (&optional uid)
  "Return the default user name on the remote machine.
Whenever VC calls this function, `file' is bound to the file name
in question.  If no uid is provided or the uid is equal to the uid
owning the file, then we return the user name given in the file name.

This should only be called when `file' is bound to the
filename we are thinking about..."
  ;; Pacify byte-compiler; this symbol is bound in the calling
  ;; function.  CCC: Maybe it would be better to move the
  ;; boundness-checking into this function?
  (let* ((file (symbol-value 'file))
	 (remote-uid
	  ;; With Emacs 22, `file-attributes' has got an optional parameter
	  ;; ID-FORMAT. Handle this case backwards compatible.
	  (if (and (functionp 'subr-arity)
		   (= 2 (cdr (funcall (symbol-function 'subr-arity)
				      (symbol-function 'file-attributes)))))
	      (nth 2 (file-attributes file 'integer))
	    (nth 2 (file-attributes file)))))
    (if (and uid (/= uid remote-uid))
	(error "tramp-handle-vc-user-login-name cannot map a uid to a name")
      (let* ((v (tramp-dissect-file-name (expand-file-name file)))
	     (u (tramp-file-name-user v)))
	(cond ((stringp u) u)
	      ((vectorp u) (elt u (1- (length u))))
	      ((null    u) (user-login-name))
	      (t	   (error "tramp-handle-vc-user-login-name cannot cope!")))))))


(defadvice vc-user-login-name
  (around tramp-vc-user-login-name activate)
  "Support for files on remote machines accessed by TRAMP."
  ;; We rely on the fact that `file' is bound when this is called.
  ;; This appears to be the case everywhere in vc.el and vc-hooks.el
  ;; as of Emacs 20.5.
  ;;
  ;; CCC TODO there should be a real solution!  Talk to Andre Spiegel
  ;; about this.
  (let ((file (when (boundp 'file)
                (symbol-value 'file))))    ;pacify byte-compiler
    (or (and (stringp file)
             (tramp-tramp-file-p file)	; tramp file
             (setq ad-return-value
		   (save-match-data
		     (tramp-handle-vc-user-login-name uid)))) ; get the owner name
        ad-do-it)))                     ; else call the original


;; Determine the name of the user owning a file.
(defun tramp-file-owner (filename)
  "Return who owns FILE (user name, as a string)."
  (let ((v (tramp-dissect-file-name
	    (expand-file-name filename))))
    (if (not (file-exists-p filename))
        nil                             ; file cannot be opened
      ;; file exists, find out stuff
      (save-excursion
        (tramp-send-command
         (tramp-file-name-multi-method v) (tramp-file-name-method v)
         (tramp-file-name-user v) (tramp-file-name-host v)
         (format "%s -Lld %s"
                 (tramp-get-ls-command (tramp-file-name-multi-method v)
                                     (tramp-file-name-method v)
                                     (tramp-file-name-user v)
                                     (tramp-file-name-host v))
                 (tramp-shell-quote-argument (tramp-file-name-localname v))))
        (tramp-wait-for-output)
        ;; parse `ls -l' output ...
        ;; ... file mode flags
        (read (current-buffer))
        ;; ... number links
        (read (current-buffer))
        ;; ... uid (as a string)
        (symbol-name (read (current-buffer)))))))

;; Wire ourselves into the VC infrastructure...
;; This function does not exist any more in Emacs-21's VC
;; CCC: it appears that no substitute is needed for Emacs 21.
(defadvice vc-file-owner
  (around tramp-vc-file-owner activate)
  "Support for files on remote machines accessed by TRAMP."
  (let ((filename (ad-get-arg 0)))
    (or (and (tramp-file-name-p filename) ; tramp file
             (setq ad-return-value
		   (save-match-data
		     (tramp-file-owner filename)))) ; get the owner name
        ad-do-it)))                     ; else call the original


;; We need to make the version control software backend version
;; information local to the current buffer. This is because each TRAMP
;; buffer can (theoretically) have a different VC version and I am
;; *way* too lazy to try and push the correct value into each new
;; buffer.
;;
;; Remote VC costs will just have to be paid, at least for the moment.
;; Well, at least, they will right until I feel guilty about doing a
;; botch job here and fix it. :/
;;
;; Daniel Pittman <daniel@danann.net>
;; CCC: this is probably still needed for Emacs 21.
(defun tramp-vc-setup-for-remote ()
  "Make the backend release variables buffer local.
This makes remote VC work correctly at the cost of some processing time."
  (when (and (buffer-file-name)
             (tramp-tramp-file-p (buffer-file-name)))
    (make-local-variable 'vc-rcs-release)
    (setq vc-rcs-release nil)))
(add-hook 'find-file-hooks 'tramp-vc-setup-for-remote t)

;; No need to load this again if anyone asks.
(provide 'tramp-vc)

;;; arch-tag: 27cc42ce-da19-468d-ad5c-a2690558db60
;;; tramp-vc.el ends here
