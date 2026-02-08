;;; savehist.el --- Save minibuffer history  -*- lexical-binding:t -*-

;; Copyright (C) 1997-2026 Free Software Foundation, Inc.

;; Author: Hrvoje Nikšić <hrvoje.niksic@avl.com>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: convenience, minibuffer
;; Old-Version: 24

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

;; Many editors (e.g. Vim) have the feature of saving minibuffer
;; history to an external file after exit.  This package provides the
;; same feature in Emacs.  When set up, it saves recorded minibuffer
;; histories to a file (`~/.emacs.d/history' by default).  Additional
;; variables may be specified by customizing
;; `savehist-additional-variables'.

;; To use savehist, turn on savehist-mode by putting the following in
;; `~/.emacs':
;;
;;     (savehist-mode 1)
;;
;; or with customize: `M-x customize-option RET savehist-mode RET'.
;;
;; You can also explicitly save history with `M-x savehist-save' and
;; load it by loading the `savehist-file' with `M-x load-file'.

;;; Code:

;; User variables

(defgroup savehist nil
  "Save minibuffer history."
  :version "22.1"
  :group 'minibuffer)

(defcustom savehist-save-minibuffer-history t
  "If non-nil, save all recorded minibuffer histories.
If you want to save only specific histories, use `savehist-save-hook'
to modify the value of `savehist-minibuffer-history-variables'."
  :type 'boolean)

(defcustom savehist-additional-variables nil
  "List of additional variables to save.
Each element is a variable that will be persisted across Emacs
sessions that use Savehist.

An element may be variable name (a symbol) or a cons cell of the form
\(VAR . MAX-SIZE), which means to truncate VAR's value to at most
MAX-SIZE elements (if the value is a list) before saving the value.

The contents of variables should be printable with the Lisp
printer.  You don't need to add minibuffer history variables to
this list, all minibuffer histories will be saved automatically
as long as `savehist-save-minibuffer-history' is non-nil.

User options should be saved with the Customize interface.  This
list is useful for saving automatically updated variables that are not
minibuffer histories, such as `compile-command' or `kill-ring'.

NOTE: If this variable is not nil, then Emacs will not try to handle
gracefully concurrent modifications to the `savehist-file', e.g. by other
Emacs sessions or other tools like file-synchronization systems."
  :type '(repeat variable))

(defcustom savehist-ignored-variables nil ;; '(command-history)
  "List of additional variables not to save."
  :type '(repeat variable))

(defcustom savehist-file
  (locate-user-emacs-file "history" ".emacs-history")
  "File name where minibuffer history is saved to and loaded from.
The minibuffer history is a series of Lisp expressions loaded
automatically when Savehist mode is turned on.  See `savehist-mode'
for more details."
  :type 'file)

(defcustom savehist-file-modes #o600
  "Default permissions of the history file.
This is decimal, not octal.  The default is 384 (0600 in octal).
Set to nil to use the default permissions that Emacs uses, typically
mandated by umask.  The default is a bit more restrictive to protect
the user's privacy."
  :type '(choice (natnum :tag "Specify")
                 (const :tag "Use default" :value nil)))

(defvar savehist-timer nil)

(defun savehist--cancel-timer ()
  "Cancel `savehist-autosave' timer, if set."
  (when (timerp savehist-timer)
    (cancel-timer savehist-timer))
  (setq savehist-timer nil))

(defvar savehist-autosave-interval)

(defun savehist--manage-timer ()
  "Set or cancel an invocation of `savehist-autosave' on a timer.
If `savehist-mode' is enabled, set the timer, otherwise cancel the timer.
This should not cause noticeable delays for users -- `savehist-autosave'
executes in under 5 ms on my system."
  (if (and savehist-mode
           savehist-autosave-interval
           (null savehist-timer))
      (setq savehist-timer
            (run-with-timer savehist-autosave-interval
                            savehist-autosave-interval #'savehist-autosave))
    (savehist--cancel-timer)))

(defcustom savehist-autosave-interval (* 5 60)
  "The interval between autosaves of minibuffer history.
If set to nil, disables timer-based autosaving."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :set (lambda (sym val)
         (set-default sym val)
         (savehist--cancel-timer)
         (savehist--manage-timer)))

(defcustom savehist-mode-hook nil
  "Hook called when Savehist mode is turned on."
  :type 'hook)

(defcustom savehist-save-hook nil
  "Hook called by `savehist-save' before saving the variables.
You can use this hook to influence choice and content of variables
to save."
  :type 'hook)

;; This should be capable of representing characters used by Emacs.
;; We prefer UTF-8 over ISO 2022 because it is well-known outside
;; Mule.
(defvar savehist-coding-system 'utf-8-emacs-unix
  "The coding system Savehist uses for saving the minibuffer history.
Changing this value while Emacs is running is supported, but considered
unwise, unless you know what you are doing.")

;; Internal variables.

(defvar savehist-last-checksum nil)

(defvar savehist-minibuffer-history-variables nil
  "List of minibuffer histories.
The contents of this variable is built while Emacs is running, and saved
along with minibuffer history.  You can change its value off
`savehist-save-hook' to influence which variables are saved.")

(defvar savehist-loaded nil
 "Whether the history has already been loaded.")
(make-obsolete-variable 'savehist-loaded 'savehist--file-sync-modtime "31")

(defvar savehist--file-sync-modtime nil
  "Modtime of the `savehist-file' when we last sync'd up with it.")


;; Functions.

;;;###autoload
(define-minor-mode savehist-mode
  "Toggle saving of minibuffer history (Savehist mode).

When Savehist mode is enabled, minibuffer history is saved
to `savehist-file' periodically and when exiting Emacs.  When
Savehist mode is enabled for the first time in an Emacs session,
it loads the previous minibuffer histories from `savehist-file'.
The variable `savehist-autosave-interval' controls the
periodicity of saving minibuffer histories.

If `savehist-save-minibuffer-history' is non-nil (the default),
all recorded minibuffer histories will be saved.  You can arrange
for additional history variables to be saved and restored by
customizing `savehist-additional-variables', which by default is
an empty list.  For example, to save the history of commands
invoked via \\[execute-extended-command], add `command-history' to the list in
`savehist-additional-variables'.

Alternatively, you could customize `savehist-save-minibuffer-history'
to nil, and add to `savehist-additional-variables' only those
history variables you want to save.

To ignore some history variables, add their symbols to the list
in `savehist-ignored-variables'.

This mode should normally be turned on from your Emacs init file.
Calling it at any other time replaces your current minibuffer
histories, which is probably undesirable."
  :global t
  (if (not savehist-mode)
      (savehist-uninstall)
    (savehist--reload (called-interactively-p 'interactive))
    (savehist-install)))

(defun savehist--reload (interactively)
  "Load the history data from `savehist-file'.
Be careful to do it while preserving the current history data."
  (when (and (file-exists-p savehist-file)
             (not (and savehist-loaded
                       (equal savehist--file-sync-modtime
                              (savehist--file-modtime)))))
    ;; FIXME: Process the file manually rather than passing it to `load'.
    (let ((savehist-old-minibuffer-history-variables
           (delq nil (mapcar (lambda (s)
                               (and (boundp s) (cons s (symbol-value s))))
                             (cons 'savehist-minibuffer-history-variables
                                   savehist-minibuffer-history-variables)))))
      (condition-case errvar
	  (progn
	    ;; Don't set coding-system-for-read -- we rely on the
	    ;; coding cookie to convey that information.  That way, if
	    ;; the user changes the value of savehist-coding-system,
	    ;; we can still correctly load the old file.
            (let ((warning-inhibit-types '((files missing-lexbind-cookie))))
	      (load savehist-file nil
                    (not interactively)))
            (setq savehist--file-sync-modtime (savehist--file-modtime))
	    (setq savehist-loaded t))
	(error
	 ;; Don't install the mode if reading failed.  Doing so would
	 ;; effectively destroy the user's data at the next save.
	 (setq savehist-mode nil)
	 (savehist-uninstall)
	 (signal (car errvar) (cdr errvar))))

      ;; In case we're loading the file late, there was info in the history
      ;; variables that may have been overwritten by the info extracted from
      ;; the file, so put it back in.
      (pcase-dolist (`(,s . ,v) savehist-old-minibuffer-history-variables)
        ;; For each histvar that we knew about, make sure all the entries that
        ;; were there before are still here now and in the same order.
        (with-demoted-errors "%S" ;Maybe some var is not a list or something.
          (unless (equal v (symbol-value s))
            (set s (savehist--merge v (symbol-value s)))))))))

(defun savehist--file-modtime ()
  (or (file-attribute-modification-time (file-attributes savehist-file))
      t))

(defun savehist--merge (old new)
  "Merge the OLD history we had with the NEW history we just loaded."
  ;; We don't know the relative age of the various entries in OLD and
  ;; NEW; it's possible that most entries in NEW are much older than
  ;; those in OLD or vice versa, or anything in-between.  Maybe we should
  ;; export the `lib/diffseq.h' to Elisp and use it here, but in the mean
  ;; time we interleave the two lists, which should usually be tolerable.
  (let ((res ()))
    (while (and old new)
      (push (pop old) res) ;; Keep the first element first.
      (push (pop new) res))
    ;; In order to avoid problems when merging repeatedly, we want to be
    ;; idempotent, i.e. (merge (merge OLD NEW) NEW) = (merge OLD NEW).
    (delete-dups (nconc (nreverse res) old new))))

(defun savehist-install ()
  "Hook Savehist into Emacs.
Normally invoked by calling `savehist-mode' to set the minor mode.
Installs `savehist-autosave' in `kill-emacs-hook' and on a timer.
To undo this, call `savehist-uninstall'."
  (add-hook 'minibuffer-setup-hook #'savehist-minibuffer-hook)
  (add-hook 'kill-emacs-hook #'savehist-autosave)
  (savehist--manage-timer))

(defun savehist-uninstall ()
  "Undo installing savehist.
Normally invoked by calling `savehist-mode' to unset the minor mode."
  (remove-hook 'minibuffer-setup-hook #'savehist-minibuffer-hook)
  (remove-hook 'kill-emacs-hook #'savehist-autosave)
  (savehist--manage-timer))

(defvar savehist--has-given-file-warning nil)
(defun savehist-save (&optional auto-save)
  "Save the values of minibuffer history variables.
Unbound symbols referenced in `savehist-additional-variables' are ignored.
If AUTO-SAVE is non-nil, compare the saved contents to the one last saved,
 and don't save the buffer if they are the same."
  (interactive)
  ;; FIXME: We don't know how to merge old and new values of those vars
  ;; in `savehist-additional-variables', so only do the auto-sync
  ;; if there aren't such variables.
  (unless (or savehist-additional-variables
              (equal savehist--file-sync-modtime (savehist--file-modtime)))
    ;; The file has been changed since last time we saw it.
    ;; Probably some other Emacs session.  Load the corresponding info so we
    ;; don't end up throwing it away by blindly overwriting it.  There's
    ;; still a race-condition, but hopefully less problematic.
    (savehist--reload nil))
  (with-temp-buffer
    (insert
     (format-message
      (concat
       ";; -*- mode: emacs-lisp; lexical-binding: t; coding: %s -*-\n"
       ";; Minibuffer history file, automatically generated by `savehist'.\n"
       "\n")
      savehist-coding-system))
    (run-hooks 'savehist-save-hook)
    (let ((print-length nil)
          (print-level nil)
          (print-quoted t)
          (print-circle t))
      ;; Save the minibuffer histories, along with the value of
      ;; savehist-minibuffer-history-variables itself.
      (when savehist-save-minibuffer-history
	(prin1 `(setq savehist-minibuffer-history-variables
		      ',savehist-minibuffer-history-variables)
	       (current-buffer))
	(insert ?\n)
	(dolist (symbol savehist-minibuffer-history-variables)
	  (when (and (boundp symbol)
		     (not (memq symbol savehist-ignored-variables)))
	    (let ((value (symbol-value symbol))
		  excess-space)
	      (when value		; Don't save empty histories.
		(insert "(setq ")
		(prin1 symbol (current-buffer))
		(insert " '(")
		;; We will print an extra space before the first element.
		;; Record where that is.
		(setq excess-space (point))
		;; Print elements of VALUE one by one, carefully.
		(dolist (elt value)
		  (let ((start (point)))
		    (insert " ")
		    ;; Try to print and then to read an element.
		    (condition-case nil
			(progn
			  (prin1 elt (current-buffer))
			  (save-excursion
			    (goto-char start)
			    (read (current-buffer))))
		      (error
		       ;; If writing or reading gave an error, comment it out.
		       (goto-char start)
		       (insert "\n")
		       (while (not (eobp))
			 (insert ";;; ")
			 (forward-line 1))
		       (insert "\n")))
		    (goto-char (point-max))))
		;; Delete the extra space before the first element.
		(save-excursion
		  (goto-char excess-space)
		  (if (eq (following-char) ?\s)
		      (delete-region (point) (1+ (point)))))
		(insert "))\n"))))))
      ;; Save the additional variables.
      (dolist (elem savehist-additional-variables)
        (when (not (memq elem savehist-minibuffer-history-variables))
          (let ((symbol (if (consp elem)
                            (car elem)
                          elem)))
	    (when (boundp symbol)
	      (let ((value (symbol-value symbol)))
	        (when (savehist-printable value)
                  ;; When we have a max-size, chop off the last elements.
                  (when (and (consp elem)
                             (listp value)
                             (length> value (cdr elem)))
                    (setq value (copy-sequence value))
                    (setcdr (nthcdr (cdr elem) value) nil))
	          (prin1 `(setq ,symbol ',value) (current-buffer))
	          (insert ?\n))))))))
    ;; If autosaving, avoid writing if nothing has changed since the
    ;; last write.
    ;; Note: it may be the case that nothing changed but that the
    ;; underlying file is not what we would write (because the file has been
    ;; overwritten by something else, but reloading the file had no effect);
    ;; the fact that we still do not autosave in this case is not a bug:
    ;; without this, when several Emacs processes share the same history
    ;; file, they'd keep fighting over it without ever reaching a fixpoint.
    (let ((checksum (md5 (current-buffer) nil nil savehist-coding-system)))
      (condition-case err
        (unless (and auto-save (equal checksum savehist-last-checksum))
	  ;; Set file-precious-flag when saving the buffer because we
	  ;; don't want a half-finished write ruining the entire
	  ;; history.  Remember that this is run from a timer and from
	  ;; kill-emacs-hook, and also that multiple Emacs instances
	  ;; could write to this file at once.
	  (let ((file-precious-flag t)
	        (coding-system-for-write savehist-coding-system)
                (dir (file-name-directory savehist-file)))
            ;; Ensure that the directory exists before saving.
            (unless (file-exists-p dir)
              (make-directory dir t))
	    (write-region (point-min) (point-max) savehist-file nil
			  (unless (called-interactively-p 'interactive) 'quiet)))
	  (when savehist-file-modes
	    (set-file-modes savehist-file savehist-file-modes))
          (setq savehist--file-sync-modtime (savehist--file-modtime))
	  (setq savehist-last-checksum checksum))
        (file-error
         (unless savehist--has-given-file-warning
          (lwarn '(savehist-file) :warning "Error writing `%s': %s"
                 savehist-file (caddr err))
          (setq savehist--has-given-file-warning t)))))))

(defun savehist-autosave ()
  "Save the minibuffer history if it has been modified since the last save.
Does nothing if Savehist mode is off."
  (when savehist-mode
    (savehist-save t)))

(define-obsolete-function-alias 'savehist-trim-history #'identity "27.1")

(defun savehist-printable (value)
  "Return non-nil if VALUE is printable."
  (cond
   ;; Quick response for oft-encountered types known to be printable.
   ((numberp value))
   ((symbolp value))
   ;; String without properties
   ((and (stringp value)
	 (equal-including-properties value (substring-no-properties value))))
   (t
    ;; For others, check explicitly.
    (with-temp-buffer
      (condition-case nil
	  (let ((print-level nil))
	    ;; Print the value into a buffer...
	    (prin1 value (current-buffer))
	    ;; ...and attempt to read it.
	    (read (point-min-marker))
	    ;; The attempt worked: the object is printable.
	    t)
	;; The attempt failed: the object is not printable.
	(error nil))))))

(defun savehist-minibuffer-hook ()
  (unless (or (eq minibuffer-history-variable t)
	      ;; If `read-string' is called with a t HISTORY argument
	      ;; (which `read-password' does),
	      ;; `minibuffer-history-variable' is bound to t to mean
	      ;; "no history is being recorded".
	      (memq minibuffer-history-variable savehist-ignored-variables)
	      ;; Filter out uninterned history vars since we can't
              ;; reliably write+read them back in anyway (and presumably
              ;; they are not intended to survive sessions).
	      (not (eq (intern-soft minibuffer-history-variable)
	               minibuffer-history-variable)))
    (add-to-list 'savehist-minibuffer-history-variables
		 minibuffer-history-variable)))

(provide 'savehist)


;;; savehist.el ends here
