;;; savehist.el --- Save minibuffer history

;; Copyright (c) 1997 Free Software Foundation

;; Author: Hrvoje Niksic <hniksic@xemacs.org>
;; Keywords: minibuffer
;; Version: 0.4

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

;; This package provides the feature of saving minibuffer
;; history to an external file after exit.  When Emacs is about the exit,
;; `savehist-save' will dump the contents of various minibuffer
;; histories (as determined by `savehist-history-variables') to a save
;; file (`~/.emacs-history' by default).  Although the package was
;; designed for saving the minibuffer histories, any variables can be
;; saved that way.

;; To use savehist, put the following to `~/.emacs':
;;
;; (require 'savehist)
;; (savehist-load)

;; Be sure to have `savehist.el' in a directory that is in your
;; load-path, and byte-compile it.

;;; Code:

;; User variables

(defgroup savehist nil
  "Save minibuffer history."
  :group 'minibuffer)


(defcustom savehist-history-variables
  '(
    ;; Catch-all minibuffer history
    minibuffer-history
    ;; File-oriented commands
    file-name-history
    ;; Regexp-related reads
    regexp-history
    ;; Searches in minibuffer (via `M-r' and such)
    minibuffer-history-search-history
    ;; Query replace
    query-replace-history
    ;; eval-expression (`M-:')
    read-expression-history
    ;; shell-command (`M-!')
    shell-command-history
    ;; compile
    compile-history
    ;; find-tag (`M-.')
    find-tag-history
    ;; grep
    grep-history
    ;; Viper stuff
    vip-ex-history vip-search-history
    vip-replace1-history vip-replace2-history
    vip-shell-history vip-search-history

    ;; XEmacs-specific:
    ;; Buffer-related commands
    buffer-history
    ;; Reads of variables and functions
    variable-history function-history
    ;; Extended commands
    read-command-history

    ;; Info, lookup, and bookmark historys
    Info-minibuffer-history
    Manual-page-minibuffer-history

    ;; Emacs-specific:
    ;; Extended commands
    extended-command-history)
  "*List of symbols to be saved.
Every symbol should refer to a variable.  The variable will be saved
only if it is bound and has a non-nil value.  Thus it is safe to
specify a superset of the variables a user is expected to want to
save.

Default value contains minibuffer history variables used by Emacs, XEmacs, 
and Viper (uh-oh)."
  :type '(repeat (symbol :tag "Variable"))
  :group 'savehist)

(defcustom savehist-file "~/.emacs-history"
  "*File name to save minibuffer history to.
The minibuffer history is a series of Lisp expressions, which should be
loaded using `savehist-load' from your .emacs.  See `savehist-load' for
more details."
  :type 'file
  :group 'savehist)

(defcustom savehist-length 100
  "*Maximum length of a minibuffer list.
If set to nil, the length is unlimited."
  :type '(choice integer
		 (const :tag "Unlimited" nil))
  :group 'savehist)

(defcustom savehist-modes 384
  "*Default permissions of the history file.
This is decimal, not octal.  The default is 384 (0600 in octal)."
  :type 'integer
  :group 'savehist)


;; Functions

;;;###autoload
(defun savehist-load (&optional no-hook)
  "Load the minibuffer histories from `savehist-file'.
Unless NO-HOOK is specified, the function will also add the save function
to `kill-emacs-hook', thus ensuring that the minibuffer contents will be
saved before leaving Emacs.

This function should be normally used from your Emacs init file.  Since it
removes your current minibuffer histories, it is unwise to call it at any
other time."
  (interactive "P")
  (unless no-hook
    (add-hook 'kill-emacs-hook 'savehist-save))
  (load savehist-file t))

;;;###autoload
(defun savehist-save ()
  "Save the histories from `savehist-history-variables' to `savehist-file'.
A variable will be saved if it is bound and non-nil."
  (interactive)
  (save-excursion
    ;; Is it wise to junk `find-file-hooks' just like that?  How else
    ;; should I avoid font-lock et al.?
    (let ((find-file-hooks nil)
	  (buffer-exists-p (get-file-buffer savehist-file)))
      (set-buffer (find-file-noselect savehist-file))
      (unwind-protect
	  (progn
	    (erase-buffer)
	    (insert
	     ";; -*- emacs-lisp -*-\n"
	     ";; Minibuffer history file.\n\n"
	     ";; This file is automatically generated by `savehist-save'"
	     " or when\n"
	     ";; exiting Emacs.\n"
	     ";; Do not edit.  Unless you really want to, that is.\n\n")
	    (let ((print-length nil)
		  (print-string-length nil)
		  (print-level nil)
		  (print-readably t))
	      (dolist (sym savehist-history-variables)
		(when (and (boundp sym)
			   (symbol-value sym))
		  (prin1
		   `(setq ,sym (quote ,(savehist-delimit (symbol-value sym)
							 savehist-length)))
		   (current-buffer))
		  (insert ?\n))))
	    (save-buffer)
	    (set-file-modes savehist-file savehist-modes))
	(or buffer-exists-p
	    (kill-buffer (current-buffer)))))))

;; If ARG is a list with less than N elements, return it, else return
;; its subsequence of N elements.  If N is nil or ARG is not a list,
;; always return ARG.
(defun savehist-delimit (arg n)
  (if (and n
	   (listp arg)
	   (> (length arg) n))
      (subseq arg 0 n)
    arg))

(provide 'savehist)

;;; savehist.el ends here
