;;; reporter.el --- customizable bug reporting of lisp programs

;; Author: 1993 Barry A. Warsaw, Century Computing Inc. <bwarsaw@cen.com>
;; Maintainer:      bwarsaw@cen.com
;; Created:         19-Apr-1993
;; Version:         1.23
;; Last Modified:   1993/09/02 20:28:36
;; Keywords: tools, mail, lisp, extensions

;; Copyright (C) 1993 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Introduction
;; ============
;; This program is for lisp package authors and is used to ease
;; reporting of bugs.  When invoked, reporter-submit-bug-report will
;; set up a mail buffer with the appropriate bug report address,
;; including a lisp expression the maintainer of the package can use
;; to completely reproduce the environment in which the bug was
;; observed (e.g. by using eval-last-sexp). This package is especially
;; useful for my development of c++-mode.el, which is highly dependent
;; on its configuration variables.
;;
;; Do a "C-h f reporter-submit-bug-report" for more information.
;; Here's an example usage:
;;
;; (defconst mypkg-version "9.801")
;; (defconst mypkg-maintainer-address "mypkg-help@foo.com")
;; (defun mypkg-submit-bug-report ()
;;   "Submit via mail a bug report on mypkg"
;;   (interactive)
;;   (require 'reporter)
;;   (and (y-or-n-p "Do you really want to submit a report on mypkg? ")
;;        (reporter-submit-bug-report
;;          mypkg-maintainer-address
;;          (concat "mypkg.el " mypkg-version)
;;          (list 'mypkg-variable-1
;;                'mypkg-variable-2
;;                ;; ...
;;                'mypkg-variable-last))))

;; Mailing List
;; ============
;; I've set up a mailing list to report bugs or suggest enhancements,
;; etc. This list's intended audience is elisp package authors who are
;; using reporter and want to stay current with releases. Here are the
;; relevent addresses:
;;
;; Administrivia: reporter-request@anthem.nlm.nih.gov
;; Submissions:   reporter@anthem.nlm.nih.gov

;; LCD Archive Entry:
;; reporter|Barry A. Warsaw|bwarsaw@cen.com|
;; Customizable bug reporting of lisp programs.|
;; 1993/09/02 20:28:36|1.23|~/misc/reporter.el.Z|

;;; Code:


;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;; user defined variables

(defvar reporter-mailer '(vm-mail mail)
  "*Mail package to use to generate bug report buffer.
This can either be a function symbol or a list of function symbols.
If a list, it tries to use each specified mailer in order until an
existing one is found.")


;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; end of user defined variables

(defvar reporter-eval-buffer nil
  "Buffer to retrieve variable's value from.
This is necessary to properly support the printing of buffer-local
variables.  Current buffer will always be the mail buffer being
composed.")


(defun reporter-dump-variable (varsym)
  "Pretty-print the value of the variable in symbol VARSYM."
  (let ((val (save-excursion
	       (set-buffer reporter-eval-buffer)
	       (eval varsym)))
	(sym (symbol-name varsym))
	(print-escape-newlines t))
    (insert "     " sym " "
	    (cond
	     ((memq val '(t nil)) "")
	     ((listp val) "'")
	     ((symbolp val) "'")
	     (t ""))
	    (prin1-to-string val)
	    "\n")))

(defun reporter-dump-state (pkgname varlist pre-hooks post-hooks)
  "Dump the state of the mode specific variables.
PKGNAME contains the name of the mode as it will appear in the bug
report (you must explicitly concat any version numbers).

VARLIST is the list of variables to dump.  Each element in VARLIST can
be a variable symbol, or a cons cell.  If a symbol, this will be
passed to `reporter-dump-variable' for insertion into the mail buffer.
If a cons cell, the car must be a variable symbol and the cdr must be
a function which will be `funcall'd with the symbol. Use this to write
your own custom variable value printers for specific variables.

Note that the global variable `reporter-eval-buffer' will be bound to
the buffer in which `reporter-submit-bug-report' was invoked.  If you
want to print the value of a buffer local variable, you should wrap
the `eval' call in your custom printer inside a `set-buffer' (and
probably a `save-excursion'). `reporter-dump-variable' handles this
properly.

PRE-HOOKS is run after the emacs-version and PKGNAME are inserted, but
before the VARLIST is dumped.  POST-HOOKS is run after the VARLIST is
dumped."
  (let ((buffer (current-buffer)))
    (set-buffer buffer)
    (insert "Emacs  : " (emacs-version) "\nPackage: " pkgname "\n")
    (run-hooks 'pre-hooks)
    (insert "\ncurrent state:\n==============\n(setq\n")
    (mapcar
     (function
      (lambda (varsym-or-cons-cell)
	(let ((varsym (or (car-safe varsym-or-cons-cell)
			  varsym-or-cons-cell))
	      (printer (or (cdr-safe varsym-or-cons-cell)
			   'reporter-dump-variable)))
	  (funcall printer varsym)
	  )))
     varlist)
    (insert "     )\n")
    (run-hooks 'post-hooks)
    ))

(defun reporter-submit-bug-report
  (address pkgname varlist &optional pre-hooks post-hooks salutation)
  "Submit a bug report via mail.

ADDRESS is the email address for the package's maintainer. PKGNAME is
the name of the mode (you must explicitly concat any version numbers).
VARLIST is the list of variables to dump (do a `\\[describe-function] reporter-dump-state'
for details). Optional PRE-HOOKS and POST-HOOKS are passed to
`reporter-dump-state'. Optional SALUTATION is inserted at the top of the
mail buffer, and point is left after the saluation.

The mailer used is described in the variable `reporter-mailer'."
  (let ((reporter-eval-buffer (current-buffer))
	(mailbuf
	 (progn
	   (call-interactively
	    (if (nlistp reporter-mailer)
		reporter-mailer
	      (let ((mlist reporter-mailer)
		    (mailer nil))
		(while mlist
		  (if (commandp (car mlist))
		      (setq mailer (car mlist)
			    mlist nil)
		    (setq mlist (cdr mlist))))
		(if (not mailer)
		    (error
		     "variable `%s' does not contain a command for mailing."
		     "reporter-mailer"))
		mailer)))
	   (current-buffer))))
    (require 'sendmail)
    (pop-to-buffer reporter-eval-buffer)
    (pop-to-buffer mailbuf)
    (goto-char (point-min))
    ;; different mailers use different separators, some may not even
    ;; use m-h-s, but sendmail.el stuff must have m-h-s bound.
    (let ((mail-header-separator
           (save-excursion
             (re-search-forward
              (concat
               "^\\("			;beginning of line
               (mapconcat
                'identity
                (list "[\t ]*"          ;simple SMTP form
                      "-+"		;mh-e form
		      (regexp-quote 
		       mail-header-separator)) ;sendmail.el form
                "\\|")			;or them together
               "\\)$")			;end of line
              nil
              'move)			;search for and move
             (buffer-substring (match-beginning 0) (match-end 0)))))
      (mail-position-on-field "to")
      (insert address)
      (mail-position-on-field "subject")
      (insert "Report on package " pkgname)
      (re-search-forward mail-header-separator (point-max) 'move)
      (forward-line 1)
      (and salutation (insert "\n" salutation "\n\n"))
      (set-mark (point))                ;user should see mark change
      (insert "\n\n")
      (reporter-dump-state pkgname varlist pre-hooks post-hooks)
      (exchange-point-and-mark))
    (let* ((sendkey "C-c C-c")		;can this be generalized like below?
	   (killkey-whereis (where-is-internal 'kill-buffer nil t))
	   (killkey (if killkey-whereis
			(key-description killkey-whereis)
		      "M-x kill-buffer")))
      (message "Please type in your report. Hit %s to send, %s to abort."
	       sendkey killkey))
    ))

;; this is useful
(provide 'reporter)

;;; reporter.el ends here
