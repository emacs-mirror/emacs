;;; netunam.el --- HP-UX RFA Commands

;; Copyright (C) 1988 Free Software Foundation, Inc.

;; Author: Chris Hanson <cph@zurich.ai.mit.edu>
;; Keywords: comm

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

;;; Commentary:

;; Use the Remote File Access (RFA) facility of HP-UX from Emacs.

;;; Code:

(defconst rfa-node-directory "/net/"
  "Directory in which RFA network special files are stored.
By HP convention, this is \"/net/\".")

(defvar rfa-default-node nil
  "If not nil, this is the name of the default RFA network special file.")

(defvar rfa-password-memoize-p t
  "If non-nil, remember login user's passwords after they have been entered.")

(defvar rfa-password-alist '()
  "An association from node-name strings to password strings.
Used if `rfa-password-memoize-p' is non-nil.")

(defvar rfa-password-per-node-p t
  "If nil, login user uses same password on all machines.
Has no effect if `rfa-password-memoize-p' is nil.")

(defun rfa-set-password (password &optional node user)
  "Add PASSWORD to the RFA password database.
Optional second arg NODE is a string specifying a particular nodename;
 if supplied and not nil, PASSWORD applies to only that node.
Optional third arg USER is a string specifying the (remote) user whose
 password this is; if not supplied this defaults to (user-login-name)."
  (if (not user) (setq user (user-login-name)))
  (let ((node-entry (assoc node rfa-password-alist)))
    (if node-entry
	(let ((user-entry (assoc user (cdr node-entry))))
	  (if user-entry
	      (rplacd user-entry password)
	      (rplacd node-entry
		      (nconc (cdr node-entry)
			     (list (cons user password))))))
	(setq rfa-password-alist
	      (nconc rfa-password-alist
		     (list (list node (cons user password))))))))

(defun rfa-open (node &optional user password)
  "Open a network connection to a server using remote file access.
First argument NODE is the network node for the remote machine.
Second optional argument USER is the user name to use on that machine.
  If called interactively, the user name is prompted for.
Third optional argument PASSWORD is the password string for that user.
  If not given, this is filled in from the value of
`rfa-password-alist', or prompted for.  A prefix argument of - will
cause the password to be prompted for even if previously memoized."
  (interactive
   (list (read-file-name "rfa-open: " rfa-node-directory rfa-default-node t)
	 (read-string "user-name: " (user-login-name))))
  (let ((node
	 (and (or rfa-password-per-node-p
		  (not (equal user (user-login-name))))
	      node)))
    (if (not password)
	(setq password
	      (let ((password
		     (cdr (assoc user (cdr (assoc node rfa-password-alist))))))
		(or (and (not current-prefix-arg) password)
		    (rfa-password-read
		     (format "password for user %s%s: "
			     user
			     (if node (format " on node \"%s\"" node) ""))
		     password))))))
  (let ((result
	 (sysnetunam (expand-file-name node rfa-node-directory)
		     (concat user ":" password))))
    (if (interactive-p)
	(if result
	    (message "Opened network connection to %s as %s" node user)
	    (error "Unable to open network connection")))
    (if (and rfa-password-memoize-p result)
	(rfa-set-password password node user))
    result))

(defun rfa-close (node)
  "Close a network connection to a server using remote file access.
NODE is the network node for the remote machine."
  (interactive
   (list (read-file-name "rfa-close: " rfa-node-directory rfa-default-node t)))
  (let ((result (sysnetunam (expand-file-name node rfa-node-directory) "")))
    (cond ((not (interactive-p)) result)
	  ((not result) (error "Unable to close network connection"))
	  (t (message "Closed network connection to %s" node)))))

(defun rfa-password-read (prompt default)
  (let ((rfa-password-accumulator (or default "")))
    (read-from-minibuffer prompt
			  (and default
			       (let ((copy (concat default))
				     (index 0)
				     (length (length default)))
				 (while (< index length)
				   (aset copy index ?.)
				   (setq index (1+ index)))
				 copy))
			  rfa-password-map)
    rfa-password-accumulator))

(defvar rfa-password-map nil)
(if (not rfa-password-map)
    (let ((char ? ))
      (setq rfa-password-map (make-keymap))
      (while (< char 127)
	(define-key rfa-password-map (char-to-string char)
	  'rfa-password-self-insert)
	(setq char (1+ char)))
      (define-key rfa-password-map "\C-g"
	'abort-recursive-edit)
      (define-key rfa-password-map "\177"
	'rfa-password-rubout)
      (define-key rfa-password-map "\n"
	'exit-minibuffer)
      (define-key rfa-password-map "\r"
	'exit-minibuffer)))

(defvar rfa-password-accumulator nil)

(defun rfa-password-self-insert ()
  (interactive)
  (setq rfa-password-accumulator
	(concat rfa-password-accumulator
		(char-to-string last-command-char)))
  (insert ?.))

(defun rfa-password-rubout ()
  (interactive)
  (delete-char -1)
  (setq rfa-password-accumulator
	(substring rfa-password-accumulator 0 -1)))

;;; netunam.el ends here
