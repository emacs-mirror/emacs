;;; password.el --- Read passwords from user, possibly using a password cache.

;; Copyright (C) 1999, 2000, 2003, 2004 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Created: 2003-12-21
;; Keywords: password cache passphrase key

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

;; Greatly influenced by pgg.el written by Daiki Ueno, with timer
;; fixes for XEmacs by Katsumi Yamaoka.  In fact, this is mostly just
;; a rip-off.
;;
;; (password-read "Password? " "test")
;; ;; Minibuffer prompt for password.
;;  => "foo"
;;
;; (password-cache-add "test" "foo")
;;  => nil

;; Note the previous two can be replaced with:
;; (password-read-and-add "Password? " "test")
;; ;; Minibuffer prompt for password.
;; => "foo"
;; ;; "foo" is now cached with key "test"


;; (password-read "Password? " "test")
;; ;; No minibuffer prompt
;;  => "foo"
;;
;; (password-read "Password? " "test")
;; ;; No minibuffer prompt
;;  => "foo"
;;
;; ;; Wait `password-cache-expiry' seconds.
;;
;; (password-read "Password? " "test")
;; ;; Minibuffer prompt for password is back.
;;  => "foo"

;;; Code:

(when (featurep 'xemacs)
  (require 'run-at-time))

(eval-when-compile
  (require 'cl))

(defcustom password-cache t
  "Whether to cache passwords."
  :group 'password
  :type 'boolean)

(defcustom password-cache-expiry 16
  "How many seconds passwords are cached, or nil to disable expiring.
Whether passwords are cached at all is controlled by `password-cache'."
  :group 'password
  :type '(choice (const :tag "Never" nil)
		 (integer :tag "Seconds")))

(defvar password-data (make-vector 7 0))

(defun password-read (prompt &optional key)
  "Read password, for use with KEY, from user, or from cache if wanted.
KEY indicate the purpose of the password, so the cache can
separate passwords.  The cache is not used if KEY is nil.  It is
typically a string.
The variable `password-cache' control whether the cache is used."
  (or (and password-cache
	   key
	   (symbol-value (intern-soft key password-data)))
      (read-passwd prompt)))

(defun password-read-and-add (prompt &optional key)
  "Read password, for use with KEY, from user, or from cache if wanted.
Then store the password in the cache.  Uses `password-read' and
`password-cache-add'."
  (let ((password (password-read prompt key)))
    (when (and password key)
      (password-cache-add key password))
    password))

(defun password-cache-remove (key)
  "Remove password indexed by KEY from password cache.
This is typically run be a timer setup from `password-cache-add',
but can be invoked at any time to forcefully remove passwords
from the cache.  This may be useful when it has been detected
that a password is invalid, so that `password-read' query the
user again."
  (let ((password (symbol-value (intern-soft key password-data))))
    (when password
      (fillarray password ?_)
      (unintern key password-data))))

(defun password-cache-add (key password)
  "Add password to cache.
The password is removed by a timer after `password-cache-expiry'
seconds."
  (set (intern key password-data) password)
  (when password-cache-expiry
    (run-at-time password-cache-expiry nil
		 #'password-cache-remove
		 key))
  nil)

;;;###autoload
(defun read-passwd (prompt &optional confirm default)
  "Read a password, prompting with PROMPT, and return it.
If optional CONFIRM is non-nil, read the password twice to make sure.
Optional DEFAULT is a default password to use instead of empty input.

This function echoes `.' for each character that the user types.
The user ends with RET, LFD, or ESC.  DEL or C-h rubs out.  C-u kills line.
C-g quits; if `inhibit-quit' was non-nil around this function,
then it returns nil if the user types C-g.

Once the caller uses the password, it can erase the password
by doing (clear-string STRING)."
  (with-local-quit
    (if confirm
	(let (success)
	  (while (not success)
	    (let ((first (read-passwd prompt nil default))
		  (second (read-passwd "Confirm password: " nil default)))
	      (if (equal first second)
		  (progn
		    (and (arrayp second) (clear-string second))
		    (setq success first))
		(and (arrayp first) (clear-string first))
		(and (arrayp second) (clear-string second))
		(message "Password not repeated accurately; please start over")
		(sit-for 1))))
	  success)
      (let ((pass nil)
	    (c 0)
	    (echo-keystrokes 0)
	    (cursor-in-echo-area t))
	(while (progn (message "%s%s"
			       prompt
			       (make-string (length pass) ?.))
		      (setq c (read-char-exclusive nil t))
		      (and (/= c ?\r) (/= c ?\n) (/= c ?\e)))
	  (clear-this-command-keys)
	  (if (= c ?\C-u)
	      (progn
		(and (arrayp pass) (clear-string pass))
		(setq pass ""))
	    (if (and (/= c ?\b) (/= c ?\177))
		(let* ((new-char (char-to-string c))
		       (new-pass (concat pass new-char)))
		  (and (arrayp pass) (clear-string pass))
		  (clear-string new-char)
		  (setq c ?\0)
		  (setq pass new-pass))
	      (if (> (length pass) 0)
		  (let ((new-pass (substring pass 0 -1)))
		    (and (arrayp pass) (clear-string pass))
		    (setq pass new-pass))))))
	(message nil)
	(or pass default "")))))

(provide 'password)

;;; arch-tag: ab160494-16c8-4c68-a4a1-73eebf6686e5
;;; password.el ends here
