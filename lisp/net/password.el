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

(provide 'password)

;;; arch-tag: ab160494-16c8-4c68-a4a1-73eebf6686e5
;;; password.el ends here
