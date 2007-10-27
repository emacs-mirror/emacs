;;; tramp-cmds.el --- Interactive commands for Tramp

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes

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
;; along with GNU Emacs; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides all interactive commands which are releated
;; to Tramp.

;;; Code:

(require 'tramp)

(defun tramp-list-tramp-buffers ()
  "Return a list of all Tramp connection buffers."
  (append
   (all-completions
    "*tramp" (mapcar 'list (mapcar 'buffer-name (buffer-list))))
   (all-completions
    "*debug tramp" (mapcar 'list (mapcar 'buffer-name (buffer-list))))))

(defun tramp-list-remote-buffers ()
  "Return a list of all buffers with remote default-directory."
  (delq
   nil
   (mapcar
    (lambda (x)
      (with-current-buffer x
	(when (and (stringp default-directory)
		   (file-remote-p default-directory))
	  x)))
    (buffer-list))))

(defun tramp-cleanup-connection (vec)
  "Flush all connection related objects.
This includes password cache, file cache, connection cache, buffers.
When called interactively, a Tramp connection has to be selected."
  (interactive
   ;; When interactive, select the Tramp remote identification.
   ;; Return nil when there is no Tramp connection.
   (list
    (let ((connections
	   (mapcar
	    (lambda (x)
	      (with-current-buffer x (list (file-remote-p default-directory))))
	    ;; We shall not count debug buffers, because their
	    ;; default-directory is random.  It could be even a remote
	    ;; one from another connection.
	    (all-completions
	     "*tramp" (mapcar 'list (tramp-list-tramp-buffers)))))
	  name)

      (when connections
	(setq name
	      (completing-read
	       "Enter Tramp connection: " connections nil t
	       (try-completion "" connections)))
	(when (and name (file-remote-p name))
	  (with-parsed-tramp-file-name name nil v))))))

  (if (not vec)
      ;; Nothing to do.
      (message "No Tramp connection found.")

    ;; Flush password cache.
    (tramp-clear-passwd vec)

    ;; Flush file cache.
    (tramp-flush-directory-property vec "/")

    ;; Flush connection cache.
    (tramp-flush-connection-property (tramp-get-connection-process vec) nil)
    (tramp-flush-connection-property vec nil)

    ;; Remove buffers.
    (dolist
	(buf (list (get-buffer (tramp-buffer-name vec))
		   (get-buffer (tramp-debug-buffer-name vec))
		   (tramp-get-connection-property vec "process-buffer" nil)))
      (when (bufferp buf) (kill-buffer buf)))))

(defun tramp-cleanup-all-connections ()
  "Flush all Tramp internal objects.
This includes password cache, file cache, connection cache, buffers."
  (interactive)

  ;; Flush password cache.
  (when (functionp 'password-reset)
    (funcall (symbol-function 'password-reset)))

  ;; Flush file and connection cache.
  (clrhash tramp-cache-data)

  ;; Remove buffers.
  (dolist (name (tramp-list-tramp-buffers))
    (when (bufferp (get-buffer name)) (kill-buffer name))))

(defun tramp-cleanup-all-buffers ()
  "Kill all remote buffers."
  (interactive)

  ;; Remove all Tramp related buffers.
  (tramp-cleanup-all-connections)

  ;; Remove all buffers with a remote default-directory.
  (dolist (name (tramp-list-remote-buffers))
    (when (bufferp (get-buffer name)) (kill-buffer name))))

(provide 'tramp-cmds)

;;; TODO:

;; * Clean up unused *tramp/foo* buffers after a while.  (Pete Forman)
;; * WIBNI there was an interactive command prompting for tramp
;;   method, hostname, username and filename and translates the user
;;   input into the correct filename syntax (depending on the Emacs
;;   flavor) (Reiner Steib)
;; * Let the user edit the connection properties interactively.
;;   Something like `gnus-server-edit-server' in Gnus' *Server* buffer.
;; * It's just that when I come to Customize `tramp-default-user-alist'
;;   I'm presented with a mismatch and raw lisp for a value.  It is my
;;   understanding that a variable declared with defcustom is a User
;;   Option and should not be modified by the code.  add-to-list is
;;   called in several places. One way to handle that is to have a new
;;   ordinary variable that gets its initial value from
;;   tramp-default-user-alist and then is added to. (Pete Forman)

;; arch-tag: 190d4c33-76bb-4e99-8b6f-71741f23d98c
;;; tramp-cmds.el ends here
