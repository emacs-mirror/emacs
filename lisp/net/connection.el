;;; connection.el --- TCP-based client connection

;; Author: Torsten Hilbrich <torsten.hilbrich@gmx.net>
;; Keywords: network
;; Version: 1.11

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; connection allows to handle TCP-based connections in client mode
;; where text-based information are exchanged. There is special
;; support for handling CR LF (and the usual CR LF . CR LF
;; terminater).

;;; Code:

(eval-when-compile
  (require 'cl))

(defmacro connection-p (connection)
  "Returns non-nil if `connection' is a connection object"
  (list 'get connection ''connection))

(defmacro connection-read-point (connection)
  "Return the read point of the connection object."
  (list 'get connection ''connection-read-point))

(defmacro connection-process (connection)
  "Return the process of the connection object."
  (list 'get connection ''connection-process))

(defmacro connection-buffer (connection)
  "Return the buffer of the connection object."
  (list 'get connection ''connection-buffer))

(defmacro connection-set-read-point (connection point)
  "Set the read-point for `connection' to `point'."
  (list 'put connection ''connection-read-point point))

(defmacro connection-set-process (connection process)
  "Set the process for `connection' to `process'."
  (list 'put connection ''connection-process process))

(defmacro connection-set-buffer (connection buffer)
  "Set the buffer for `connection' to `buffer'."
  (list 'put connection ''connection-buffer buffer))

(defun connection-create-data (buffer process point)
  "Create a new connection data based on `buffer', `process', and `point'."
  (let ((connection (make-symbol "connection")))
    (put connection 'connection t)
    (connection-set-read-point connection point)
    (connection-set-process connection process)
    (connection-set-buffer connection buffer)
    connection))

(defun connection-open (server port)
  "Open a connection to `server' and `port'.
A data structure identifing the connection is returned"

  (let ((process-buffer (generate-new-buffer (format " connection to %s:%s"
						     server
						     port)))
	(process))
    (with-current-buffer process-buffer
      (setq process (open-network-stream "connection" process-buffer
					 server port))
      (connection-create-data process-buffer process (point-min)))))

(defun connection-status (connection)
  "Return the status of the connection.
Possible return values are the symbols:
nil: argument is no connection object
'none: argument has no connection
'up: connection is open and buffer is existing
'down: connection is closed
'alone: connection is not associated with a buffer"
  (if (connection-p connection)
      (let ((process (connection-process connection))
	    (buffer (connection-buffer connection)))
	(if (not process)
	    'none
	  (if (not (buffer-live-p buffer))
	      'alone
	    (if (not (eq (process-status process) 'open))
		'down
	      'up))))
    nil))

(defun connection-close (connection)
  "Force closing of the connection."
  (if (connection-p connection)
      (progn
	(let ((buffer (connection-buffer connection))
	      (process (connection-process connection)))
	  (if process
	      (delete-process process))
	  (if buffer
	      (kill-buffer buffer))

	  (connection-set-process connection nil)
	  (connection-set-buffer connection nil)))))

(defun connection-send (connection data)
  "Send `data' to the process."
  (unless (eq (connection-status connection) 'up)
    (error "Connection is not up"))
  (with-current-buffer (connection-buffer connection)
    (goto-char (point-max))
    (connection-set-read-point connection (point))
    (process-send-string (connection-process connection) data)))

(defun connection-send-crlf (connection data)
  "Send `data' together with CRLF to the process."
  (connection-send connection (concat data "\r\n")))

(defun connection-read (connection delimiter)
  "Read data until `delimiter' is found inside the buffer."
  (unless (eq (connection-status connection) 'up)
    (error "Connection is not up"))
  (let ((case-fold-search nil)
	match-end)
    (with-current-buffer (connection-buffer connection)
      (goto-char (connection-read-point connection))
      ;; Wait until there is enough data
      (while (not (search-forward-regexp delimiter nil t))
	(accept-process-output (connection-process connection) 3)
	(goto-char (connection-read-point connection)))
      (setq match-end (point))
      ;; Return the result
      (let ((result (buffer-substring (connection-read-point connection)
				      match-end)))
	(connection-set-read-point connection match-end)
	result))))

(defun connection-read-crlf (connection)
  "Read until a line is completedx with CRLF"
  (connection-read connection "\015?\012"))

(defun connection-read-to-point (connection)
  "Read until a line is consisting of a single point"
  (connection-read connection "\015?\012[.]\015?\012"))

(provide 'connection)
;;; connection.el ends here
