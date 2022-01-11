;;; dictionary-connection.el --- TCP-based client connection for dictionary  -*- lexical-binding:t -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

;; Author: Torsten Hilbrich <torsten.hilbrich@gmx.net>
;; Keywords: network

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

;; dictionary-connection allows handling TCP-based connections in
;; client mode where text-based information is exchanged.  There is
;; special support for handling CR LF (and the usual CR LF . CR LF
;; terminator).

;;; Code:

(defsubst dictionary-connection-p (connection)
  "Return non-nil if CONNECTION is a connection object."
  (get connection 'connection))

(defsubst dictionary-connection-read-point (connection)
  "Return the read point of the CONNECTION object."
  (get connection 'dictionary-connection-read-point))

(defsubst dictionary-connection-process (connection)
  "Return the process of the CONNECTION object."
  (get connection 'dictionary-connection-process))

(defsubst dictionary-connection-buffer (connection)
  "Return the buffer of the CONNECTION object."
  (get connection 'dictionary-connection-buffer))

(defsubst dictionary-connection-set-read-point (connection point)
  "Set the read-point for CONNECTION to POINT."
  (put connection 'dictionary-connection-read-point point))

(defsubst dictionary-connection-set-process (connection process)
  "Set the process for CONNECTION to PROCESS."
  (put connection 'dictionary-connection-process process))

(defsubst dictionary-connection-set-buffer (connection buffer)
  "Set the buffer for CONNECTION to BUFFER."
  (put connection 'dictionary-connection-buffer buffer))

(defun dictionary-connection-create-data (buffer process point)
  "Create a new connection data based on BUFFER, PROCESS, and POINT."
  (let ((connection (make-symbol "connection")))
    (put connection 'connection t)
    (dictionary-connection-set-read-point connection point)
    (dictionary-connection-set-process connection process)
    (dictionary-connection-set-buffer connection buffer)
    connection))

(defun dictionary-connection-open (server port)
  "Open a connection to SERVER at PORT.
Return a data structure identifying the connection."

  (let ((process-buffer (generate-new-buffer (format " connection to %s:%s"
						     server
						     port)))
	(process))
    (with-current-buffer process-buffer
      (setq process (open-network-stream "connection" process-buffer
					 server port))
      (dictionary-connection-create-data process-buffer process (point-min)))))

(defun dictionary-connection-status (connection)
  "Return the status of the CONNECTION.
Possible return values are the symbols:
    nil:    argument is not a connection object
    'none:  argument is not connected
    'up:    connection is open and buffer is existing
    'down:  connection is closed
    'alone: connection is not associated with a buffer"
  (when (dictionary-connection-p connection)
    (let ((process (dictionary-connection-process connection))
          (buffer (dictionary-connection-buffer connection)))
      (if (not process)
          'none
        (if (not (buffer-live-p buffer))
            'alone
          (if (not (eq (process-status process) 'open))
              'down
            'up))))))

(defun dictionary-connection-close (connection)
  "Force closing of the CONNECTION."
  (when (dictionary-connection-p connection)
    (let ((buffer (dictionary-connection-buffer connection))
          (process (dictionary-connection-process connection)))
      (if process
          (delete-process process))
      (if buffer
          (kill-buffer buffer))

      (dictionary-connection-set-process connection nil)
      (dictionary-connection-set-buffer connection nil))))

(defun dictionary-connection-send (connection data)
  "Send DATA to the process stored in CONNECTION."
  (unless (eq (dictionary-connection-status connection) 'up)
    (error "Connection is not up"))
  (with-current-buffer (dictionary-connection-buffer connection)
    (goto-char (point-max))
    (dictionary-connection-set-read-point connection (point))
    (process-send-string (dictionary-connection-process connection) data)))

(defun dictionary-connection-send-crlf (connection data)
  "Send DATA together with CRLF to the process found in CONNECTION."
  (dictionary-connection-send connection (concat data "\r\n")))

(defun dictionary-connection-read (connection delimiter)
  "Read data from CONNECTION until DELIMITER is found inside the buffer."
  (unless (eq (dictionary-connection-status connection) 'up)
    (error "Connection is not up"))
  (let ((case-fold-search nil)
	match-end)
    (with-current-buffer (dictionary-connection-buffer connection)
      (goto-char (dictionary-connection-read-point connection))
      ;; Wait until there is enough data
      (while (not (search-forward-regexp delimiter nil t))
	(accept-process-output (dictionary-connection-process connection) 3)
	(goto-char (dictionary-connection-read-point connection)))
      (setq match-end (point))
      ;; Return the result
      (let ((result (buffer-substring (dictionary-connection-read-point connection)
				      match-end)))
	(dictionary-connection-set-read-point connection match-end)
	result))))

(defun dictionary-connection-read-crlf (connection)
  "Read from CONNECTION until a line is completed with CRLF."
  (dictionary-connection-read connection "\015?\012"))

(defun dictionary-connection-read-to-point (connection)
  "Read from CONNECTION until an end of entry is encountered.
End of entry is a decimal point found on a line by itself."
  (dictionary-connection-read connection "\015?\012[.]\015?\012"))

(provide 'dictionary-connection)
;;; dictionary-connection.el ends here
