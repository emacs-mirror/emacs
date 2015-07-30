;;; pinentry.el --- GnuPG Pinentry server implementation -*- lexical-binding: t -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@gnu.org>
;; Version: 0.1
;; Keywords: GnuPG

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows GnuPG passphrase to be prompted through the
;; minibuffer instead of graphical dialog.
;;
;; To use, add allow-emacs-pinentry to ~/.gnupg/gpg-agent.conf, and
;; start the server with M-x pinentry-start.
;;
;; The actual communication path between the relevant components is
;; as follows:
;;
;;   gpg --> gpg-agent --> pinentry --> Emacs
;;
;; where pinentry and Emacs communicate through a Unix domain socket
;; created at:
;;
;;   ${TMPDIR-/tmp}/emacs$(id -u)/pinentry
;;
;; under the same directory which server.el uses.  The protocol is a
;; subset of the Pinentry Assuan protocol described in (info
;; "(pinentry) Protocol").
;;
;; NOTE: As of June 2015, this feature requires newer versions of
;; GnuPG (2.1.5+) and Pinentry (not yet released, possibly 0.9.5+).
;; For details, see the discussion on gnupg-devel mailing list:
;; <https://lists.gnupg.org/pipermail/gnupg-devel/2015-May/029875.html>.

;;; Code:

(defvar pinentry--server-process nil)
(defvar pinentry--connection-process-list nil)

(defvar pinentry--labels nil)
(put 'pinentry-read-point 'permanent-local t)
(defvar pinentry--read-point nil)
(put 'pinentry--read-point 'permanent-local t)

;; We use the same location as `server-socket-dir', when local sockets
;; are supported.
(defvar pinentry--socket-dir
  (format "%s/emacs%d" (or (getenv "TMPDIR") "/tmp") (user-uid))
  "The directory in which to place the server socket.
If local sockets are not supported, this is nil.")

(defconst pinentry--set-label-commands
  '("SETPROMPT" "SETTITLE" "SETDESC"
    "SETREPEAT" "SETREPEATERROR"
    "SETOK" "SETCANCEL" "SETNOTOK"))

;; These error codes are defined in libgpg-error/src/err-codes.h.in.
(defmacro pinentry--error-code (code)
  (logior (lsh 5 24) code))
(defconst pinentry--error-not-implemented
  (cons (pinentry--error-code 69) "not implemented"))
(defconst pinentry--error-cancelled
  (cons (pinentry--error-code 99) "cancelled"))
(defconst pinentry--error-not-confirmed
  (cons (pinentry--error-code 114) "not confirmed"))

(autoload 'server-ensure-safe-dir "server")

;;;###autoload
(defun pinentry-start ()
  "Start a Pinentry service.

Once the environment is properly set, subsequent invocations of
the gpg command will interact with Emacs for passphrase input."
  (interactive)
  (unless (featurep 'make-network-process '(:family local))
    (error "local sockets are not supported"))
  (if (process-live-p pinentry--server-process)
      (message "Pinentry service is already running")
    (let* ((server-file (expand-file-name "pinentry" pinentry--socket-dir)))
      (server-ensure-safe-dir pinentry--socket-dir)
      ;; Delete the socket files made by previous server invocations.
      (ignore-errors
        (let (delete-by-moving-to-trash)
          (delete-file server-file)))
      (setq pinentry--server-process
            (make-network-process
             :name "pinentry"
             :server t
             :noquery t
             :sentinel #'pinentry--process-sentinel
             :filter #'pinentry--process-filter
             :coding 'no-conversion
             :family 'local
             :service server-file))
      (process-put pinentry--server-process :server-file server-file))))

(defun pinentry-stop ()
  "Stop a Pinentry service."
  (interactive)
  (when (process-live-p pinentry--server-process)
    (delete-process pinentry--server-process))
  (setq pinentry--server-process nil)
  (dolist (process pinentry--connection-process-list)
    (when (buffer-live-p (process-buffer process))
      (kill-buffer (process-buffer process))))
  (setq pinentry--connection-process-list nil))

(defun pinentry--labels-to-shortcuts (labels)
  "Convert strings in LABEL by stripping mnemonics."
  (mapcar (lambda (label)
            (when label
              (let (c)
                (if (string-match "\\(?:\\`\\|[^_]\\)_\\([[:alnum:]]\\)" label)
                    (let ((key (match-string 1 label)))
                      (setq c (downcase (aref key 0)))
                      (setq label (replace-match
                                   (propertize key 'face 'underline)
                                   t t label)))
                  (setq c (if (= (length label) 0)
                              ??
                            (downcase (aref label 0)))))
                ;; Double underscores mean a single underscore.
                (when (string-match "__" label)
                  (setq label (replace-match "_" t t label)))
                (cons c label))))
          labels))

(defun pinentry--escape-string (string)
  "Escape STRING in the Assuan percent escape."
  (let ((length (length string))
        (index 0)
        (count 0))
    (while (< index length)
      (if (memq (aref string index) '(?\n ?\r ?%))
          (setq count (1+ count)))
      (setq index (1+ index)))
    (setq index 0)
    (let ((result (make-string (+ length (* count 2)) ?\0))
          (result-index 0)
          c)
      (while (< index length)
        (setq c (aref string index))
        (if (memq c '(?\n ?\r ?%))
            (let ((hex (format "%02X" c)))
              (aset result result-index ?%)
              (setq result-index (1+ result-index))
              (aset result result-index (aref hex 0))
              (setq result-index (1+ result-index))
              (aset result result-index (aref hex 1))
              (setq result-index (1+ result-index)))
          (aset result result-index c)
          (setq result-index (1+ result-index)))
        (setq index (1+ index)))
      result)))

(defun pinentry--unescape-string (string)
  "Unescape STRING in the Assuan percent escape."
  (let ((length (length string))
        (index 0))
    (let ((result (make-string length ?\0))
          (result-index 0)
          c)
      (while (< index length)
        (setq c (aref string index))
        (if (and (eq c '?%) (< (+ index 2) length))
	    (progn
	      (aset result result-index
		    (string-to-number (substring string
						 (1+ index)
						 (+ index 3))
				      16))
	      (setq result-index (1+ result-index))
	      (setq index (+ index 2)))
          (aset result result-index c)
          (setq result-index (1+ result-index)))
	(setq index (1+ index)))
      (substring result 0 result-index))))

(defun pinentry--send-data (process escaped)
  "Send a string ESCAPED to a process PROCESS.
ESCAPED will be split if it exceeds the line length limit of the
Assuan protocol."
  (let ((length (length escaped))
        (index 0))
    (if (= length 0)
        (process-send-string process "D \n")
      (while (< index length)
        ;; 997 = ASSUAN_LINELENGTH (= 1000) - strlen ("D \n")
        (let* ((sub-length (min (- length index) 997))
               (sub (substring escaped index (+ index sub-length))))
          (unwind-protect
              (progn
                (process-send-string process "D ")
                (process-send-string process sub)
                (process-send-string process "\n"))
            (clear-string sub))
          (setq index (+ index sub-length)))))))

(defun pinentry--send-error (process error)
  (process-send-string process (format "ERR %d %s\n" (car error) (cdr error))))

(defun pinentry--process-filter (process input)
  (unless (buffer-live-p (process-buffer process))
    (let ((buffer (generate-new-buffer " *pinentry*")))
      (set-process-buffer process buffer)
      (with-current-buffer buffer
        (if (fboundp 'set-buffer-multibyte)
            (set-buffer-multibyte nil))
        (make-local-variable 'pinentry--read-point)
        (setq pinentry--read-point (point-min))
        (make-local-variable 'pinentry--labels))))
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert input)
      (goto-char pinentry--read-point)
      (beginning-of-line)
      (while (looking-at ".*\n")        ;the input line finished
        (if (looking-at "\\([A-Z_]+\\) ?\\(.*\\)")
            (let ((command (match-string 1))
                  (string (pinentry--unescape-string (match-string 2))))
              (pcase command
                ((and set (guard (member set pinentry--set-label-commands)))
		 (when (> (length string) 0)
		   (let* ((symbol (intern (downcase (substring set 3))))
			  (entry (assq symbol pinentry--labels))
			  (label (decode-coding-string string 'utf-8)))
		     (if entry
			 (setcdr entry label)
		       (push (cons symbol label) pinentry--labels))))
		 (ignore-errors
		   (process-send-string process "OK\n")))
		("NOP"
		 (ignore-errors
		   (process-send-string process "OK\n")))
                ("GETPIN"
                 (let ((prompt
                        (or (cdr (assq 'desc pinentry--labels))
                            (cdr (assq 'prompt pinentry--labels))
                            ""))
		       (confirm (not (null (assq 'repeat pinentry--labels))))
                       entry)
                   (if (setq entry (assq 'error pinentry--labels))
                       (setq prompt (concat "Error: "
                                            (propertize
                                             (copy-sequence (cdr entry))
                                             'face 'error)
                                            "\n"
                                            prompt)))
                   (if (setq entry (assq 'title pinentry--labels))
                       (setq prompt (format "[%s] %s"
                                            (cdr entry) prompt)))
                   (if (string-match ":?[ \n]*\\'" prompt)
                       (setq prompt (concat
                                     (substring
                                      prompt 0 (match-beginning 0)) ": ")))
                   (let (passphrase escaped-passphrase encoded-passphrase)
                     (unwind-protect
                         (condition-case nil
                             (progn
                               (setq passphrase
				     (read-passwd prompt confirm))
                               (setq escaped-passphrase
                                     (pinentry--escape-string
                                      passphrase))
                               (setq encoded-passphrase (encode-coding-string
                                                         escaped-passphrase
                                                         'utf-8))
			       (ignore-errors
				 (pinentry--send-data
				  process encoded-passphrase)
				 (process-send-string process "OK\n")))
                           (error
			    (ignore-errors
			      (pinentry--send-error
			       process
			       pinentry--error-cancelled))))
                       (if passphrase
                           (clear-string passphrase))
                       (if escaped-passphrase
                           (clear-string escaped-passphrase))
                       (if encoded-passphrase
                           (clear-string encoded-passphrase))))
                   (setq pinentry--labels nil)))
                ("CONFIRM"
                 (let ((prompt
                        (or (cdr (assq 'desc pinentry--labels))
                            ""))
                       (buttons
                        (pinentry--labels-to-shortcuts
                         (list (cdr (assq 'ok pinentry--labels))
                               (cdr (assq 'notok pinentry--labels))
			       (cdr (assq 'cancel pinentry--labels)))))
                       entry)
                   (if (setq entry (assq 'error pinentry--labels))
                       (setq prompt (concat "Error: "
                                            (propertize
                                             (copy-sequence (cdr entry))
                                             'face 'error)
                                            "\n"
                                            prompt)))
                   (if (setq entry (assq 'title pinentry--labels))
                       (setq prompt (format "[%s] %s"
                                            (cdr entry) prompt)))
                   (if (remq nil buttons)
                       (progn
                         (setq prompt
                               (concat prompt " ("
                                       (mapconcat #'cdr (remq nil buttons)
                                                  ", ")
                                       ") "))
                         (condition-case nil
                             (let ((result (read-char prompt)))
                               (if (eq result (caar buttons))
				   (ignore-errors
				     (process-send-string process "OK\n"))
                                 (if (eq result (car (nth 1 buttons)))
				     (ignore-errors
				       (pinentry--send-error
					process
					pinentry--error-not-confirmed))
				   (ignore-errors
				     (pinentry--send-error
				      process
				      pinentry--error-cancelled)))))
                           (error
			    (ignore-errors
			      (pinentry--send-error
			       process
			       pinentry--error-cancelled)))))
                     (if (string-match "[ \n]*\\'" prompt)
                         (setq prompt (concat
                                       (substring
                                        prompt 0 (match-beginning 0)) " ")))
                     (if (condition-case nil
                             (y-or-n-p prompt)
                           (quit))
			 (ignore-errors
			   (process-send-string process "OK\n"))
		       (ignore-errors
			 (pinentry--send-error
			  process
			  pinentry--error-not-confirmed))))
                   (setq pinentry--labels nil)))
                (_ (ignore-errors
		     (pinentry--send-error
		      process
		      pinentry--error-not-implemented))))
              (forward-line)
              (setq pinentry--read-point (point))))))))

(defun pinentry--process-sentinel (process _status)
  "The process sentinel for Emacs server connections."
  ;; If this is a new client process, set the query-on-exit flag to nil
  ;; for this process (it isn't inherited from the server process).
  (when (and (eq (process-status process) 'open)
	     (process-query-on-exit-flag process))
    (push process pinentry--connection-process-list)
    (set-process-query-on-exit-flag process nil)
    (ignore-errors
      (process-send-string process "OK Your orders please\n")))
  ;; Kill the process buffer of the connection process.
  (when (and (not (process-contact process :server))
	     (eq (process-status process) 'closed))
    (when (buffer-live-p (process-buffer process))
      (kill-buffer (process-buffer process)))
    (setq pinentry--connection-process-list
	  (delq process pinentry--connection-process-list)))
  ;; Delete the associated connection file, if applicable.
  ;; Although there's no 100% guarantee that the file is owned by the
  ;; running Emacs instance, server-start uses server-running-p to check
  ;; for possible servers before doing anything, so it *should* be ours.
  (and (process-contact process :server)
       (eq (process-status process) 'closed)
       (ignore-errors
	 (delete-file (process-get process :server-file)))))

(provide 'pinentry)

;;; pinentry.el ends here
