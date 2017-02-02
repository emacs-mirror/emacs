;;; pinentry.el --- GnuPG Pinentry server implementation -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

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
;; To use, add "allow-emacs-pinentry" to "~/.gnupg/gpg-agent.conf",
;; reload the configuration with "gpgconf --reload gpg-agent", and
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
;; NOTE: As of August 2015, this feature requires newer versions of
;; GnuPG (2.1.5+) and Pinentry (0.9.5+).

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup pinentry nil
  "The Pinentry server"
  :version "25.1"
  :group 'external)

(defcustom pinentry-popup-prompt-window t
  "If non-nil, display multiline prompt in another window."
  :type 'boolean
  :group 'pinentry)

(defcustom pinentry-prompt-window-height 5
  "Number of lines used to display multiline prompt."
  :type 'integer
  :group 'pinentry)

(defvar pinentry-debug nil)
(defvar pinentry-debug-buffer nil)
(defvar pinentry--server-process nil)
(defvar pinentry--connection-process-list nil)

(defvar pinentry--labels nil)
(put 'pinentry-read-point 'permanent-local t)
(defvar pinentry--read-point nil)
(put 'pinentry--read-point 'permanent-local t)

(defvar pinentry--prompt-buffer nil)

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

(defvar pinentry-prompt-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "q" 'quit-window)
    keymap))

(define-derived-mode pinentry-prompt-mode special-mode "Pinentry"
  "Major mode for `pinentry--prompt-buffer'."
  (buffer-disable-undo)
  (setq truncate-lines t
	buffer-read-only t))

(defun pinentry--prompt (labels query-function &rest query-args)
  (let ((desc (cdr (assq 'desc labels)))
        (error (cdr (assq 'error labels)))
        (prompt (cdr (assq 'prompt labels))))
    (when (string-match "[ \n]*\\'" prompt)
      (setq prompt (concat
                    (substring
                     prompt 0 (match-beginning 0)) " ")))
    (when error
      (setq desc (concat "Error: " (propertize error 'face 'error)
                         "\n" desc)))
    (if (and desc pinentry-popup-prompt-window)
      (save-window-excursion
        (delete-other-windows)
	(unless (and pinentry--prompt-buffer
                     (buffer-live-p pinentry--prompt-buffer))
	  (setq pinentry--prompt-buffer (generate-new-buffer "*Pinentry*")))
	(if (get-buffer-window pinentry--prompt-buffer)
	    (delete-window (get-buffer-window pinentry--prompt-buffer)))
	(with-current-buffer pinentry--prompt-buffer
	  (let ((inhibit-read-only t)
		buffer-read-only)
	    (erase-buffer)
	    (insert desc))
	  (pinentry-prompt-mode)
	  (goto-char (point-min)))
	(if (> (window-height)
	       pinentry-prompt-window-height)
	    (set-window-buffer (split-window nil
                                             (- (window-height)
                                                pinentry-prompt-window-height))
			       pinentry--prompt-buffer)
	  (pop-to-buffer pinentry--prompt-buffer)
	  (if (> (window-height) pinentry-prompt-window-height)
	      (shrink-window (- (window-height)
                                pinentry-prompt-window-height))))
        (prog1 (apply query-function prompt query-args)
          (quit-window)))
      (apply query-function (concat desc "\n" prompt) query-args))))

;;;###autoload
(defun pinentry-start (&optional quiet)
  "Start a Pinentry service.

Once the environment is properly set, subsequent invocations of
the gpg command will interact with Emacs for passphrase input.

If the optional QUIET argument is non-nil, messages at startup
will not be shown."
  (interactive)
  (unless (featurep 'make-network-process '(:family local))
    (error "local sockets are not supported"))
  (if (process-live-p pinentry--server-process)
      (unless quiet
        (message "Pinentry service is already running"))
    (let* ((server-file (expand-file-name "pinentry" pinentry--socket-dir)))
      (server-ensure-safe-dir pinentry--socket-dir)
      ;; Delete the socket files made by previous server invocations.
      (ignore-errors
        (let (delete-by-moving-to-trash)
          (delete-file server-file)))
      (cl-letf (((default-file-modes) ?\700))
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
        (process-put pinentry--server-process :server-file server-file)))))

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
    (when pinentry-debug
      (with-current-buffer
          (or pinentry-debug-buffer
              (setq pinentry-debug-buffer (generate-new-buffer
                                           " *pinentry-debug*")))
        (goto-char (point-max))
        (insert input)))
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
                 (let ((confirm (not (null (assq 'repeat pinentry--labels))))
                       passphrase escaped-passphrase encoded-passphrase)
                   (unwind-protect
                       (condition-case err
                           (progn
                             (setq passphrase
                                   (pinentry--prompt
                                    pinentry--labels
                                    #'read-passwd confirm))
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
                          (message "GETPIN error %S" err)
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
                   (setq pinentry--labels nil))
                ("CONFIRM"
                 (let ((prompt
                        (or (cdr (assq 'prompt pinentry--labels))
                            "Confirm? "))
                       (buttons
                        (delq nil
                              (pinentry--labels-to-shortcuts
                               (list (cdr (assq 'ok pinentry--labels))
                                     (cdr (assq 'notok pinentry--labels))
                                     (cdr (assq 'cancel pinentry--labels))))))
                       entry)
                   (if buttons
                       (progn
                         (setq prompt
                               (concat prompt " ("
                                       (mapconcat #'cdr buttons
                                                  ", ")
                                       ") "))
                         (if (setq entry (assq 'prompt pinentry--labels))
                             (setcdr entry prompt)
                           (setq pinentry--labels (cons (cons 'prompt prompt)
                                                        pinentry--labels)))
                         (condition-case nil
                             (let ((result (pinentry--prompt pinentry--labels
                                                             #'read-char)))
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
                     (if (setq entry (assq 'prompt pinentry--labels))
                         (setcdr entry prompt)
                       (setq pinentry--labels (cons (cons 'prompt prompt)
                                                    pinentry--labels)))
                     (if (condition-case nil
                             (pinentry--prompt pinentry--labels #'y-or-n-p)
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
