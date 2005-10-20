;;; rcirc.el --- default, simple IRC client.

;; Copyright (C) 2005 Free Software Foundation, Inc.

;; Author: Ryan Yeske
;; URL: http://www.nongnu.org/rcirc
;; Keywords: comm

;; This file is not currently part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; rcirc is an Internet Relay Chat (IRC) client for Emacs

;; IRC is a form of instant communication over the Internet. It is
;; mainly designed for group (many-to-many) communication in
;; discussion forums called channels, but also allows one-to-one
;; communication.

;; Open a new irc connection with:
;; M-x irc RET

;;; Code:

(require 'ring)
(require 'time-date)
(eval-when-compile (require 'cl))

(defvar rcirc-server "irc.freenode.net"
  "The default server to connect to.")

(defvar rcirc-port 6667
  "The default port to connect to.")

(defvar rcirc-nick (user-login-name)
  "Your nick.")

(defvar rcirc-user-name (user-login-name)
  "Your user name sent to the server when connecting.")

(defvar rcirc-user-full-name (if (string= (user-full-name) "")
				 rcirc-user-name
			       (user-full-name))
  "The full name sent to the server when connecting.")

(defvar rcirc-startup-channels-alist nil
  "Alist of channels to join at startup.
Each element looks like (REGEXP . CHANNEL-LIST).")

(defvar rcirc-fill-flag t
  "*Non-nil means fill messages printed in channel buffers.")

(defvar rcirc-fill-column nil
  "*If non-nil, fill to this column, otherwise use value of `fill-column'.")

(defvar rcirc-fill-prefix nil
  "*Text to insert before filled lines.
If nil, calculate the prefix dynamically to line up text
underneath each nick.")

(defvar rcirc-ignore-channel-activity nil
  "If non-nil, ignore activity in this channel.")
(make-variable-buffer-local 'rcirc-ignore-channel-activity)

(defvar rcirc-ignore-all-activity-flag nil
  "*Non-nil means track activity, but do not display it in the modeline.")

(defvar rcirc-time-format "%H:%M "
  "*Describes how timestamps are printed.
Used as the first arg to `format-time-string'.")

(defvar rcirc-input-ring-size 1024
  "*Size of input history ring.")

(defvar rcirc-read-only-flag t
  "*Non-nil means make text in irc buffers read-only.")

(defvar rcirc-buffer-maximum-lines nil
  "*The maximum size in lines for rcirc buffers.
Channel buffers are truncated from the top to be no greater than this
number.	 If zero or nil, no truncating is done.")

(defvar rcirc-authinfo-file-name
  "~/.rcirc-authinfo"
  "File containing rcirc authentication passwords.
The file consists of a single list, with each element itself a
list with a SERVER-REGEXP string, a NICK-REGEXP string, a METHOD
and the remaining method specific ARGUMENTS.  The valid METHOD
symbols are `nickserv', `chanserv' and `bitlbee'.

The required ARGUMENTS for each METHOD symbol are:
  `nickserv': PASSWORD
  `chanserv': CHANNEL PASSWORD
  `bitlbee': PASSWORD

Example:
 ((\"freenode\" \"bob\" nickserv \"p455w0rd\")
  (\"freenode\" \"bob\" chanserv \"#bobland\" \"passwd99\")
  (\"bitlbee\" \"robert\" bitlbee \"sekrit\"))")

(defvar rcirc-auto-authenticate-flag (file-readable-p rcirc-authinfo-file-name)
  "*Non-nil means automatically send authentication string to server.
See also `rcirc-authinfo-file-name'.")

(defvar rcirc-print-hooks nil
  "Hook run after text is printed.
Called with 5 arguments, PROCESS, SENDER, RESPONSE, TARGET and TEXT.")

(defvar rcirc-prompt "%n> "
  "Prompt string to use in irc buffers.

The following replacements are made:
%n is your nick.
%s is the server.
%t is the buffer target, a channel or a user.

Setting this alone will not affect the prompt; 
use `rcirc-update-prompt' after changing this variable.")

(defvar rcirc-prompt-start-marker nil)
(defvar rcirc-prompt-end-marker nil)

(defvar rcirc-nick-table nil)

(defvar rcirc-activity nil
  "List of channels with unviewed activity.")

(defvar rcirc-activity-string ""
  "String displayed in modeline representing `rcirc-activity'.")
(put 'rcirc-activity-string 'risky-local-variable t)

(defvar rcirc-process nil
  "The server process associated with this buffer.")

(defvar rcirc-target nil
  "The channel or user associated with this buffer.")

(defvar rcirc-channels nil
  "Joined channels.")

(defvar rcirc-private-chats nil
  "Private chats open.")

(defvar rcirc-urls nil
  "List of urls seen in the current buffer.")

(defvar rcirc-keepalive-seconds 60
  "Number of seconds between keepalive pings.")


(defun rcirc-version (&optional here)
  "Return rcirc version string.
If optional argument HERE is non-nil, insert string at point."
  (interactive "P")
  (let ((version "rcirc.el 0.9 $Revision: 1.2 $"))
    (if here
	(insert version)
      (if (interactive-p)
	  (message "%s" version)
	version))))

(defvar rcirc-startup-channels nil)
;;;###autoload
(defun rcirc (&optional server port nick channels)
  "Connect to IRC.

If any of the the optional SERVER, PORT, NICK or CHANNELS are not
supplied, they are taken from the variables `rcirc-server',
`rcirc-port', `rcirc-nick', and `rcirc-startup-channels',
respectively."
  (interactive (list (read-string "IRC Server: " rcirc-server)
		     (read-string "IRC Port: " (number-to-string rcirc-port))
		     (read-string "IRC Nick: " rcirc-nick)))
  (or server (setq server rcirc-server))
  (or port (setq port rcirc-port))
  (or nick (setq nick rcirc-nick))
  (or channels
      (setq channels
	    (if (interactive-p)
		(delete ""
			(split-string
			 (read-string "Channels: "
				     (mapconcat 'identity
						(rcirc-startup-channels server)
						" "))
			 "[, ]+"))
	      (rcirc-startup-channels server))))
  (or global-mode-string (setq global-mode-string '("")))
  (and (not (memq 'rcirc-activity-string global-mode-string))
       (setq global-mode-string
	     (append global-mode-string '(rcirc-activity-string))))
  (add-hook 'window-configuration-change-hook 'rcirc-update-activity)
  (rcirc-connect server port nick rcirc-user-name rcirc-user-full-name
		 channels))

;;;###autoload
(defalias 'irc 'rcirc)


(defvar rcirc-process-output nil)
(defvar rcirc-last-buffer nil)
(defvar rcirc-topic nil)
(defvar rcirc-keepalive-timer nil)
(make-variable-buffer-local 'rcirc-topic)
(defun rcirc-connect (server port nick user-name full-name startup-channels)
  "Return a connection to SERVER on PORT.

User will identify using the values of NICK, USER-NAME and
FULL-NAME.  The variable list of channel names in
STARTUP-CHANNELS will automatically be joined on startup."
  (save-excursion
    (message "Connecting to %s..." server)
    (let* ((inhibit-eol-conversion)
           (port-number (if (stringp port)
                            (string-to-number port)
                          port))
           (process (open-network-stream server nil server port-number)))
      ;; set up process
      (set-process-coding-system process 'raw-text 'raw-text)
      (set-process-filter process 'rcirc-filter)
      (switch-to-buffer (concat "*" (process-name process) "*"))
      (set-process-buffer process (current-buffer))
      (set-process-sentinel process 'rcirc-sentinel)
      (rcirc-mode process nil)
      (make-local-variable 'rcirc-nick-table)
      (setq rcirc-nick-table (make-hash-table :test 'equal))
      (make-local-variable 'rcirc-server)
      (setq rcirc-server server)
      (make-local-variable 'rcirc-nick)
      (setq rcirc-nick nick)
      (make-local-variable 'rcirc-process-output)
      (setq rcirc-process-output nil)
      (make-local-variable 'rcirc-last-buffer)
      (setq rcirc-last-buffer (current-buffer))
      (make-local-variable 'rcirc-channels)
      (setq rcirc-channels nil)
      (make-local-variable 'rcirc-private-chats)
      (setq rcirc-private-chats nil)
      (make-local-variable 'rcirc-startup-channels)
      (setq rcirc-startup-channels startup-channels)

      ;; identify
      (rcirc-send-string process (concat "NICK " nick))
      (rcirc-send-string process (concat "USER " user-name
                                      " hostname servername :"
                                      full-name))

      ;; setup ping timer if necessary
      (unless rcirc-keepalive-timer
        (setq rcirc-keepalive-timer
              (run-at-time 0 rcirc-keepalive-seconds 'rcirc-keepalive)))

      (message "Connecting to %s...done" server)

      ;; return process object
      process)))

(defun rcirc-keepalive ()
  "Send keep alive pings to active rcirc processes."
  (if (rcirc-process-list)
      (mapc (lambda (process)
              (with-current-buffer (process-buffer process)
                (rcirc-send-string process (concat "PING " rcirc-server))))
            (rcirc-process-list))
    (cancel-timer rcirc-keepalive-timer)
    (setq rcirc-keepalive-timer nil)))

(defvar rcirc-log-buffer "*rcirc log*")
(defvar rcirc-log-p nil
  "If non-nil, write information to `rcirc-log-buffer'.")
(defun rcirc-log (process text)
  "Add an entry to the debug log including PROCESS and TEXT.
Debug text is written to `rcirc-log-buffer' if `rcirc-log-p' is
non-nil."
  (when rcirc-log-p
    (save-excursion
      (save-window-excursion
        (set-buffer (get-buffer-create rcirc-log-buffer))
        (goto-char (point-max))
        (insert (concat
                 "["
                 (format-time-string "%Y-%m-%dT%T ") (process-name process)
                 "] "
                 text))))))
               
(defvar rcirc-sentinel-hooks nil
  "Hook functions called when the process sentinel is called.
Functions are called with PROCESS and SENTINEL arguments.")

(defun rcirc-sentinel (process sentinel)
  "Called when PROCESS receives SENTINEL."
  (let ((sentinel (replace-regexp-in-string "\n" "" sentinel)))
    (rcirc-log process (format "SENTINEL: %S %S\n" process sentinel))
    (with-current-buffer (process-buffer process)
      (dolist (target (append rcirc-channels
                              rcirc-private-chats
                              (list (current-buffer))))
        (rcirc-print process "rcirc.el" "ERROR" target
                     (format "%s: %s (%S)"
                             (process-name process)
                             sentinel
                             (process-status process)) t)
	;; remove the prompt from buffers
	(with-current-buffer (if (eq target (current-buffer))
				 (current-buffer)
			       (rcirc-get-buffer process target))
	  (let ((inhibit-read-only t))
	    (delete-region rcirc-prompt-start-marker
			   rcirc-prompt-end-marker)))))
    (run-hook-with-args 'rcirc-sentinel-hooks process sentinel)))

(defun rcirc-process-list ()
  "Return a list of rcirc processes."
  (let (ps)
    (mapc (lambda (p)
            (when (process-buffer p)
              (with-current-buffer (process-buffer p)
                (when (eq major-mode 'rcirc-mode)
                  (setq ps (cons p ps))))))
          (process-list))
    ps))

(defvar rcirc-receive-message-hooks nil
  "Hook functions run when a message is recieved from server.
Function is called with PROCESS COMMAND SENDER ARGS and LINE.")
(defun rcirc-filter (process output)
  "Called when PROCESS receives OUTPUT."
  (rcirc-log process output)
  (with-current-buffer (process-buffer process)
    (setq rcirc-process-output (concat rcirc-process-output output))
    (when (= (aref rcirc-process-output
                   (1- (length rcirc-process-output))) ?\n)
      (mapc (lambda (line)
              (rcirc-process-server-response process line))
            (delete "" (split-string rcirc-process-output "[\n\r]")))
      (setq rcirc-process-output nil))))

(defvar rcirc-trap-errors nil)
(defun rcirc-process-server-response (process text)
  (if rcirc-trap-errors
      (condition-case err
          (rcirc-process-server-response-1 process text)
        (error
         (rcirc-print process "RCIRC" "ERROR" nil
                      (format "rcirc: error processing: \"%s\" %s" text err))))
    (rcirc-process-server-response-1 process text)))

(defun rcirc-process-server-response-1 (process text)
  (if (string-match "^\\(:\\([^ ]+\\) \\)?\\([^ ]+\\) \\(.+\\)$" text)
      (let* ((sender (match-string 2 text))
             (cmd (match-string 3 text))
             (args (match-string 4 text))
             (handler (intern-soft (concat "rcirc-handler-" cmd))))
        (string-match "^\\([^:]*\\):?\\(.+\\)?$" args)
        (let* ((args1 (match-string 1 args))
               (args2 (match-string 2 args))
               (args (append (delete "" (split-string args1 " "))
                             (list args2))))
        (if (not (fboundp handler))
            (rcirc-handler-generic process cmd sender args text)
          (funcall handler process sender args text))
        (run-hook-with-args 'rcirc-receive-message-hooks
                            process cmd sender args text)))
    (message "UNHANDLED: %s" text)))

(defun rcirc-handler-generic (process command sender args text)
  "Generic server response handler."
  (rcirc-print process sender command nil
               (mapconcat 'identity (cdr args) " ")))

(defun rcirc-send-string (process string)
  "Send PROCESS a STRING plus a newline."
  (let ((string (concat (encode-coding-string string
                                              buffer-file-coding-system)
                        "\n")))
    (rcirc-log process string)
    (process-send-string process string)))

(defun rcirc-server (process)
  "Return PROCESS server, given by the 001 response."
  (with-current-buffer (process-buffer process)
    rcirc-server))

(defun rcirc-nick (process)
  "Return PROCESS nick."
  (with-current-buffer (process-buffer process)
    rcirc-nick))

(defvar rcirc-max-message-length 450
  "Messages longer than this value will be split.")

(defun rcirc-send-message (process target message &optional noticep)
  "Send TARGET associated with PROCESS a privmsg with text MESSAGE.
If NOTICEP is non-nil, send a notice instead of privmsg."
  ;; max message length is 512 including CRLF
  (let* ((response (if noticep "NOTICE" "PRIVMSG"))
         (oversize (> (length message) rcirc-max-message-length))
         (text (if oversize
                   (substring message 0 rcirc-max-message-length)
                 message))
         (text (if (string= text "")
                   " "
                 text))
         (more (if oversize
                   (substring message rcirc-max-message-length))))
    (rcirc-print process (rcirc-nick process) response target text)
    (rcirc-send-string process (concat response " " target " :" text))
    (if more
        (rcirc-send-message process target more noticep))))

(defvar rcirc-input-ring nil)
(defvar rcirc-input-ring-index 0)
(defun rcirc-prev-input-string (arg)
  (ring-ref rcirc-input-ring (+ rcirc-input-ring-index arg)))

(defun rcirc-insert-prev-input (arg)
  (interactive "p")
  (when (<= rcirc-prompt-end-marker (point))
    (delete-region rcirc-prompt-end-marker (point-max))
    (insert (rcirc-prev-input-string 0))
    (setq rcirc-input-ring-index (1+ rcirc-input-ring-index))))

(defun rcirc-insert-next-input (arg)
  (interactive "p")
  (when (<= rcirc-prompt-end-marker (point))
    (delete-region rcirc-prompt-end-marker (point-max))
    (setq rcirc-input-ring-index (1- rcirc-input-ring-index))
    (insert (rcirc-prev-input-string -1))))

(defvar rcirc-nick-completions nil)
(defvar rcirc-nick-completion-start-offset nil)
(defun rcirc-complete-nick ()
  "Cycle through nick completions from list of nicks in channel."
  (interactive)
  (if (eq last-command 'rcirc-complete-nick)
      (setq rcirc-nick-completions
            (append (cdr rcirc-nick-completions)
                    (list (car rcirc-nick-completions))))
    (setq rcirc-nick-completion-start-offset
          (- (save-excursion
               (if (re-search-backward " " rcirc-prompt-end-marker t)
                   (1+ (point))
                 rcirc-prompt-end-marker))
             rcirc-prompt-end-marker))
    (setq rcirc-nick-completions
          (let ((completion-ignore-case t))
            (all-completions 
	     (buffer-substring 
	      (+ rcirc-prompt-end-marker
		 rcirc-nick-completion-start-offset)
	      (point))
	     (mapcar (lambda (x) (cons x nil))
		     (rcirc-channel-nicks rcirc-process
					  (rcirc-buffer-target)))))))
  (let ((completion (car rcirc-nick-completions)))
    (when completion
      (delete-region (+ rcirc-prompt-end-marker 
                        rcirc-nick-completion-start-offset)
                     (point))
      (insert (concat completion
                      (if (= (+ rcirc-prompt-end-marker 
                                rcirc-nick-completion-start-offset)
                             rcirc-prompt-end-marker)
                          ": "))))))

(defun rcirc-buffer-target (&optional buffer)
  "Return the name of target for BUFFER.
If buffer is nil, return the target of the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    rcirc-target))

(defvar rcirc-mode-map (make-sparse-keymap)
  "Keymap for rcirc mode.")

(define-key rcirc-mode-map (kbd "RET") 'rcirc-send-input)
(define-key rcirc-mode-map (kbd "M-p") 'rcirc-insert-prev-input)
(define-key rcirc-mode-map (kbd "M-n") 'rcirc-insert-next-input)
(define-key rcirc-mode-map (kbd "TAB") 'rcirc-complete-nick)
(define-key rcirc-mode-map (kbd "C-c C-b") 'rcirc-browse-url)
(define-key rcirc-mode-map (kbd "C-c C-c") 'rcirc-edit-multiline)
(define-key rcirc-mode-map (kbd "C-c C-j") 'rcirc-cmd-join)
(define-key rcirc-mode-map (kbd "C-c C-k") 'rcirc-cmd-kick)
(define-key rcirc-mode-map (kbd "C-c C-l") 'rcirc-cmd-list)
(define-key rcirc-mode-map (kbd "C-c C-d") 'rcirc-cmd-mode)
(define-key rcirc-mode-map (kbd "C-c C-m") 'rcirc-cmd-msg)
(define-key rcirc-mode-map (kbd "C-c C-r") 'rcirc-cmd-nick) ; rename
(define-key rcirc-mode-map (kbd "C-c C-o") 'rcirc-cmd-oper)
(define-key rcirc-mode-map (kbd "C-c C-p") 'rcirc-cmd-part)
(define-key rcirc-mode-map (kbd "C-c C-q") 'rcirc-cmd-query)
(define-key rcirc-mode-map (kbd "C-c C-t") 'rcirc-cmd-topic)
(define-key rcirc-mode-map (kbd "C-c C-n") 'rcirc-cmd-names)
(define-key rcirc-mode-map (kbd "C-c C-w") 'rcirc-cmd-whois)
(define-key rcirc-mode-map (kbd "C-c C-x") 'rcirc-cmd-quit)
(define-key rcirc-mode-map (kbd "C-c TAB") ; C-i
  'rcirc-toggle-ignore-channel-activity)
(define-key rcirc-mode-map (kbd "C-c C-s") 'rcirc-switch-to-server-buffer)
(define-key rcirc-mode-map (kbd "C-c C-a") 'rcirc-jump-to-first-unread-line)

(define-key global-map (kbd "C-c `") 'rcirc-next-active-buffer)
(define-key global-map (kbd "C-c C-@") 'rcirc-next-active-buffer)
(define-key global-map (kbd "C-c C-SPC") 'rcirc-next-active-buffer)

(defvar rcirc-mode-hook nil
  "Hook run when setting up rcirc buffer.")

(defun rcirc-mode (process target)
  "Major mode for irc channel buffers.

\\{rcirc-mode-map}"
  (kill-all-local-variables)
  (use-local-map rcirc-mode-map)
  (setq mode-name "rcirc")
  (setq major-mode 'rcirc-mode)

  (make-local-variable 'rcirc-input-ring)
  (setq rcirc-input-ring (make-ring rcirc-input-ring-size))
  (make-local-variable 'rcirc-process)
  (setq rcirc-process process)
  (make-local-variable 'rcirc-target)
  (setq rcirc-target target)
  (make-local-variable 'rcirc-urls)
  (setq rcirc-urls nil)
  (setq use-hard-newlines t)
  (when (rcirc-channel-p rcirc-target)
    (setq header-line-format 'rcirc-topic))

  ;; setup the prompt and markers
  (make-local-variable 'rcirc-prompt-start-marker)
  (setq rcirc-prompt-start-marker (make-marker))
  (set-marker rcirc-prompt-start-marker (point-max))
  (make-local-variable 'rcirc-prompt-end-marker)
  (setq rcirc-prompt-end-marker (make-marker))
  (set-marker rcirc-prompt-end-marker (point-max))
  (rcirc-update-prompt)
  (goto-char rcirc-prompt-end-marker)
  (make-local-variable 'overlay-arrow-position)
  (setq overlay-arrow-position (make-marker))
  (set-marker overlay-arrow-position nil)

  (run-hooks 'rcirc-mode-hook))

(defmacro with-rcirc-process-buffer (process &rest body)
  (declare (indent 1) (debug t))
  `(with-current-buffer (process-buffer ,process)
     ,@body))

(defun rcirc-update-prompt ()
  "Reset the prompt string in the current buffer."
  (let ((inhibit-read-only t)
        (prompt (or rcirc-prompt "")))
    (mapc (lambda (rep)
            (setq prompt 
		  (replace-regexp-in-string (car rep) (cdr rep) prompt)))
          (list (cons "%n" (with-rcirc-process-buffer rcirc-process
			     rcirc-nick))
                (cons "%s" (with-rcirc-process-buffer rcirc-process 
			     rcirc-server))
                (cons "%t" (or rcirc-target ""))))
    (save-excursion
      (delete-region rcirc-prompt-start-marker rcirc-prompt-end-marker)
      (goto-char rcirc-prompt-start-marker)
      (let ((start (point)))
        (insert-before-markers prompt)
        (set-marker rcirc-prompt-start-marker start)
        (when (not (zerop (- rcirc-prompt-end-marker
                             rcirc-prompt-start-marker)))
          (add-text-properties rcirc-prompt-start-marker
                               rcirc-prompt-end-marker
                               (list 'face 'rcirc-prompt-face
                                     'read-only t 'field t
                                     'front-sticky t 'rear-nonsticky t)))))))

(defun rcirc-channel-p (target)
  "Return t if TARGET is a channel name."
  (and target
       (not (zerop (length target)))
       (or (eq (aref target 0) ?#)
           (eq (aref target 0) ?&))))

(defun rcirc-kill-buffer-hook ()
  "Part the channel when killing an rcirc buffer."
  (when (eq major-mode 'rcirc-mode)
    (rcirc-clear-activity (current-buffer))
    (when (and rcirc-process
               (eq (process-status rcirc-process) 'open))
      (if (rcirc-channel-p rcirc-target)
          (rcirc-cmd-part "" rcirc-process rcirc-target)
        ;; remove target from privchat list
        (when rcirc-target
          (let ((target (downcase rcirc-target)))
            (with-rcirc-process-buffer rcirc-process
              (setq rcirc-private-chats
		    (delete target rcirc-private-chats)))))))))
(add-hook 'kill-buffer-hook 'rcirc-kill-buffer-hook)

(defun rcirc-get-buffer-name (process target)
  "Return buffer name based on PROCESS and TARGET."
  (concat (and target (downcase target)) "@" (process-name process)))

(defun rcirc-get-buffer (process target &optional error)
  "Return the buffer associated with the PROCESS and TARGET.
If TARGET is nil and ERROR is nil, return the process buffer."
  (let ((buffer (and target 
                     (get-buffer (rcirc-get-buffer-name process target)))))
    (if (and buffer (buffer-live-p buffer))
        buffer
      (if error
          (error "Buffer associated with %s does not exist" target)
        (process-buffer process)))))

(defun rcirc-get-buffer-create (process target)
  "Return the buffer named associated with the PROCESS and TARGET.
Create the buffer if it doesn't exist.  If TARGET is nil, return
the process buffer."
  (with-current-buffer (process-buffer process)
    (if (not target)
        (current-buffer)
      (let ((target (downcase target)))
        ;; add private chats to list.  we dont add channels here, they
        ;; are managed by the join/part/quit handlers
        (when (and (not (rcirc-channel-p target))
                   (not (member target rcirc-private-chats)))
          (with-rcirc-process-buffer process
            (setq rcirc-private-chats (cons target rcirc-private-chats))))
        ;; create and setup a buffer, or return the existing one
        (let ((bufname (rcirc-get-buffer-name process target)))
          (with-current-buffer (get-buffer-create bufname)
            (if (or (not rcirc-process)
                    (not (equal (process-status rcirc-process) 'open)))
                (rcirc-mode process target)
              (setq rcirc-target target))
            (current-buffer)))))))

(defun rcirc-send-input ()
  "Send input to target associated with the current buffer."
  (interactive)
  (if (not (eq (process-status rcirc-process) 'open))
      (error "Network connection to %s is not open" 
             (process-name rcirc-process))
    ;; update last buffer
    (rcirc-set-last-buffer rcirc-process (current-buffer))
    (if (< (point) rcirc-prompt-end-marker)
        ;; copy the line down to the input area
        (progn
          (forward-line 0)
          (let ((start (if (eq (point) (point-min))
                           (point)
                         (if (get-text-property (1- (point)) 'hard)
                             (point)
                           (previous-single-property-change (point) 'hard))))
                (end (next-single-property-change (1+ (point)) 'hard)))
            (goto-char (point-max))
            (insert (replace-regexp-in-string
                     "\n\\s-+" " "
                     (buffer-substring-no-properties start end)))))
      ;; assume text has been read
      (when (marker-position overlay-arrow-position)
	(set-marker overlay-arrow-position nil))
      ;; process input
      (goto-char (point-max))
      (let ((target (rcirc-buffer-target))
            (start rcirc-prompt-end-marker))
        (when (not (equal 0 (- (point) start)))
          ;; delete a trailing newline
          (when (eq (point) (point-at-bol))
            (delete-backward-char 1))
          (let ((input (buffer-substring-no-properties
                        rcirc-prompt-end-marker (point))))
            ;; process a /cmd
            (if (string-match "^/\\([^ ]+\\) ?\\(.*\\)$" input)
                (let* ((command (match-string 1 input))
                       (fun (intern-soft (concat "rcirc-cmd-" command)))
                       (args (match-string 2 input)))
                  (newline)
                  (with-current-buffer (current-buffer)
                    (delete-region rcirc-prompt-end-marker (point))
                    (if (string= command "me")
                        (rcirc-print rcirc-process (rcirc-nick rcirc-process) 
                                     "ACTION" target args)
                      (rcirc-print rcirc-process (rcirc-nick rcirc-process) 
                                   "COMMAND" target input))
                    (set-marker rcirc-prompt-end-marker (point))
                    (if (fboundp fun)
                        (funcall fun args rcirc-process target)
                      (rcirc-send-string rcirc-process
					 (concat command " " args)))))
              ;; send message to server
              (if (not rcirc-target)
                  (message "Not joined")
                (delete-region rcirc-prompt-end-marker (point))
                (mapc (lambda (message)
                        (rcirc-send-message rcirc-process target message))
                      (split-string input "\n"))))
            ;; add to input-ring
            (save-excursion
              (ring-insert rcirc-input-ring input)
              (setq rcirc-input-ring-index 0))))))))

(defvar rcirc-parent-buffer nil)
(defvar rcirc-window-configuration nil)
(defun rcirc-edit-multiline ()
  "Move current edit to a dedicated buffer."
  (interactive)
  (let ((pos (1+ (- (point) rcirc-prompt-end-marker))))
    (goto-char (point-max))
    (let ((text (buffer-substring rcirc-prompt-end-marker (point)))
          (parent (buffer-name))
          (process rcirc-process))
      (delete-region rcirc-prompt-end-marker (point))
      (setq rcirc-window-configuration (current-window-configuration))
      (pop-to-buffer (concat "*multiline " parent "*"))
      (rcirc-multiline-edit-mode)
      (setq rcirc-parent-buffer parent)
      (setq rcirc-process process)
      (insert text)
      (and (> pos 0) (goto-char pos)))))

(define-derived-mode rcirc-multiline-edit-mode
  text-mode "rcirc multi"
  "Major mode for multiline edits
\\{rcirc-multiline-edit-mode-map}"
  (make-local-variable 'rcirc-parent-buffer)
  (make-local-variable 'rcirc-process))

(define-key rcirc-multiline-edit-mode-map
  (kbd "C-c C-c") 'rcirc-multiline-edit-submit)
(define-key rcirc-multiline-edit-mode-map
  (kbd "C-x C-s") 'rcirc-multiline-edit-submit)
(define-key rcirc-multiline-edit-mode-map
  (kbd "C-c C-k") 'rcirc-multiline-edit-cancel)
(define-key rcirc-multiline-edit-mode-map
  (kbd "ESC ESC ESC") 'rcirc-multiline-edit-cancel)

(defun rcirc-multiline-edit-submit ()
  "Send the text in buffer back to parent buffer."
  (interactive)
  (assert (and (eq major-mode 'rcirc-multiline-edit-mode)))
  (assert rcirc-parent-buffer)
  (let ((text (buffer-substring (point-min) (point-max)))
        (buffer (current-buffer))
        (pos (point)))
    (set-buffer rcirc-parent-buffer)
    (goto-char (point-max))
    (insert text)
    (goto-char (+ rcirc-prompt-end-marker (1- pos)))
    (kill-buffer buffer)
    (set-window-configuration rcirc-window-configuration)))

(defun rcirc-multiline-edit-cancel ()
  "Cancel the multiline edit."
  (interactive)
  (assert (and (eq major-mode 'rcirc-multiline-edit-mode)))
  (kill-buffer (current-buffer))
  (set-window-configuration rcirc-window-configuration))

(defun rcirc-last-buffer (process)
  "Return the last working buffer for PROCESS.
Used for displaying messages that don't have an explicit destination."
  (with-current-buffer (process-buffer process)
    (or (and rcirc-last-buffer
             (buffer-live-p rcirc-last-buffer)
             rcirc-last-buffer)
        (current-buffer))))

(defun rcirc-set-last-buffer (process buffer)
  "Set the last working buffer for PROCESS to BUFFER."
  (with-current-buffer (process-buffer process)
    (setq rcirc-last-buffer buffer)))

(defun rcirc-format-response-string (process sender response target text)
  (concat (when rcirc-time-format
            (format-time-string rcirc-time-format (current-time)))
          (cond ((or (string= response "PRIVMSG")
                     (string= response "NOTICE")
                     (string= response "ACTION"))
                 (let (first middle end)
                   (cond ((string= response "PRIVMSG")
                          (setq first "<" middle "> "))
                         ((string= response "NOTICE")
                          (setq first "-" middle "- "))
                         (t
                          (setq first "[" middle " " end "]")))
                   (concat first 
                           (rcirc-facify (rcirc-user-nick sender)
                                         (if (string= sender
                                                      (rcirc-nick process))
                                             'rcirc-my-nick-face
                                           'rcirc-other-nick-face))
                           middle
                           (rcirc-mangle-text process text)
                           end)))
                ((string= response "COMMAND")
                 text)
                ((string= response "ERROR")
                 (propertize text 'face 'font-lock-warning-face))
                (t
                 (rcirc-mangle-text
                  process
                  (rcirc-facify
                   (concat "*** "
			   (when (not (string= sender (rcirc-server process)))
			     (concat (rcirc-user-nick sender) " "))
			   (when (zerop (string-to-number response))
			     (concat response " "))
                           (when (and target (not (string= target rcirc-target)))
                             (concat target " "))
                           text)
                   'rcirc-server-face))))))

(defvar rcirc-activity-type nil)
(make-variable-buffer-local 'rcirc-activity-type)
(defun rcirc-print (process sender response target text &optional activity)
  "Print TEXT in the buffer associated with TARGET.
Format based on SENDER and RESPONSE.  If ACTIVITY is non-nil,
record activity."
  (let* ((buffer (cond ((bufferp target) 
                        target)
                       ((not target)
                        (rcirc-last-buffer process))
                       ((not (rcirc-channel-p target)) 
                        (rcirc-get-buffer-create process target))
                       ((rcirc-get-buffer process target))
                       (t (process-buffer process))))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (let ((moving (= (point) rcirc-prompt-end-marker))
            (old-point (point-marker))
            (fill-start (marker-position rcirc-prompt-start-marker)))

        (unless (string= sender (rcirc-nick process))
	  ;; only decode text from other senders, not ours
	  (setq text (decode-coding-string text buffer-file-coding-system))
	  ;; mark the line with overlay arrow
	  (unless (or (marker-position overlay-arrow-position)
		      (get-buffer-window (current-buffer)))
	    (set-marker overlay-arrow-position 
			(marker-position rcirc-prompt-start-marker))))

        ;; temporarily set the marker insertion-type because
        ;; insert-before-markers results in hidden text in new buffers
        (goto-char rcirc-prompt-start-marker)
        (set-marker-insertion-type rcirc-prompt-start-marker t)
        (set-marker-insertion-type rcirc-prompt-end-marker t)
        (insert
	 (rcirc-format-response-string process sender response target text)
	 (propertize "\n" 'hard t))
        (set-marker-insertion-type rcirc-prompt-start-marker nil)
        (set-marker-insertion-type rcirc-prompt-end-marker nil)

        ;; fill the text we just inserted, maybe
        (when (and rcirc-fill-flag
		   (not (string= response "372"))) ;/motd
          (let ((fill-prefix
                 (or rcirc-fill-prefix
                     (make-string
                      (+ (if rcirc-time-format
                             (length (format-time-string 
                                      rcirc-time-format))
                           0)
                         (cond ((or (string= response "PRIVMSG")
				    (string= response "NOTICE"))
				(+ (length (rcirc-user-nick sender))
				   2))	; <>
			       ((string= response "ACTION")
				(+ (length (rcirc-user-nick sender))
				   1))		; [
			       (t 3))		; ***
                         1)
                      ? )))
                (fill-column (or rcirc-fill-column fill-column)))
            (fill-region fill-start rcirc-prompt-start-marker 'left t)))

        ;; truncate buffer if it is very long
        (save-excursion
          (when (and rcirc-buffer-maximum-lines
                     (> rcirc-buffer-maximum-lines 0)
                     (= (forward-line (- rcirc-buffer-maximum-lines)) 0))
            (delete-region (point-min) (point))))

        ;; set inserted text to be read-only
        (when rcirc-read-only-flag
          (put-text-property rcirc-prompt-start-marker fill-start 'read-only t)
          (let ((inhibit-read-only t))
            (put-text-property rcirc-prompt-start-marker fill-start 
			       'front-sticky t)
            (put-text-property (1- (point)) (point) 'rear-nonsticky t)))

        ;; set the window point for buffers show in windows
        (walk-windows (lambda (w)
                        (unless (eq (selected-window) w)
                          (when (and (eq (current-buffer) 
					 (window-buffer w))
                                     (>= (window-point w) 
					 rcirc-prompt-end-marker))
                            (set-window-point w (point-max)))))
                      nil t)

        ;; restore the point
        (goto-char (if moving rcirc-prompt-end-marker old-point))

        ;; flush undo (can we do something smarter here?)
	(buffer-disable-undo)
	(buffer-enable-undo))

      ;; record modeline activity
      (when activity
        (let ((nick-match
	       (string-match (concat "\\b"
				     (regexp-quote (rcirc-nick process))
				     "\\b")
			     text)))
          (when (or (not rcirc-ignore-channel-activity)
                    ;; always notice when our nick is mentioned, even
                    ;; if ignoring channel activity
                    nick-match)
            (rcirc-record-activity
	     (current-buffer) 
	     (when (or nick-match (not (rcirc-channel-p rcirc-target)))
	       'nick)))))

      (run-hook-with-args 'rcirc-print-hooks
                          process sender response target text))))

(defun rcirc-startup-channels (server)
  "Return the list of startup channels for server."
  (let (channels)
    (dolist (i rcirc-startup-channels-alist)
      (if (string-match (car i) server)
          (setq channels (append channels (cdr i)))))
    channels))

(defun rcirc-join-channels (process channels)
  "Join CHANNELS."
  (save-window-excursion
    (mapc (lambda (channel)
            (with-current-buffer (process-buffer process)
              (let (rcirc-last-buffer)  ; make sure /join text is
                                        ; printed in server buffer
                (rcirc-print process (rcirc-nick process) "COMMAND"
                             nil (concat "/join " channel)))
              (rcirc-cmd-join channel process)))
          channels)))

;;; nick management
(defun rcirc-user-nick (user)
  "Return the nick from USER.  Remove any non-nick junk."
  (if (string-match "^[@%+]?\\([^! ]+\\)!?" (or user ""))
      (match-string 1 user)
    user))

(defun rcirc-user-non-nick (user)
  "Return the non-nick portion of USER."
  (if (string-match "^[@+]?[^! ]+!?\\(.*\\)" (or user ""))
      (match-string 1 user)
    user))

(defun rcirc-nick-channels (process nick)
  "Return list of channels for NICK."
  (let ((nick (rcirc-user-nick nick)))
    (with-current-buffer (process-buffer process)
      (mapcar (lambda (x) (car x))
              (gethash nick rcirc-nick-table)))))

(defun rcirc-put-nick-channel (process nick channel)
  "Add CHANNEL to list associated with NICK."
  (with-current-buffer (process-buffer process)
    (let* ((nick (rcirc-user-nick nick))
           (chans (gethash nick rcirc-nick-table))
           (record (assoc channel chans)))
      (if record
          (setcdr record (current-time))
        (puthash nick (cons (cons channel (current-time))
                            chans)
                 rcirc-nick-table)))))

(defun rcirc-nick-remove (process nick)
  "Remove NICK from table."
  (with-current-buffer (process-buffer process)
    (remhash nick rcirc-nick-table)))

(defun rcirc-remove-nick-channel (process nick channel)
  "Remove the CHANNEL from list associated with NICK."
  (with-current-buffer (process-buffer process)
    (let* ((nick (rcirc-user-nick nick))
           (chans (gethash nick rcirc-nick-table))
           (newchans (assq-delete-all channel chans)))
      (if newchans
          (puthash nick newchans rcirc-nick-table)
        (remhash nick rcirc-nick-table)))))

(defun rcirc-channel-nicks (process channel)
  "Return the list of nicks in CHANNEL sorted by last activity."
  (with-current-buffer (process-buffer process)
    (let (nicks)
      (maphash
       (lambda (k v)
         (let ((record (assoc channel v)))
           (if record
               (setq nicks (cons (cons k (cdr record)) nicks)))))
       rcirc-nick-table)
      (mapcar (lambda (x) (car x))
              (sort nicks (lambda (x y) (time-less-p (cdr y) (cdr x))))))))

;;; activity tracking
(or (assq 'rcirc-ignore-channel-activity minor-mode-alist)
    (setq minor-mode-alist
          (cons '(rcirc-ignore-channel-activity " Ignore") minor-mode-alist)))

(defun rcirc-toggle-ignore-channel-activity (&optional all)
  "Toggle the value of `rcirc-ignore-channel-activity'.
If ALL is non-nil, instead toggle the value of
`rcirc-ignore-all-activity-flag'."
  (interactive "P")
  (if all
      (progn
        (setq rcirc-ignore-all-activity-flag
              (not rcirc-ignore-all-activity-flag))
        (message (concat "Global activity "
                         (if rcirc-ignore-all-activity-flag
                             "hidden"
                           "displayed")))
        (rcirc-update-activity-string))
    (setq rcirc-ignore-channel-activity
          (not rcirc-ignore-channel-activity)))
  (force-mode-line-update))

(defvar rcirc-switch-to-buffer-function 'switch-to-buffer
  "Function to use when switching buffers.
Possible values are `switch-to-buffer', `pop-to-buffer', and
`display-buffer'.")

(defun rcirc-switch-to-server-buffer ()
  "Switch to the server buffer associated with current channel buffer."
  (interactive)
  (funcall rcirc-switch-to-buffer-function (process-buffer rcirc-process)))

(defun rcirc-jump-to-first-unread-line ()
  "Move the point to the first unread line in this buffer."
  (interactive)
  (when (marker-position overlay-arrow-position)
    (goto-char overlay-arrow-position)))

(defvar rcirc-last-non-irc-buffer nil
  "The buffer to switch to when there is no more activity.")

(defun rcirc-next-active-buffer (arg)
  "Go to the ARGth rcirc buffer with activity.
The function given by `rcirc-switch-to-buffer-function' is used to
show the buffer."
  (interactive "p")
  (if rcirc-activity
      (progn
        (unless (eq major-mode 'rcirc-mode)
          (setq rcirc-last-non-irc-buffer (current-buffer)))
        (if (and (> arg 0)
                 (<= arg (length rcirc-activity)))
            (funcall rcirc-switch-to-buffer-function 
		     (nth (1- arg) rcirc-activity))
          (message "Invalid arg: %d" arg)))
    (if (eq major-mode 'rcirc-mode)
        (if (not (and rcirc-last-non-irc-buffer
                      (buffer-live-p rcirc-last-non-irc-buffer)))
            (message "No last buffer.")
          (funcall rcirc-switch-to-buffer-function rcirc-last-non-irc-buffer)
          (setq rcirc-last-non-irc-buffer nil))
      (message "No channel activity.  Go start something."))))

(defvar rcirc-activity-hooks nil
  "Hook to be run when there is channel activity.

Functions are called with a single argument, the buffer with the
activity.  Only run if the buffer is not visible and
`rcirc-ignore-channel-activity' is non-nil.")

(defun rcirc-record-activity (buffer type)
  "Record BUFFER activity with TYPE."
  (with-current-buffer buffer
    (when (not (get-buffer-window (current-buffer) t))
      (add-to-list 'rcirc-activity (current-buffer) 'append)
      (if (not rcirc-activity-type)
          (setq rcirc-activity-type type))
      (rcirc-update-activity-string)))
  (run-hook-with-args 'rcirc-activity-hooks buffer))

(defun rcirc-clear-activity (buffer)
  "Clear the BUFFER activity."
  (setq rcirc-activity (delete buffer rcirc-activity))
  (with-current-buffer buffer
    (setq rcirc-activity-type nil)))

(defun rcirc-update-activity-string ()
  "Update mode-line string."
  (setq rcirc-activity-string
        (if (or rcirc-ignore-all-activity-flag
                (not rcirc-activity))
            ""
          (concat " [" (mapconcat 
			(lambda (b)
			  (let ((s (rcirc-short-buffer-name b)))
			    (with-current-buffer b
			      (if (not (eq rcirc-activity-type 'nick))
				  s
				(rcirc-facify s 
					      'rcirc-mode-line-nick-face)))))
			rcirc-activity ",") "]"))))

(defun rcirc-short-buffer-name (buffer)
  "Return a short name for BUFFER to use in the modeline indicator."
  (with-current-buffer buffer
    (or rcirc-target (process-name rcirc-process))))

(defun rcirc-update-activity ()
  "Go through visible windows and remove buffers from activity list."
  (walk-windows (lambda (w) (rcirc-clear-activity (window-buffer w))))
  (rcirc-update-activity-string))


;;; /commands these are called with 3 args: PROCESS, TARGET, which is
;; the current buffer/channel/user, and ARGS, which is a string
;; containing the text following the /cmd.

(defmacro defun-rcirc-command (command argument docstring interactive-form 
                                       &rest body)
  "Define a command."
  `(defun ,(intern (concat "rcirc-cmd-" (symbol-name command)))
     (,@argument &optional process target)
     ,(concat docstring "\n\nNote: If PROCESS or TARGET are nil, the values of"
              "\nbuffer local variables `rcirc-process' and `rcirc-target',"
              "\nwill be used.")
     ,interactive-form
     (let ((process (or process rcirc-process))
           (target (or target rcirc-target)))
       ,@body)))

(defun-rcirc-command msg (message)
  "Send private MESSAGE to TARGET."
  (interactive "i")
  (if (null message)
      (progn
        (setq target (completing-read "Message nick: "
                                      (with-current-buffer
                                          (process-buffer rcirc-process)
                                        rcirc-nick-table)))
        (when (> (length target) 0)
          (setq message (read-string (format "Message %s: " target)))
          (when (> (length message) 0)
            (rcirc-send-message process target message))))
    (if (not (string-match "\\([^ ]+\\) \\(.+\\)" message))
        (message "Not enough args, or something.")
      (setq target (match-string 1 message)
            message (match-string 2 message))
      (rcirc-send-message process target message))))

(defun-rcirc-command query (nick)
  "Open a private chat buffer to NICK."
  (interactive (list (completing-read "Query nick: "
                                      (with-current-buffer
                                          (process-buffer rcirc-process)
                                        rcirc-nick-table))))
  (let ((new-buffer (eq (rcirc-get-buffer rcirc-process nick)
                        (process-buffer rcirc-process))))
    (switch-to-buffer (rcirc-get-buffer-create process nick))
    (when new-buffer
      (rcirc-cmd-whois nick))))

(defun-rcirc-command join (args)
  "Join CHANNEL."
  (interactive "sJoin channel: ")
  (let* ((channel (car (split-string args)))
         (buffer (rcirc-get-buffer-create process channel)))
    (when (not (eq (selected-window) (minibuffer-window)))
      (funcall rcirc-switch-to-buffer-function buffer))
    (rcirc-send-string process (concat "JOIN " args))
    (rcirc-set-last-buffer process buffer)))

(defun-rcirc-command part (channel)
  "Part CHANNEL."
  (interactive "sPart channel: ")
  (let ((channel (if (> (length channel) 0) channel target)))
    (rcirc-send-string process (concat "PART " channel " :" (rcirc-version)))))

(defun-rcirc-command quit (reason)
  "Send a quit message to server with REASON."
  (interactive "sQuit reason: ")
  (rcirc-send-string process (concat "QUIT :" reason)))

(defun-rcirc-command nick (nick)
  "Change nick to NICK."
  (interactive "i")
  (when (null nick)
    (setq nick (read-string "New nick: " (rcirc-nick process))))
  (rcirc-send-string process (concat "NICK " nick)))

(defun-rcirc-command names (channel)
  "Display list of names in CHANNEL or in current channel if CHANNEL is nil.
If called interactively, prompt for a channel when prefix arg is supplied."
  (interactive "P")
  (if (interactive-p)
      (if channel
          (setq channel (read-string "List names in channel: " target))))
  (let ((channel (if (> (length channel) 0)
                     channel
                   target)))
    (rcirc-send-string process (concat "NAMES " channel))))

(defun-rcirc-command topic (topic)
  "List TOPIC for the TARGET channel.
With a prefix arg, prompt for new topic."
  (interactive "P")
  (if (and (interactive-p) topic)
      (setq topic (read-string "New Topic: " rcirc-topic)))
  (rcirc-send-string process (concat "TOPIC " target
                                     (when (> (length topic) 0)
                                       (concat " :" topic)))))

(defun-rcirc-command whois (nick)
  "Request information from server about NICK."
  (interactive (list
                (completing-read "Whois: "
                                 (with-current-buffer
                                     (process-buffer rcirc-process)
                                   rcirc-nick-table))))
  (rcirc-set-last-buffer rcirc-process (current-buffer))
  (rcirc-send-string process (concat "WHOIS " nick)))

(defun-rcirc-command mode (args)
  "Set mode with ARGS."
  (interactive (list (concat (read-string "Mode nick or channel: ")
                             " " (read-string "Mode: "))))
  (rcirc-send-string process (concat "MODE " args)))

(defun-rcirc-command list (channels)
  "Request information on CHANNELS from server."
  (interactive "sList Channels: ")
  (rcirc-send-string process (concat "LIST " channels)))

(defun-rcirc-command oper (args)
  "Send operator command to server."
  (interactive "sOper args: ")
  (rcirc-send-string process (concat "OPER " args)))

(defun-rcirc-command quote (message)
  "Send MESSAGE literally to server."
  (interactive "sServer message: ")
  (rcirc-send-string process message))

(defun-rcirc-command kick (arg)
  "Kick NICK from current channel."
  (interactive (list
                (concat (completing-read "Kick nick: "
                                         (rcirc-channel-nicks rcirc-process
                                                              rcirc-target))
                        (read-from-minibuffer "Kick reason: "))))
  (let* ((arglist (split-string arg))
         (argstring (concat (car arglist) " :" 
                            (mapconcat 'identity (cdr arglist) " "))))
    (rcirc-send-string process (concat "KICK " target " " argstring))))

(defun rcirc-cmd-ctcp (args &optional process target)
  (if (string-match "^\\([^ ]+\\)\\s-+\\(.+\\)$" args)
      (let ((target (match-string 1 args))
            (request (match-string 2 args)))
        (rcirc-send-message process target 
                            (concat "\C-a" (upcase request) "\C-a")))
    (rcirc-print process (rcirc-nick process) "ERROR" target
                 "usage: /ctcp NICK REQUEST")))

(defun rcirc-cmd-me (args &optional process target)
  (rcirc-send-string process (format "PRIVMSG %s :\C-aACTION %s\C-a"
                                     target args)))

(defun rcirc-message-leader (sender face)
  "Return a string with SENDER propertized with FACE."
  (rcirc-facify (concat "<" (rcirc-user-nick sender) "> ") face))
  
(defun rcirc-facify (string face)
  "Return a copy of STRING with FACE property added."
  (propertize (or string "") 'face face 'rear-nonsticky t))

;; shy grouping must be used within this regexp
(defvar rcirc-url-regexp
  "\\b\\(?:\\(?:www\\.\\|\\(?:s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\
\\|wais\\|mailto\\):\\)\\(?://[-a-zA-Z0-9_.]+:[0-9]*\\)?\\(?:[-a-zA-Z0-9_=!?#$\
@~`%&*+|\\/:;.,]\\|\\w\\)+\\(?:[-a-zA-Z0-9_=#$@~`%&*+|\\/]\\|\\w\\)\\)"
  "Regexp matching URL's.  Set to nil to disable URL features in rcirc.")

(defun rcirc-browse-url (&optional arg)
  "Prompt for url to browse based on urls in buffer."
  (interactive)
  (let ((completions (mapcar (lambda (x) (cons x nil)) rcirc-urls))
        (initial-input (car rcirc-urls))
        (history (cdr rcirc-urls)))
    (browse-url (completing-read "rcirc browse-url: "
                                 completions nil nil initial-input 'history)
                arg)))

(defun rcirc-map-regexp (function regexp string)
  "Return a copy of STRING after calling FUNCTION for each REGEXP match.
FUNCTION takes 3 arguments, MATCH-START, MATCH-END, and STRING."
  (let ((start 0))
    (while (string-match regexp string start)
      (setq start (match-end 0))
      (funcall function (match-beginning 0) (match-end 0) string)))
  string)

(defvar rcirc-nick-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    (mapc (lambda (c) (modify-syntax-entry c "w" table))
          "[]\\`_^{|}-")
    (modify-syntax-entry ?' "_" table)
    table)
  "Syntax table which includes all nick characters as word constituents.")

(defun rcirc-mangle-text (process text)
  "Return TEXT with properties added based on various patterns."
  ;; ^B
  (setq text
        (rcirc-map-regexp (lambda (start end string)
                            (add-text-properties
                             start end
                             (list 'face 'bold 'rear-nonsticky t)
                             string))
                          ".*?"
                          text))
  (while (string-match "\\(.*\\)[]\\(.*\\)" text) ; deal with 
    (setq text (concat (match-string 1 text)
                       (match-string 2 text))))
  ;; my nick
  (setq text
        (with-syntax-table rcirc-nick-syntax-table
          (rcirc-map-regexp (lambda (start end string)
                              (add-text-properties
                               start end
                               (list 'face 'rcirc-nick-in-message-face
                                     'rear-nonsticky t)
                               string))
                            (concat "\\b"
                                    (regexp-quote (rcirc-nick process))
                                    "\\b")
                            text)))
  ;; urls
  (setq text
        (rcirc-map-regexp
	 (lambda (start end string)
	   (let ((orig-face (get-text-property start 'face string)))
	     (add-text-properties start end
				  (list 'face (list orig-face 'bold)
					'rear-nonsticky t)
				  string))
	     (push (substring string start end) rcirc-urls))
	   rcirc-url-regexp
	   text))
  text)


;;; handlers
;; these are called with the server PROCESS, the SENDER, which is a
;; server or a user, depending on the command, the ARGS, which is a
;; list of strings, and the TEXT, which is the original server text,
;; verbatim
(defun rcirc-handler-001 (process sender args text)
  (rcirc-handler-generic process "001" sender args text)
  ;; set the real server name
  (with-current-buffer (process-buffer process)
    (setq rcirc-server sender)
    (setq rcirc-nick (car args))
    (rcirc-update-prompt)
    (when rcirc-auto-authenticate-flag (rcirc-authenticate))
    (let (rcirc-last-buffer)
      (rcirc-join-channels process rcirc-startup-channels))))

(defun rcirc-handler-PRIVMSG (process sender args text)
  (let ((target (if (rcirc-channel-p (car args))
                    (car args)
                  (rcirc-user-nick sender)))
        (message (or (cadr args) "")))
    (if (string-match "^\C-a\\(.*\\)\C-a$" message)
        (rcirc-handler-CTCP process target sender (match-string 1 message))
      (rcirc-print process sender "PRIVMSG" target message t))
    ;; update nick timestamp
    (if (member target (rcirc-nick-channels process sender))
        (rcirc-put-nick-channel process sender target))))

(defun rcirc-handler-NOTICE (process sender args text)
  (let ((target (car args))
        (message (cadr args)))
    (rcirc-print process sender "NOTICE"
                 (cond ((rcirc-channel-p target)
                        target)
                       ((string-match "^\\[\\(#[^ ]+\\)\\]" message)
                        (match-string 1 message))
                       (sender
                        (if (string= sender (rcirc-server process))
                            (process-buffer process)
                          (rcirc-user-nick sender))))
                 message t)
    (and sender (rcirc-put-nick-channel process sender target))))

(defun rcirc-handler-WALLOPS (process sender args text)
  (let ((target (rcirc-user-nick sender)))
    (rcirc-print process sender "WALLOPS" target (car args) t)))

(defun rcirc-handler-JOIN (process sender args text)
  (let ((channel (downcase (car args)))
        (nick (rcirc-user-nick sender)))
    (rcirc-get-buffer-create process channel)
    (rcirc-print process sender "JOIN" channel "")

    ;; print in private chat buffer if it exists
    (if (not (eq (process-buffer rcirc-process)
                 (rcirc-get-buffer rcirc-process nick)))
        (rcirc-print process sender "JOIN" nick channel))

    (rcirc-put-nick-channel process sender channel)
    (if (string= nick (rcirc-nick process))
        (setq rcirc-channels (cons channel rcirc-channels)))))

;; PART and KICK are handled the same way
(defun rcirc-handler-PART-or-KICK (process response channel sender nick args)
  (rcirc-print process sender response channel (concat channel " " args))

  ;; print in private chat buffer if it exists
  (when (not (eq (process-buffer rcirc-process)
                 (rcirc-get-buffer rcirc-process nick)))
    (rcirc-print process sender response nick (concat channel " " args)))

  (if (not (string= nick (rcirc-nick process)))
      ;; this is someone else leaving
  (rcirc-remove-nick-channel process nick channel)
  ;; this is us leaving
  (mapc (lambda (n)
          (rcirc-remove-nick-channel process n channel))
        (rcirc-channel-nicks process channel))
  (setq rcirc-channels (delete channel rcirc-channels))
  (with-current-buffer (rcirc-get-buffer process channel)
    (setq rcirc-target nil))))

(defun rcirc-handler-PART (process sender args text)
  (rcirc-handler-PART-or-KICK process "PART"
                              (car args) sender (rcirc-user-nick sender)
                              (cadr args)))

(defun rcirc-handler-KICK (process sender args text)
  (rcirc-handler-PART-or-KICK process "KICK" (car args) sender (cadr args) 
                              (caddr args)))

(defun rcirc-handler-QUIT (process sender args text)
  (let ((nick (rcirc-user-nick sender)))
    (mapc (lambda (channel)
            (rcirc-print process sender "QUIT" channel (apply 'concat args)))
          (rcirc-nick-channels process nick))

    ;; print in private chat buffer if it exists
    (if (not (eq (process-buffer rcirc-process)
                 (rcirc-get-buffer rcirc-process nick)))
        (rcirc-print process sender "QUIT" nick (apply 'concat args)))

    (rcirc-nick-remove process nick)))

(defun rcirc-handler-NICK (process sender args text)
  (let* ((old-nick (rcirc-user-nick sender))
         (new-nick (car args))
         (channels (rcirc-nick-channels process old-nick)))
    ;; print message to nick's channels
    (dolist (target channels)
      (rcirc-print process sender "NICK" target new-nick))
    ;; update private chat buffer, if it exists
    (with-current-buffer (rcirc-get-buffer process old-nick)
      (when (not (equal (process-buffer rcirc-process)
                        (current-buffer)))
        (rcirc-print process sender "NICK" old-nick new-nick)
        (setq rcirc-target new-nick)
        (rename-buffer (rcirc-get-buffer-name process new-nick))))
    ;; remove old nick and add new one
    (with-current-buffer (process-buffer process)
      (let ((v (gethash old-nick rcirc-nick-table)))
        (remhash old-nick rcirc-nick-table)
        (puthash new-nick v rcirc-nick-table))
      ;; if this is our nick...
      (when (string= old-nick rcirc-nick)
        (setq rcirc-nick new-nick)
        ;; update prompts
        (mapc (lambda (target)
                (with-current-buffer (rcirc-get-buffer process target)
                  (rcirc-update-prompt)))
              (append rcirc-channels rcirc-private-chats))
        ;; reauthenticate
        (when rcirc-auto-authenticate-flag (rcirc-authenticate))))))

(defun rcirc-handler-PING (process sender args text)
  (rcirc-send-string process (concat "PONG " (car args))))

(defun rcirc-handler-PONG (process sender args text)
  ;; do nothing
  )

(defun rcirc-handler-TOPIC (process sender args text)
  (let ((topic (cadr args)))
    (rcirc-print process sender "TOPIC" (car args) topic)
    (with-current-buffer (rcirc-get-buffer process (car args))
      (setq rcirc-topic topic))))

(defun rcirc-handler-332 (process sender args text)
  "RPL_TOPIC"
  (with-current-buffer (rcirc-get-buffer process (cadr args))
    (setq rcirc-topic (caddr args))))

(defun rcirc-handler-333 (process sender args text)
  "Not in rfc1459.txt"
  (with-current-buffer (rcirc-get-buffer process (cadr args))
    (let ((setter (caddr args))
          (time (current-time-string
                 (seconds-to-time
                  (string-to-number (cadddr args))))))
      (rcirc-print process sender "TOPIC" (cadr args)
                   (format "%s (%s on %s)" rcirc-topic setter time)))))

(defun rcirc-handler-477 (process sender args text)
  "ERR_NOCHANMODES"
  (rcirc-print process sender "477" (cadr args) (caddr args)))

(defun rcirc-handler-MODE (process sender args text)
  (let ((target (car args))
        (msg (mapconcat 'identity (cdr args) " ")))
    (rcirc-print process sender "MODE"
                 (if (string= target (rcirc-nick process))
                     nil
                   target)
                 msg)

    ;; print in private chat buffers if they exist
    (mapc (lambda (nick)
            (when (not (eq (process-buffer rcirc-process)
                           (rcirc-get-buffer rcirc-process nick)))
              (rcirc-print process sender "MODE" nick msg)))
          (cddr args))))

(defun rcirc-get-temp-buffer-create (process channel)
  "Return a buffer based on PROCESS and CHANNEL."
  (let ((tmpnam (concat " " (downcase channel) "TMP" (process-name process))))
    (get-buffer-create tmpnam)))

(defun rcirc-handler-353 (process sender args text)
  "RPL_NAMREPLY"
  (let ((channel (downcase (caddr args))))
    (mapc (lambda (nick)
            (rcirc-put-nick-channel process nick channel))
          (delete "" (split-string (cadddr args) " ")))
    (with-current-buffer (rcirc-get-temp-buffer-create process channel)
      (goto-char (point-max))
      (insert (car (last args)) " "))))

(defun rcirc-handler-366 (process sender args text)
  "RPL_ENDOFNAMES"
  (let* ((channel (cadr args))
         (buffer (rcirc-get-temp-buffer-create process channel)))
    (with-current-buffer buffer
      (rcirc-print process sender "NAMES" channel
                   (buffer-substring (point-min) (point-max))))
    (kill-buffer buffer)))

(defun rcirc-handler-433 (process sender args text)
  "ERR_NICKNAMEINUSE"
  (rcirc-handler-generic process "433" sender args text)
  (let* ((new-nick (concat (cadr args) "`")))
    (with-current-buffer (process-buffer process)
      (rcirc-cmd-nick new-nick nil process))))

(defun rcirc-authenticate ()
  "Send authentication to process associated with current buffer.
Passwords are read from `rcirc-authinfo-file-name' (which see)."
  (interactive)
  (let ((password-alist
         (with-temp-buffer
           (insert-file-contents-literally rcirc-authinfo-file-name)
           (goto-char (point-min))
           (read (current-buffer)))))
    (with-current-buffer (process-buffer rcirc-process)
      (dolist (i password-alist)
        (let ((server (car i))
              (nick (cadr i))
              (method (caddr i))
              (args (cdddr i)))
          (when (and (string-match server rcirc-server)
                     (string-match nick rcirc-nick))
            (cond ((equal method 'nickserv)
                   (rcirc-send-string
                    rcirc-process
                    (concat
                     "PRIVMSG nickserv :identify " 
		     (car args))))
                  ((equal method 'chanserv)
                   (rcirc-send-string
                    rcirc-process
                    (concat
                     "PRIVMSG chanserv :identify " 
		     (car args) " " (cadr args))))
                  ((equal method 'bitlbee)
                   (rcirc-send-string
                    rcirc-process
                    (concat "PRIVMSG #bitlbee :identify " (car args))))
                  (t
                   (message "No %S authentication method defined" 
			    method)))))))))
 
(defun rcirc-handler-INVITE (process sender args text)
  (rcirc-print process sender "INVITE" nil (mapconcat 'identity args " ") t))

(defun rcirc-handler-ERROR (process sender args text)
  (rcirc-print process sender "ERROR" nil (mapconcat 'identity args " ")))

(defun rcirc-handler-CTCP (process target sender text)
  (if (string-match "^\\([^ ]+\\) *\\(.*\\)$" text)
      (let* ((request (upcase (match-string 1 text)))
             (args (match-string 2 text))
             (nick (rcirc-user-nick sender))
             (handler (intern-soft (concat "rcirc-handler-ctcp-" request))))
        (if (not (fboundp handler))
            (rcirc-print process sender "ERROR" target 
                         (format "unhandled ctcp: %s" text))
          (funcall handler process target sender args)
          (if (not (string= request "ACTION"))
              (rcirc-print process sender "CTCP" target
			   (format "%s" text)))))))

(defun rcirc-handler-ctcp-VERSION (process target sender args)
  (rcirc-send-string process
                     (concat "NOTICE " (rcirc-user-nick sender)
                             " :\C-aVERSION " (rcirc-version)
                             " - http://www.nongnu.org/rcirc"
                             "\C-a")))

(defun rcirc-handler-ctcp-ACTION (process target sender args)
  (rcirc-print process sender "ACTION" target args t))

(defun rcirc-handler-ctcp-TIME (process target sender args)
  (rcirc-send-string process
                     (concat "NOTICE " (rcirc-user-nick sender)
                             " :\C-aTIME " (current-time-string) "\C-a")))

(defface rcirc-my-nick-face
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    (t (:inverse-video t :bold t)))
  "The rcirc face used to highlight my messages."
  :group 'rcirc)

(defface rcirc-other-nick-face
  '((((type tty) (class color)) (:foreground "yellow" :weight light))
    (((class grayscale) (background light))
     (:foreground "Gray90" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :bold t :italic t))
    (((class color) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:foreground "LightGoldenrod"))
    (t (:bold t :italic t)))
  "The rcirc face used to highlight other messages."
  :group 'rcirc)

(defface rcirc-server-face
  '((((type tty pc) (class color) (background light)) (:foreground "red"))
    (((type tty pc) (class color) (background dark)) (:foreground "red1"))
    (((class grayscale) (background light))
     (:foreground "DimGray" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :bold t :italic t))
    (((class color) (background light)) (:foreground "gray40"))
    (((class color) (background dark)) (:foreground "chocolate1"))
    (t (:bold t :italic t)))
  "The rcirc face used to highlight server messages."
  :group 'rcirc)

(defface rcirc-nick-in-message-face
  '((((type tty) (class color)) (:foreground "cyan" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Cyan"))
    (t (:bold t)))
  "The rcirc face used to highlight instances of nick within messages."
  :group 'rcirc)

(defface rcirc-prompt-face
  '((((background dark)) (:foreground "cyan"))
    (t (:foreground "dark blue")))
  "The rcirc face to use to highlight prompts."
  :group 'rcirc)

(defface rcirc-mode-line-nick-face
  '((t (:bold t)))
  "The rcirc face used indicate activity directed at you."
  :group 'rcirc)

;; When using M-x flyspell-mode, only check words past the input marker
(put 'rcirc-mode 'flyspell-mode-predicate 'rcirc-looking-at-input)
(defun rcirc-looking-at-input ()
  "Returns true if point is past the input marker."
  (>= (point) rcirc-prompt-end-marker))


(provide 'rcirc)
;;; rcirc.el ends here
