;;; org-mac-message.el - Support for links to Apple Mail messages by Message-ID
;; Carstens outline-mode for keeping track of everything.
;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: John Wiegey <johnw@gnu.org>
;; Version: 1.2
;; Keywords: outlines, hypermedia, calendar, wp
;;
;; This file is part of GNU Emacs.
;;
;; Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)

(org-add-link-type "message" 'org-mac-message-open)

(declare-function do-applescript "mac.c" (string))
(unless (fboundp 'do-applescript)
  ;; Need to fake this using shell-command-to-string
  (defun do-applescript (script)
    (let (start cmd return)
      (while (string-match "\n" script)
	(setq script (replace-match "\r" t t script)))
      (while (string-match "'" script start)
	(setq start (+ 2 (match-beginning 0))
	      script (replace-match "\\'" t t script)))
      (setq cmd (concat "osascript -e '" script "'"))
      (setq return (shell-command-to-string cmd))
      (concat "\"" (org-trim return) "\""))))

(defun org-mac-message-open (message-id)
  "Visit the message with the given Message-ID.
This will use the command `open' with the message url."
  (start-process (concat "open message:" message-id) nil
		 "open" (concat "message://<" (substring message-id 2) ">")))

(defun org-mac-message-insert-link ()
  "Insert a link to the messages currently selected in Apple Mail.
This will use applescript to get the message-id and the subject of the
active mail in AppleMail and make a link out of it."
  (interactive)
  (insert (org-mac-message-get-link)))

(defun org-mac-message-get-link ()
  "Insert a link to the messages currently selected in Apple Mail.
This will use applescript to get the message-id and the subject of the
active mail in AppleMail and make a link out of it."
  (let ((subject (do-applescript "tell application \"Mail\"
	set theMessages to selection
	subject of beginning of theMessages
end tell"))
	(message-id (do-applescript "tell application \"Mail\"
	set theMessages to selection
	message id of beginning of theMessages
end tell")))
    (org-make-link-string
     (concat "message://"
	     (substring message-id 1 (1- (length message-id))))
     (substring subject 1 (1- (length subject))))))

(provide 'org-mac-message)

;;; org-mac-message.el ends here
