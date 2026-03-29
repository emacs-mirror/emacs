;;; erc-join.el --- autojoin channels on connect and reconnects  -*- lexical-binding: t; -*-

;; Copyright (C) 2002-2004, 2006-2026 Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Amin Bandali <bandali@gnu.org>, F. Jason Park <jp@neverwas.me>
;; Keywords: comm, irc
;; URL: https://www.emacswiki.org/emacs/ErcAutoJoin

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

;; This allows us to customize an `erc-autojoin-channels-alist'.  As
;; we /JOIN and /PART channels, this alist is updated to reflect our
;; current setup, so that when we reconnect, we rejoin the same
;; channels.  The alist can be customized, so that the customized
;; value will be used when we reconnect in our next Emacs session.

;;; Code:

(require 'erc)

(defgroup erc-autojoin nil
  "Enable autojoining."
  :group 'erc)

;;;###autoload(autoload 'erc-autojoin-mode "erc-join" nil t)
(define-erc-module autojoin nil
  "Makes ERC autojoin on connects and reconnects."
  ((add-hook 'erc-after-connect #'erc-autojoin-channels)
   (add-hook 'erc-nickserv-identified-hook #'erc-autojoin-after-ident)
   (add-hook 'erc-server-JOIN-functions #'erc-autojoin-add)
   (add-hook 'erc-server-PART-functions #'erc-autojoin-remove)
   (add-hook 'erc-server-405-functions #'erc-join--remove-requested-channel)
   (add-hook 'erc-server-471-functions #'erc-join--remove-requested-channel)
   (add-hook 'erc-server-473-functions #'erc-join--remove-requested-channel)
   (add-hook 'erc-server-474-functions #'erc-join--remove-requested-channel)
   (add-hook 'erc-server-475-functions #'erc-join--remove-requested-channel))
  ((remove-hook 'erc-after-connect #'erc-autojoin-channels)
   (remove-hook 'erc-nickserv-identified-hook #'erc-autojoin-after-ident)
   (remove-hook 'erc-server-JOIN-functions #'erc-autojoin-add)
   (remove-hook 'erc-server-PART-functions #'erc-autojoin-remove)
   (remove-hook 'erc-server-405-functions #'erc-join--remove-requested-channel)
   (remove-hook 'erc-server-471-functions #'erc-join--remove-requested-channel)
   (remove-hook 'erc-server-473-functions #'erc-join--remove-requested-channel)
   (remove-hook 'erc-server-474-functions #'erc-join--remove-requested-channel)
   (remove-hook 'erc-server-475-functions #'erc-join--remove-requested-channel)
   (erc-buffer-do (lambda ()
                    (kill-local-variable 'erc-join--requested-channels)))))

(defcustom erc-autojoin-channels-alist nil
  "Alist of channels to autojoin on IRC networks.
Every element in the alist has the form (SERVER . CHANNELS).
SERVER is a regexp matching the server, and channels is the list
of channels to join.  SERVER can also be a symbol, in which case
it's matched against a non-nil `:id' passed to `erc' or `erc-tls'
when connecting or the value of the current `erc-network' instead of
`erc-server-announced-name' or `erc-session-server' (this can be
useful when connecting to an IRC proxy that relays several
networks under the same server).

Note that for historical reasons, this option is mutated at runtime,
which is regrettable but here to stay.  Please double check the value
before saving it to a `custom-file'.

If the channel(s) require channel keys for joining, the passwords
are found via auth-source.  For instance, if you use ~/.authinfo
as your auth-source backend, then put something like the
following in that file:

machine irc.example.net login \"#fsf\" password sEcReT

Customize this variable to set the value for your first connect.
Once you are connected and join and part channels, this alist
keeps track of what channels you are on, and will join them
again when you get disconnected.  When you restart Emacs, however,
those changes are lost, and the customization you saved the last
time is used again."
  :type '(alist :options (Libera.Chat)
                :key-type (choice :tag "Server"
                                  (symbol :tag "Network")
                                  (regexp :tag "Host or domain"))
                :value-type (repeat :tag "Channels" (string :tag "Name"))))

(defcustom erc-autojoin-timing 'connect
  "When ERC should attempt to autojoin a channel.
If the value is `connect', autojoin immediately on connecting.
If the value is `ident', autojoin after successful NickServ
identification, or after `erc-autojoin-delay' seconds.
Any other value means the same as `connect'."
  :version "24.1"
  :type  '(choice (const :tag "On Connection" connect)
		  (const :tag "When Identified" ident)))

(defcustom erc-autojoin-delay 30
  "Number of seconds to wait before attempting to autojoin channels.
This only takes effect if `erc-autojoin-timing' is `ident'.
If NickServ identification occurs before this delay expires, ERC
autojoins immediately at that time."
  :version "24.1"
  :type  'integer)

(defcustom erc-autojoin-domain-only t
  "Truncate host name to the domain name when joining a server.
If non-nil, and a channel on the server a.b.c is joined, then
only b.c is used as the server for `erc-autojoin-channels-alist'.
This is important for networks that redirect you to other
servers, presumably in the same domain."
  :type 'boolean)

(defvar-local erc--autojoin-timer nil)

(defun erc-autojoin-channels-delayed (_ _ buffer)
  "Attempt to autojoin channels in a server BUFFER.
Expect to run on a timer after `erc-autojoin-delay' seconds when
`erc-autojoin-timing' is the symbol `ident'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cl-assert (erc--server-buffer-p))
      (when erc--autojoin-timer
        (cancel-timer erc--autojoin-timer)
        (setq erc--autojoin-timer nil))
      ;; This log message is likely supposed to indicate that
      ;; `erc-nickserv-identified-hook' did not yet run, assuming the
      ;; services module is active.
      (erc-log "Delayed autojoin started (no ident success detected yet)")
      (erc-autojoin--join))))

(defun erc-autojoin-server-match (candidate)
  "Match the current network ID or server against CANDIDATE.
CANDIDATE is a key from `erc-autojoin-channels-alist'.  Return the
matching entity, either a string or a non-nil symbol (in the case of a
network or a network ID).  Return nil on failure."
  (if (symbolp candidate)
      (eq (or (erc-networks--id-given erc-networks--id) (erc-network))
          candidate)
    (when (stringp candidate)
      (string-match-p candidate (or erc-server-announced-name
                                    erc-session-server)))))

(defvar-local erc-join--requested-channels nil
  "List of channels for which an outgoing JOIN was sent.")

;; Assume users will update their `erc-autojoin-channels-alist' when
;; encountering errors, like a 475 ERR_BADCHANNELKEY.
(defun erc-join--remove-requested-channel (_ parsed)
  "Remove channel from `erc-join--requested-channels'."
  (when-let* ((channel (cadr (erc-response.command-args parsed)))
              ((member channel erc-join--requested-channels)))
    (setq erc-join--requested-channels
          (delete channel erc-join--requested-channels)))
  nil)

(cl-defmethod erc--server-determine-join-display-context
  (channel alist &context (erc-autojoin-mode (eql t)))
  "Add item to `erc-display-context' ALIST if CHANNEL was autojoined."
  (when (member channel erc-join--requested-channels)
    (setq erc-join--requested-channels
          (delete channel erc-join--requested-channels))
    (push (cons 'erc-autojoin-mode channel) alist))
  (cl-call-next-method channel alist))

(defun erc-autojoin--join ()
  ;; This is called in the server buffer
  (pcase-dolist (`(,name . ,channels) erc-autojoin-channels-alist)
    (when-let* ((match (erc-autojoin-server-match name)))
      (dolist (chan channels)
        (let ((buf (erc-get-buffer chan erc-server-process)))
          (unless (and buf (with-current-buffer buf
                             (erc--current-buffer-joined-p)))
            (push chan erc-join--requested-channels)
            (erc-server-join-channel nil chan)))))))

(defun erc-autojoin-after-ident (_network _nick)
  "Autojoin channels in `erc-autojoin-channels-alist'.
Expect to run in a server buffer on `erc-nickserv-identified-hook' after
services has authenticated the client."
  (when (eq erc-autojoin-timing 'ident)
    (when erc--autojoin-timer
      (cancel-timer erc--autojoin-timer)
      (setq erc--autojoin-timer nil))
    (erc-autojoin--join)))

(defun erc-autojoin-channels (server nick)
  "Autojoin channels in `erc-autojoin-channels-alist'."
  (if (eq erc-autojoin-timing 'ident)
      ;; Prepare the delayed autojoin timer, in case ident doesn't
      ;; happen within the allotted time limit:
      (when (> erc-autojoin-delay 0)
	(setq erc--autojoin-timer
	      (run-with-timer erc-autojoin-delay nil
			      #'erc-autojoin-channels-delayed
			      server nick (current-buffer))))
    ;; `erc-autojoin-timing' is `connect':
    (erc-autojoin--join)))

(defun erc-autojoin-current-server ()
  "Compute the current server for lookup in `erc-autojoin-channels-alist'.
Respects `erc-autojoin-domain-only'."
  (let ((server (or erc-server-announced-name erc-session-server)))
    (if (and erc-autojoin-domain-only
	     (string-match "[^.\n]+\\.\\([^.\n]+\\.[^.\n]+\\)$" server))
	(match-string 1 server)
      server)))

(defun erc-autojoin--mutate (proc parsed remove)
  (when-let* ((nick (car (erc-parse-user (erc-response.sender parsed))))
              ((erc-current-nick-p nick))
              (chnl (car (erc-response.command-args parsed)))
              (elem (or (and (erc--valid-local-channel-p chnl)
                             (regexp-quote erc-server-announced-name))
                        (erc-networks--id-given erc-networks--id)
                        (erc-network)
                        (with-current-buffer (process-buffer proc)
                          (erc-autojoin-current-server))))
              (test (if (symbolp elem) #'eq #'equal)))
    (if remove
        (let ((cs (delete chnl (assoc-default elem erc-autojoin-channels-alist
                                              test))))
          (setf (alist-get elem erc-autojoin-channels-alist nil (null cs) test)
                cs))
      (cl-pushnew chnl
                  (alist-get elem erc-autojoin-channels-alist nil nil test)
                  :test #'equal))))

(defun erc-autojoin-add (proc parsed)
  "Add the channel being joined to `erc-autojoin-channels-alist'."
  (erc-autojoin--mutate proc parsed nil)
  ;; We must return nil to tell ERC to continue running the other
  ;; functions.
  nil)

;; (erc-parse-user "kensanata!~user@dclient217-162-233-228.hispeed.ch")

(defun erc-autojoin-remove (proc parsed)
  "Remove the channel being left from `erc-autojoin-channels-alist'."
  (erc-autojoin--mutate proc parsed 'remove)
  ;; We must return nil to tell ERC to continue running the other
  ;; functions.
  nil)

(provide 'erc-join)

;;; erc-join.el ends here
;;
;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
