;;; erc-nickserv.el --- Identify to NickServ

;; Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; There are two ways to go about identifying yourself automatically to
;; NickServ with this module.  The more secure way is to listen for identify
;; requests from the user NickServ.  Another way is to identify yourself to
;; NickServ directly after a successful connection and every time you change
;; your nickname.  This method is rather insecure, though, because no checks
;; are made to test if NickServ is the real NickServ for a given network or
;; server.

;; As a default, ERC has the data for the official nickname services on the
;; networks Austnet, BrasNET, Dalnet, freenode, GalaxyNet, and Slashnet.
;; You can add more by using M-x customize-variable RET erc-nickserv-alist.

;; Usage:
;;
;; Put into your .emacs:
;;
;; (require 'erc-nickserv)
;; (erc-services-mode 1)
;;
;; Add your nickname and NickServ password to `erc-nickserv-passwords'.
;; Using the freenode network as an example:
;;
;; (setq erc-nickserv-passwords '((freenode (("nickname" "password")))))
;;
;; The default automatic identification mode is autodetection of NickServ
;; identify requests.  Set the variable `erc-nickserv-identify-mode' if
;; you'd like to change this behavior.  You can also change the way
;; automatic identification is handled by using:
;;
;; M-x erc-nickserv-identify-mode
;;
;; If you'd rather not identify yourself automatically but would like access
;; to the functions contained in this file, just load this file without
;; enabling `erc-services-mode'.
;;

;;; Code:

(require 'erc)
(require 'erc-nets)
(eval-when-compile (require 'cl))

;; Customization:

(defgroup erc-services nil
  "Configuration for IRC services.

On some networks, there exists a special type of automated irc bot,
called Services.  Those usually allow you to register your nickname,
post/read memos to other registered users who are currently offline,
and do various other things.

This group allows you to set variables to somewhat automate
communication with those Services."
  :group 'erc)

;;;###autoload (autoload 'erc-services-mode "erc-nickserv" nil t)
(define-erc-module services nickserv
  "This mode automates communication with services."
  ((erc-nickserv-identify-mode erc-nickserv-identify-mode))
  ((remove-hook 'erc-server-NOTICE-functions
		'erc-nickserv-identify-autodetect)
   (remove-hook 'erc-after-connect
		'erc-nickserv-identify-on-connect)
   (remove-hook 'erc-nick-changed-functions
		'erc-nickserv-identify-on-nick-change)))

;;;###autoload
(defun erc-nickserv-identify-mode (mode)
  "Set up hooks according to which MODE the user has chosen."
  (interactive
   (list (intern (completing-read
		  "Choose Nickserv identify mode (RET to disable): "
		  '(("autodetect") ("nick-change")) nil t))))
  (cond ((eq mode 'autodetect)
	 (setq erc-nickserv-identify-mode 'autodetect)
	 (add-hook 'erc-server-NOTICE-functions
		   'erc-nickserv-identify-autodetect)
	 (remove-hook 'erc-nick-changed-functions
		      'erc-nickserv-identify-on-nick-change)
	 (remove-hook 'erc-after-connect
		      'erc-nickserv-identify-on-connect))
	((eq mode 'nick-change)
	 (setq erc-nickserv-identify-mode 'nick-change)
	 (add-hook 'erc-after-connect
		   'erc-nickserv-identify-on-connect)
	 (add-hook 'erc-nick-changed-functions
		   'erc-nickserv-identify-on-nick-change)
	 (remove-hook 'erc-server-NOTICE-functions
		      'erc-nickserv-identify-autodetect))
	(t
	 (setq erc-nickserv-identify-mode nil)
	 (remove-hook 'erc-server-NOTICE-functions
		      'erc-nickserv-identify-autodetect)
	 (remove-hook 'erc-after-connect
		      'erc-nickserv-identify-on-connect)
	 (remove-hook 'erc-nick-changed-functions
		      'erc-nickserv-identify-on-nick-change))))

(defcustom erc-nickserv-identify-mode 'autodetect
  "The mode which is used when identifying to Nickserv.

Possible settings are:.

'autodetect  - Identify when the real Nickserv sends an identify request.
'nick-change - Identify when you change your nickname.
nil          - Disables automatic Nickserv identification.

You can also use M-x erc-nickserv-identify-mode to change modes."
  :group 'erc-services
  :type '(choice (const autodetect)
		 (const nick-change)
		 (const nil))
  :set (lambda (sym val)
	 (set-default sym val)
	 (erc-nickserv-identify-mode val)))

(defcustom erc-prompt-for-nickserv-password t
  "Ask for the password when identifying to NickServ."
  :group 'erc-services
  :type 'boolean)

(defcustom erc-nickserv-passwords nil
  "Passwords used when identifying to NickServ automatically.

Example of use:
  (setq erc-nickserv-passwords
        '((freenode ((\"nick-one\" . \"password\")
                     (\"nick-two\" . \"password\")))
          (DALnet ((\"nick\" . \"password\")))))"
  :group 'erc-services
  :type '(repeat
	  (list :tag "Network"
		(choice :tag "Network name"
			(const freenode)
			(const DALnet)
			(const GalaxyNet)
			(const SlashNET)
			(const BRASnet)
			(const iip)
			(const Austnet)
			(symbol :tag "Network name"))
		(repeat :tag "Nickname and password"
			(cons :tag "Identity"
			      (string :tag "Nick")
			      (string :tag "Password"))))))

;; Variables:

(defcustom erc-nickserv-alist
  '((DALnet
     "NickServ!service@dal.net"
     "/msg\\s-NickServ@services.dal.net\\s-IDENTIFY\\s-<password>"
     "NickServ@services.dal.net"
     "IDENTIFY"
     nil)
    (freenode
     "NickServ!NickServ@services."
     "/msg\\s-NickServ\\s-IDENTIFY\\s-<password>"
     "NickServ"
     "IDENTIFY"
     nil)
    (GalaxyNet
     "NS!nickserv@galaxynet.org"
     "Please\\s-change\\s-nicks\\s-or\\s-authenticate."
     "NS@services.galaxynet.org"
     "AUTH"
     t)
    (SlashNET
     "NickServ!services@services.slashnet.org"
     "/msg\\s-NickServ\\s-IDENTIFY\\s-password"
     "NickServ@services.slashnet.org"
     "IDENTIFY"
     nil)
    (iip
     "Trent@anon.iip"
     "type\\s-/squery\\s-Trent\\s-identify\\s-<password>"
     "Trent@anon.iip"
     "IDENTIFY"
     nil
     "SQUERY")
    (BRASnet
     "NickServ!services@brasnet.org"
     "/NickServ\\s-IDENTIFY\\s-senha"
     "NickServ"
     "IDENTIFY"
     nil
     "")
     (Austnet
      "NickOP!service@austnet.org"
      "/msg\\s-NickOP@austnet.org\\s-identify\\s-<password>"
      "nickop@austnet.org"
      "identify"
      nil)
     (Azzurra
      "NickServ!service@azzurra.org"
      "/ns\\s-IDENTIFY\\s-password"
      "NickServ"
      "IDENTIFY"
      nil)
     (OFTC
      "NickServ!services@services.oftc.net"
      "/msg\\s-NickServ\\s-IDENTIFY\\s-\^_password"
      "NickServ"
      "IDENTIFY"
      nil))
   "Alist of NickServer details, sorted by network.
Every element in the list has the form
  \(SYMBOL NICKSERV REGEXP NICK KEYWORD USE-CURRENT ANSWER)

SYMBOL is a network identifier, a symbol, as used in `erc-networks-alist'.
NICKSERV is the description of the nickserv in the form nick!user@host.
REGEXP is a regular expression matching the message from nickserv.
NICK is nickserv's nickname.  Use nick@server where necessary/possible.
KEYWORD is the keyword to use in the reply message to identify yourself.
USE-CURRENT indicates whether the current nickname must be used when
  identifying.
ANSWER is the command to use for the answer.  The default is 'privmsg.
  This last element is optional."
   :group 'erc-services
   :type '(repeat
	   (list :tag "Nickserv data"
		 (symbol :tag "Network name")
		 (string :tag "Nickserv's nick!user@host")
		 (regexp :tag "Identify request sent by Nickserv")
		 (string :tag "Identify to")
		 (string :tag "Identify keyword")
		 (boolean :tag "Use current nick in identify message?")
		 (choice :tag "Command to use (optional)"
		  (string :tag "Command")
		  (const :tag "No special command necessary" nil)))))

;; Functions:

(defun erc-nickserv-identify-autodetect (proc parsed)
  "Check for a NickServ identify request everytime a notice is received.
Make sure it is the real NickServ for this network and that it has
specifically asked the user to IDENTIFY.
If `erc-prompt-for-nickserv-password' is non-nil, prompt the user for the
password for this nickname, otherwise try to send it automatically."
  (unless (and (null erc-nickserv-passwords)
	       (null erc-prompt-for-nickserv-password))
    (let* ((network (erc-network))
	   (nickserv (nth 1 (assoc network erc-nickserv-alist)))
	   (identify-regex (nth 2 (assoc network erc-nickserv-alist)))
	   (sspec (erc-response.sender parsed))
	   (nick (car (erc-response.command-args parsed)))
	   (msg (erc-response.contents parsed)))
      ;; continue only if we're sure it's the real nickserv for this network
      ;; and it's asked us to identify
      (when (and nickserv (equal sspec nickserv)
		 (string-match identify-regex msg))
	(erc-log "NickServ IDENTIFY request detected")
	(erc-nickserv-call-identify-function nick)
	nil))))

(defun erc-nickserv-identify-on-connect (server nick)
  "Identify to Nickserv after the connection to the server is established."
  (unless (and (null erc-nickserv-passwords)
	       (null erc-prompt-for-nickserv-password))
    (erc-nickserv-call-identify-function nick)))

(defun erc-nickserv-identify-on-nick-change (nick old-nick)
  "Identify to Nickserv whenever your nick changes."
  (unless (and (null erc-nickserv-passwords)
	       (null erc-prompt-for-nickserv-password))
    (erc-nickserv-call-identify-function nick)))

(defun erc-nickserv-call-identify-function (nickname)
  "Call `erc-nickserv-identify' interactively or run it with NICKNAME's
password.
The action is determined by the value of `erc-prompt-for-nickserv-password'."
  (if erc-prompt-for-nickserv-password
      (call-interactively 'erc-nickserv-identify)
    (when erc-nickserv-passwords
      (erc-nickserv-identify
       (cdr (assoc nickname
		   (nth 1 (assoc (erc-network)
				 erc-nickserv-passwords))))))))

;;;###autoload
(defun erc-nickserv-identify (password)
  "Send an \"identify <PASSWORD>\" message to NickServ.
When called interactively, read the password using `read-passwd'."
  (interactive
   (list (read-passwd
	  (format "NickServ password for %s on %s (RET to cancel): "
		  (erc-current-nick)
		  (or (and (erc-network)
			   (symbol-name (erc-network)))
		      "Unknown network")))))
  (when (and password (not (string= "" password)))
    (let* ((erc-auto-discard-away nil)
	   (network (erc-network))
	   (nickserv-info (assoc network erc-nickserv-alist))
	   (nickserv (or (nth 3 nickserv-info) "NickServ"))
	   (identify-word (or (nth 4 nickserv-info) "IDENTIFY"))
	   (nick (if (nth 5 nickserv-info)
		     (concat (erc-current-nick) " ")
		   ""))
	   (msgtype (or (nth 6 nickserv-info) "PRIVMSG")))
      (erc-message msgtype
		   (concat nickserv " " identify-word " " nick password)))))

(provide 'erc-nickserv)

;;; erc-nickserv.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

;; arch-tag: d401c8aa-d938-4255-96a9-3efb64c47e58
