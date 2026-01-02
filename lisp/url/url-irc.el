;;; url-irc.el --- IRC URL interface  -*- lexical-binding: t -*-

;; Copyright (C) 1996-1999, 2004-2026 Free Software Foundation, Inc.

;; Keywords: comm, data, processes

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

;; IRC URLs are defined in
;; https://www.w3.org/Addressing/draft-mirashi-url-irc-01.txt

;;; Code:

(require 'url-vars)
(require 'url-parse)

(defconst url-irc-default-port 6667 "Default port for IRC connections.")

(defcustom url-irc-function 'url-irc-rcirc
  "Function to actually open an IRC connection.
The function should take the following arguments:
    HOST - the hostname of the IRC server to contact
    PORT - the port number of the IRC server to contact
 CHANNEL - What channel on the server to visit right away (can be nil)
    USER - What username to use
PASSWORD - What password to use.
  SCHEME - a URI scheme, such as \"irc\" or \"ircs\""
  :type '(choice (const :tag "rcirc" :value url-irc-rcirc)
		 (const :tag "ERC" :value url-irc-erc)
		 (const :tag "ZEN IRC" :value url-irc-zenirc)
		 (function :tag "Other"))
  :version "29.1" ; Added SCHEME
  :group 'url)

;; External.
(declare-function zenirc "ext:zenirc" (&optional prefix))
(declare-function zenirc-send-line "ext:zenirc" ())
(defvar zenirc-server-alist)
(defvar zenirc-buffer-name)

(defun url-irc-zenirc (host port channel user password _)
  (let ((zenirc-buffer-name (if (and user host port)
				(format "%s@%s:%d" user host port)
			      (format "%s:%d" host port)))
	(zenirc-server-alist
	 (list
	  (list host port password nil user))))
    (zenirc)
    (goto-char (point-max))
    (if (not channel)
	nil
      (insert "/join " channel)
      (zenirc-send-line))))

(defun url-irc-rcirc (host port channel user password _)
  (let ((chan (when channel (concat "#" channel))))
    (rcirc-connect host port user nil nil (when chan (list chan)) password)
    (when chan
      (switch-to-buffer (concat chan "@" host)))))

(defun url-irc-erc (host port channel user password scheme)
  (erc-handle-irc-url host port channel user password scheme))

;;;###autoload
(defun url-irc (url)
  (let* ((host (url-host url))
	 (port (url-port url))
	 (pass (url-password url))
	 (user (url-user url))
         (chan (url-filename url))
         (type (url-type url)))
    (if (url-target url)
	(setq chan (concat chan "#" (url-target url))))
    (if (string-match "^/" chan)
	(setq chan (substring chan 1 nil)))
    (if (= (length chan) 0)
	(setq chan nil))
    (condition-case nil
        (funcall url-irc-function host port chan user pass type)
      (wrong-number-of-arguments
       (display-warning 'url
                        (concat "Incompatible value for `url-irc-function'."
                                " Likely not expecting a 6th (SCHEME) arg."))
       (funcall url-irc-function host port chan user pass)))
    nil))

;;;; ircs://

;; The function `url-scheme-get-property' tries and fails to load the
;; nonexistent url-ircs.el but falls back to using the following:

;;;###autoload
(defconst url-ircs-default-port 6697 "Default port for IRCS connections.")

;;;###autoload
(defalias 'url-ircs 'url-irc)

(provide 'url-irc)

;;; url-irc.el ends here
