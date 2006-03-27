;;; erc-identd.el --- RFC1413 (identd authentication protocol) server

;; Copyright (C) 2003, 2006 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Keywords: comm, processes

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

;; You can have a local identd server (running on port 8113; I use DNAT
;; to bind 113->8113) if you add this to .emacs.el:

;;    (add-hook 'erc-connect-pre-hook 'erc-identd-start)
;;    (add-hook 'erc-disconnected-hook 'erc-identd-stop)

;;; Code:

(defvar erc-identd-process nil)

(defun erc-identd-filter (proc string)
  "This filter implements RFC1413 (identd authentication protocol)."
  (let ((erc-identd-process proc))
    (when (string-match "\\([0-9]+\\)\\s-*,\\s-*\\([0-9]+\\)" string)
      (let ((port-on-server (match-string 1 string))
	    (port-on-client (match-string 2 string)))
	(send-string erc-identd-process
		     (format "%s, %s : USERID : %s : %s\n"
			     port-on-server port-on-client
			     system-type (user-login-name)))
	(process-send-eof erc-identd-process)))))

;;;###autoload
(defun erc-identd-start (&optional port)
  "Start an identd server listening to port 8113.
Port 113 (auth) will need to be redirected to port 8113 on your
machine -- using iptables, or a program like redir which can be
run from inetd.  The idea is to provide a simple identd server
when you need one, without having to install one globally on your
system."
  (interactive (list (read-string "Serve identd requests on port: " "8113")))
  (if (null port)
      (setq port 8113)
    (if (stringp port)
	(setq port (string-to-number port))))
  (if erc-identd-process
      (delete-process erc-identd-process))
  (setq erc-identd-process
	(make-network-process :name "identd"
			      :buffer (generate-new-buffer "identd")
			      :host 'local :service port
			      :server t :noquery t
			      :filter 'erc-identd-filter)))

;;;###autoload
(defun erc-identd-stop (&rest ignore)
  (interactive)
  (when erc-identd-process
    (delete-process erc-identd-process)
    (setq erc-identd-process nil)))

(provide 'erc-identd)

;;; erc-identd.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

;; arch-tag: e0b5f926-0f35-40b9-8ddb-ca06b62a7544
