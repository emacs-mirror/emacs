;;; nndb.el --- nndb access for Gnus
;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: Kai Grossjohann <grossjohann@ls6.informatik.uni-dortmund.de>
;; Keywords: news

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; I have shamelessly snarfed the code of nntp.el from sgnus.
;;              Kai


;;-
;; Register nndb with known select methods.

(setq gnus-valid-select-methods
      (cons '("nndb" mail address respool prompt-address)
            gnus-valid-select-methods))


;;; Code:

(require 'nnheader)
(require 'nntp)
(eval-when-compile (require 'cl))

(eval-and-compile
  (unless (fboundp 'open-network-stream)
    (require 'tcp)))

(eval-when-compile (require 'cl))

(eval-and-compile
  (autoload 'news-setup "rnewspost")
  (autoload 'news-reply-mode "rnewspost")
  (autoload 'cancel-timer "timer")
  (autoload 'telnet "telnet" nil t)
  (autoload 'telnet-send-input "telnet" nil t)
  (autoload 'timezone-parse-date "timezone"))

;; Declare nndb as derived from nntp

(nnoo-declare nndb nntp)

;; Variables specific to nndb

;;- currently not used but just in case...
(defvoo nndb-deliver-program "nndel"
  "*The program used to put a message in an NNDB group.")

;; Variables copied from nntp

(defvoo nndb-server-opened-hook '(nntp-send-authinfo-from-file)
  "Like nntp-server-opened-hook."
  nntp-server-opened-hook)

;(defvoo nndb-rlogin-parameters '("telnet" "${NNDBSERVER:=localhost}" "9000")
;  "*Parameters to nndb-open-login.  Like nntp-rlogin-parameters."
;  nntp-rlogin-parameters)

;(defvoo nndb-rlogin-user-name nil
;  "*User name for rlogin connect method."
;  nntp-rlogin-user-name)

(defvoo nndb-address "localhost"
  "*The name of the NNDB server."
  nntp-address)

(defvoo nndb-port-number 9000
  "*Port number to connect to."
  nntp-port-number)

;(defvoo nndb-current-group ""
;  "Like nntp-current-group."
;  nntp-current-group)

(defvoo nndb-status-string nil "" nntp-status-string)



(defconst nndb-version "nndb 0.3"
  "Version numbers of this version of NNDB.")


;;; Interface functions.

(nnoo-define-basics nndb)

;; Import other stuff from nntp as is.

(nnoo-import nndb
  (nntp))

;;- maybe this should be mail??
;;-(defun nndb-request-type (group &optional article)
;;-  'news)

;;------------------------------------------------------------------
;;- only new stuff below

; nndb-request-update-info does not exist and is not needed

; nndb-request-update-mark does not exist and is not needed

; nndb-request-scan does not exist
; get new mail from somewhere -- maybe this is not needed?
; --> todo

(deffoo nndb-request-create-group (group &optional server)
  "Creates a group if it doesn't exist yet."
  (nntp-send-command "^[23].*\n" "MKGROUP" group))

; todo -- use some other time than the creation time of the article
; best is time since article has been marked as expirable
(deffoo nndb-request-expire-articles
  (articles &optional group server force)
  "Expires ARTICLES from GROUP on SERVER.
If FORCE, delete regardless of exiration date, otherwise use normal
expiry mechanism."
  (let (msg art)
    (nntp-possibly-change-server group server) ;;-
    (while articles
      (setq art (pop articles))
      (nntp-send-command "^\\([23]\\|^423\\).*\n" "DATE" art)
      (setq msg (nndb-status-message))
      ;; CCC we shouldn't be using the variable nndb-status-string?
      (if (string-match "^423" (nnheader-get-report 'nndb))
          ()
        (or (string-match "\\([0-9]+\\) \\([0-9]+\\)$" msg)
            (error "Not a valid response for DATE command: %s"
                   msg))
        (if (nnmail-expired-article-p
             group
             (list (string-to-int
                    (substring msg (match-beginning 1) (match-end 1)))
                   (string-to-int
                    (substring msg (match-beginning 2) (match-end 2))))
             force)
            (nnheader-message 5 "Deleting article %s in %s..."
                              art group)
          (nntp-send-command "^[23].*\n" "DELETE" art))))))

(deffoo nndb-request-move-article
  (article group server accept-form &optional last)
  "Move ARTICLE (a number) from GROUP on SERVER.
Evals ACCEPT-FORM in current buffer, where the article is.
Optional LAST is ignored."
  (let ((artbuf (get-buffer-create " *nndb move*"))
	result)
    (and
     (nndb-request-article article group server artbuf)
     (save-excursion
       (set-buffer artbuf)
       (setq result (eval accept-form))
       (kill-buffer (current-buffer))
       result)
     (nndb-request-expire-articles (list article)
                                   group
                                   server
                                   t))
    result))
  
(deffoo nndb-request-accept-article (group server &optional last)
  "The article in the current buffer is put into GROUP."
  (nntp-possibly-change-server group server) ;;-
  (let (art statmsg)
    (when (nntp-send-command "^[23].*\r?\n" "ACCEPT" group)
      (nnheader-insert "")
      (nntp-encode-text)
      (nntp-send-region-to-server (point-min) (point-max))
      ;; 1.2a NNTP's post command is buggy. "^M" (\r) is not
      ;;  appended to end of the status message.
      (nntp-wait-for-response "^[23].*\n")
      (setq statmsg (nntp-status-message))
      (or (string-match "^\\([0-9]+\\)" statmsg)
          (error "nndb: %s" statmsg))
      (setq art (substring statmsg
                           (match-beginning 1)
                           (match-end 1)))
      (message "nndb: accepted %s" art)
      (list art))))

(deffoo nndb-request-replace-article (article group buffer)
  "ARTICLE is the number of the article in GROUP to be replaced 
with the contents of the BUFFER."
  (set-buffer buffer)
  (let (art statmsg)
    (when (nntp-send-command "^[23].*\r?\n" "REPLACE" (int-to-string article))
      (nnheader-insert "")
      (nntp-encode-text)
      (nntp-send-region-to-server (point-min) (point-max))
      ;; 1.2a NNTP's post command is buggy. "^M" (\r) is not
      ;;  appended to end of the status message.
      (nntp-wait-for-response "^[23].*\n")
;      (setq statmsg (nntp-status-message))
;      (or (string-match "^\\([0-9]+\\)" statmsg)
;          (error "nndb: %s" statmsg))
;      (setq art (substring statmsg
;                           (match-beginning 1)
;                           (match-end 1)))
;      (message "nndb: replaced %s" art)
      (list (int-to-string article)))))

; nndb-request-delete-group does not exist
; todo -- maybe later

; nndb-request-rename-group does not exist
; todo -- maybe later

(provide 'nndb)


