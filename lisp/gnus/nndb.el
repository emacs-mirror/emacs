;;; nndb.el --- nndb access for Gnus

;; Copyright (C) 1997, 1998, 2000, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;;         Kai Grossjohann <grossjohann@ls6.informatik.uni-dortmund.de>
;;         Joe Hildebrand <joe.hildebrand@ilg.com>
;;         David Blacka <davidb@rwhois.net>
;; Keywords: news

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

;;; This was based upon Kai Grossjohan's shamessly snarfed code and
;;; further modified by Joe Hildebrand.  It has been updated for Red
;;; Gnus.

;; TODO:
;;
;; * Fix bug where server connection can be lost and impossible to regain
;;   This hasn't happened to me in a while; think it was fixed in Rgnus
;;
;; * make it handle different nndb servers seemlessly
;;
;; * Optimize expire if FORCE
;;
;; * Optimize move (only expire once)
;;
;; * Deal with add/deletion of groups
;;
;; * make the backend TOUCH an article when marked as expireable (will
;;   make article expire 'expiry' days after that moment).

;;; Code:

;; For Emacs < 22.2.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

;;-
;; Register nndb with known select methods.

(require 'gnus-start)
(unless (assoc "nndb" gnus-valid-select-methods)
  (gnus-declare-backend "nndb" 'mail 'respool 'address 'prompt-address))

(require 'nnmail)
(require 'nnheader)
(require 'nntp)
(eval-when-compile (require 'cl))

;; Declare nndb as derived from nntp

(nnoo-declare nndb nntp)

;; Variables specific to nndb

;;- currently not used but just in case...
(defvoo nndb-deliver-program "nndel"
  "*The program used to put a message in an NNDB group.")

(defvoo nndb-server-side-expiry nil
  "If t, expiry calculation will occur on the server side.")

(defvoo nndb-set-expire-date-on-mark nil
  "If t, the expiry date for a given article will be set to the time
it was marked as expireable; otherwise the date will be the time the
article was posted to nndb")

;; Variables copied from nntp

(defvoo nndb-server-opened-hook '(nntp-send-authinfo-from-file)
  "Like nntp-server-opened-hook."
  nntp-server-opened-hook)

(defvoo nndb-address "localhost"
  "*The name of the NNDB server."
  nntp-address)

(defvoo nndb-port-number 9000
  "*Port number to connect to."
  nntp-port-number)

;; change to 'news if you are actually using nndb for news
(defvoo nndb-article-type 'mail)

(defvoo nndb-status-string nil "" nntp-status-string)



(defconst nndb-version "nndb 0.7"
  "Version numbers of this version of NNDB.")


;;; Interface functions.

(nnoo-define-basics nndb)

;;------------------------------------------------------------------

;; this function turns the lisp list into a string list.  There is
;; probably a more efficient way to do this.
(defun nndb-build-article-string (articles)
  (let (art-string art)
    (while articles
      (setq art (pop articles))
      (setq art-string (concat art-string art " ")))
    art-string))

(defun nndb-build-expire-rest-list (total expire)
  (let (art rest)
    (while total
      (setq art (pop total))
      (if (memq art expire)
	  ()
	(push art rest)))
    rest))


;;
(deffoo nndb-request-type (group &optional article)
  nndb-article-type)

;; nndb-request-update-info does not exist and is not needed

;; nndb-request-update-mark does not exist; it should be used to TOUCH
;; articles as they are marked exipirable
(defun nndb-touch-article (group article)
  (nntp-send-command nil "X-TOUCH" article))

(deffoo nndb-request-update-mark
    (group article mark)
  "Sets the expiry date for ARTICLE in GROUP to now, if the mark is 'E'"
  (if (and nndb-set-expire-date-on-mark (string-equal mark "E"))
      (nndb-touch-article group article))
  mark)

;; nndb-request-create-group -- currently this isn't necessary; nndb
;;   creates groups on demand.

;; todo -- use some other time than the creation time of the article
;;         best is time since article has been marked as expirable

(defun nndb-request-expire-articles-local
  (articles &optional group server force)
  "Let gnus do the date check and issue the delete commands."
  (let (msg art delete-list (num-delete 0) rest)
    (nntp-possibly-change-group group server)
    (while articles
      (setq art (pop articles))
      (nntp-send-command "^\\([23]\\|^423\\).*\n" "X-DATE" art)
      (setq msg (nndb-status-message))
      (if (string-match "^423" msg)
	  ()
	(or (string-match "'\\(.+\\)'" msg)
	    (error "Not a valid response for X-DATE command: %s"
		   msg))
	(if (nnmail-expired-article-p
	     group
	     (date-to-time (substring msg (match-beginning 1) (match-end 1)))
	     force)
	    (progn
	      (setq delete-list (concat delete-list " " (int-to-string art)))
	      (setq num-delete  (1+ num-delete)))
	  (push art rest))))
    (if (> (length delete-list) 0)
	(progn
	  (nnheader-message 5 "Deleting %s article(s) from %s"
			    (int-to-string num-delete) group)
	  (nntp-send-command "^[23].*\n" "X-DELETE" delete-list))
      )

    (nnheader-message 5 "")
    (nconc rest articles)))

(defun nndb-get-remote-expire-response ()
  (let (list)
    (set-buffer nntp-server-buffer)
    (goto-char (point-min))
    (if (looking-at "^[34]")
       ;; x-expire returned error--presume no articles were expirable)
	(setq list nil)
      ;; otherwise, pull all of the following numbers into the list
      (re-search-forward "follows\r?\n?" nil t)
      (while (re-search-forward "^[0-9]+$" nil t)
      (push (string-to-number (match-string 0)) list)))
    list))

(defun nndb-request-expire-articles-remote
  (articles &optional group server force)
  "Let the nndb backend expire articles"
  (let (days art-string delete-list (num-delete 0))
    (nntp-possibly-change-group group server)

    ;; first calculate the wait period in days
    (setq days (or (and nnmail-expiry-wait-function
			(funcall nnmail-expiry-wait-function group))
    nnmail-expiry-wait))
    ;; now handle the special cases
    (cond (force
    (setq days 0))
	  ((eq days 'never)
	   ;; This isn't an expirable group.
	  (setq days -1))
	  ((eq days 'immediate)
	  (setq days 0)))


    ;; build article string
    (setq art-string (concat days " " (nndb-build-article-string articles)))
    (nntp-send-command "^\.\r?\n\\|^[345].*\n" "X-EXPIRE" art-string)

    (setq delete-list (nndb-get-remote-expire-response))
    (setq num-delete (length delete-list))
    (if (> num-delete 0)
	(nnheader-message 5 "Deleting %s article(s) from %s"
			  (int-to-string num-delete) group))

    (nndb-build-expire-rest-list articles delete-list)))

(deffoo nndb-request-expire-articles
    (articles &optional group server force)
  "Expires ARTICLES from GROUP on SERVER.
If FORCE, delete regardless of exiration date, otherwise use normal
expiry mechanism."
  (if nndb-server-side-expiry
      (nndb-request-expire-articles-remote articles group server force)
    (nndb-request-expire-articles-local articles group server force)))

;; _Something_ defines it...
(declare-function nndb-request-article "nndb" t t)

(deffoo nndb-request-move-article
    (article group server accept-form &optional last move-is-internal)
  "Move ARTICLE (a number) from GROUP on SERVER.
Evals ACCEPT-FORM in current buffer, where the article is.
Optional LAST is ignored."
  ;; we guess that the second arg in accept-form is the new group,
  ;; which it will be for nndb, which is all that matters anyway
  (let ((new-group (nth 1 accept-form)) result)
    (nntp-possibly-change-group group server)

    ;; use the move command for nndb-to-nndb moves
    (if (string-match "^nndb" new-group)
	(let ((new-group-name (gnus-group-real-name new-group)))
	  (nntp-send-command "^[23].*\n" "X-MOVE" article new-group-name)
	  (cons new-group article))
      ;; else move normally
      (let ((artbuf (get-buffer-create " *nndb move*")))
      (and
       (nndb-request-article article group server artbuf)
       (save-excursion
	 (set-buffer artbuf)
	 (insert-buffer-substring nntp-server-buffer)
	 (setq result (eval accept-form))
	 (kill-buffer (current-buffer))
	 result)
       (nndb-request-expire-articles (list article)
				     group
				     server
				     t))
      result)
      )))

(deffoo nndb-request-accept-article (group server &optional last)
  "The article in the current buffer is put into GROUP."
  (nntp-possibly-change-group group server)
  (let (art msg)
    (when (nntp-send-command "^[23].*\r?\n" "ACCEPT" group)
      (nnheader-insert "")
      (nntp-send-buffer "^[23].*\n"))

    (set-buffer nntp-server-buffer)
    (setq msg (buffer-string))
    (or (string-match "^\\([0-9]+\\)" msg)
	(error "nndb: %s" msg))
    (setq art (substring msg (match-beginning 1) (match-end 1)))
    (nnheader-message 5 "nndb: accepted %s" art)
    (list art)))

(deffoo nndb-request-replace-article (article group buffer)
  "ARTICLE is the number of the article in GROUP to be replaced with the contents of the BUFFER."
  (set-buffer buffer)
  (when (nntp-send-command "^[23].*\r?\n" "X-REPLACE" (int-to-string article))
    (nnheader-insert "")
    (nntp-send-buffer "^[23.*\n")
    (list (int-to-string article))))

			    ; nndb-request-delete-group does not exist
					; todo -- maybe later

			    ; nndb-request-rename-group does not exist
					; todo -- maybe later

;; -- standard compatability functions

(deffoo nndb-status-message (&optional server)
  "Return server status as a string."
  (set-buffer nntp-server-buffer)
  (buffer-string))

;; Import stuff from nntp

(nnoo-import nndb
  (nntp))

(provide 'nndb)

;; arch-tag: 83bd6fb4-58d9-4fed-a901-c6c625ad5f8a
;;; nndb.el ends here
