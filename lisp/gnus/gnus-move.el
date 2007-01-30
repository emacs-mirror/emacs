;;; gnus-move.el --- commands for moving Gnus from one server to another

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'gnus-start)
(require 'gnus-int)
(require 'gnus-range)

;;;
;;; Moving by comparing Message-ID's.
;;;

;;;###autoload
(defun gnus-change-server (from-server to-server)
  "Move from FROM-SERVER to TO-SERVER.
Update the .newsrc.eld file to reflect the change of nntp server."
  (interactive
   (list gnus-select-method (gnus-read-method "Move to method: ")))

  ;; First start Gnus.
  (let ((gnus-activate-level 0)
	(mail-sources nil)
	(nnmail-spool-file nil))
    (gnus))

  (save-excursion
    ;; Go through all groups and translate.
    (let ((newsrc gnus-newsrc-alist)
	  (nntp-nov-gap nil)
	  info)
      (while (setq info (pop newsrc))
	(when (gnus-group-native-p (gnus-info-group info))
	  (gnus-move-group-to-server info from-server to-server))))))

(defun gnus-move-group-to-server (info from-server to-server)
  "Move group INFO from FROM-SERVER to TO-SERVER."
  (let ((group (gnus-info-group info))
	to-active hashtb type mark marks
	to-article to-reads to-marks article
	act-articles)
    (gnus-message 7 "Translating %s..." group)
    (when (gnus-request-group group nil to-server)
      (setq to-active (gnus-parse-active)
	    hashtb (gnus-make-hashtable 1024)
	    act-articles (gnus-uncompress-range to-active))
      ;; Fetch the headers from the `to-server'.
      (when (and to-active
		 act-articles
		 (setq type (gnus-retrieve-headers
			     act-articles
			     group to-server)))
	;; Convert HEAD headers.  I don't care.
	(when (eq type 'headers)
	  (nnvirtual-convert-headers))
	;; Create a mapping from Message-ID to article number.
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(while (looking-at
		"^[0-9]+\t[^\t]*\t[^\t]*\t[^\t]*\t\\([^\t]*\\)\t")
	  (gnus-sethash
	   (buffer-substring (match-beginning 1) (match-end 1))
	   (read (current-buffer))
	   hashtb)
	  (forward-line 1))
	;; Then we read the headers from the `from-server'.
	(when (and (gnus-request-group group nil from-server)
		   (gnus-active group)
		   (gnus-uncompress-range
		    (gnus-active group))
		   (setq type (gnus-retrieve-headers
			       (gnus-uncompress-range
				(gnus-active group))
			       group from-server)))
	  ;; Make it easier to map marks.
	  (let ((mark-lists (gnus-info-marks info))
		ms type m)
	    (while mark-lists
	      (setq type (caar mark-lists)
		    ms (gnus-uncompress-range (cdr (pop mark-lists))))
	      (while ms
		(if (setq m (assq (car ms) marks))
		    (setcdr m (cons type (cdr m)))
		  (push (list (car ms) type) marks))
		(pop ms))))
	  ;; Convert.
	  (when (eq type 'headers)
	    (nnvirtual-convert-headers))
	  ;; Go through the headers and map away.
	  (set-buffer nntp-server-buffer)
	  (goto-char (point-min))
	  (while (looking-at
		  "^[0-9]+\t[^\t]*\t[^\t]*\t[^\t]*\t\\([^\t]*\\)\t")
	    (when (setq to-article
			(gnus-gethash
			 (buffer-substring (match-beginning 1) (match-end 1))
			 hashtb))
	      ;; Add this article to the list of read articles.
	      (push to-article to-reads)
	      ;; See if there are any marks and then add them.
	      (when (setq mark (assq (read (current-buffer)) marks))
		(setq marks (delq mark marks))
		(setcar mark to-article)
		(push mark to-marks))
	      (forward-line 1)))
	  ;; Now we know what the read articles are and what the
	  ;; article marks are.  We transform the information
	  ;; into the Gnus info format.
	  (setq to-reads
		(gnus-range-add
		 (gnus-compress-sequence
		  (and (setq to-reads (delq nil to-reads))
		       (sort to-reads '<))
		  t)
		 (cons 1 (1- (car to-active)))))
	  (gnus-info-set-read info to-reads)
	  ;; Do the marks.  I'm sure y'all understand what's
	  ;; going on down below, so I won't bother with any
	  ;; further comments.  <duck>
	  (let ((mlists gnus-article-mark-lists)
		lists ms a)
	    (while mlists
	      (push (list (cdr (pop mlists))) lists))
	    (while (setq ms (pop marks))
	      (setq article (pop ms))
	      (while ms
		(setcdr (setq a (assq (pop ms) lists))
			(cons article (cdr a)))))
	    (setq a lists)
	    (while a
	      (setcdr (car a) (gnus-compress-sequence
			       (and (cdar a) (sort (cdar a) '<))))
	      (pop a))
	    (gnus-info-set-marks info lists t)))))
    (gnus-message 7 "Translating %s...done" group)))

(defun gnus-group-move-group-to-server (info from-server to-server)
  "Move the group on the current line from FROM-SERVER to TO-SERVER."
  (interactive
   (let ((info (gnus-get-info (gnus-group-group-name))))
     (list info (gnus-find-method-for-group (gnus-info-group info))
	   (gnus-read-method (format "Move group %s to method: "
				     (gnus-info-group info))))))
  (save-excursion
    (gnus-move-group-to-server info from-server to-server)
    ;; We have to update the group info to point use the right server.
    (gnus-info-set-method info to-server t)
    ;; We also have to change the name of the group and stuff.
    (let* ((group (gnus-info-group info))
	   (new-name (gnus-group-prefixed-name
		      (gnus-group-real-name group) to-server)))
      (gnus-info-set-group info new-name)
      (gnus-sethash new-name (gnus-gethash group gnus-newsrc-hashtb)
		    gnus-newsrc-hashtb)
      (gnus-sethash group nil gnus-newsrc-hashtb))))

(provide 'gnus-move)

;;; arch-tag: 503742b8-7d66-4d79-bb31-4a698070707b
;;; gnus-move.el ends here
