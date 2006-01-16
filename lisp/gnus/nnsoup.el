;;; nnsoup.el --- SOUP access for Gnus

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
;;   2004, 2005 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; Keywords: news, mail

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

;;; Code:

(require 'nnheader)
(require 'nnmail)
(require 'gnus-soup)
(require 'gnus-msg)
(require 'nnoo)
(eval-when-compile (require 'cl))

(nnoo-declare nnsoup)

(defvoo nnsoup-directory "~/SOUP/"
  "*SOUP packet directory.")

(defvoo nnsoup-tmp-directory
    (cond ((fboundp 'temp-directory) (temp-directory))
	  ((boundp 'temporary-file-directory) temporary-file-directory)
	  ("/tmp/"))
  "*Where nnsoup will store temporary files.")

(defvoo nnsoup-replies-directory (expand-file-name "replies/" nnsoup-directory)
  "*Directory where outgoing packets will be composed.")

(defvoo nnsoup-replies-format-type ?u  ;; u is USENET news format.
  "*Format of the replies packages.")

(defvoo nnsoup-replies-index-type ?n
  "*Index type of the replies packages.")

(defvoo nnsoup-active-file (expand-file-name "active" nnsoup-directory)
  "Active file.")

(defvoo nnsoup-packer "tar cf - %s | gzip > $HOME/Soupin%d.tgz"
  "Format string command for packing a SOUP packet.
The SOUP files will be inserted where the %s is in the string.
This string MUST contain both %s and %d.  The file number will be
inserted where %d appears.")

(defvoo nnsoup-unpacker "gunzip -c %s | tar xvf -"
  "*Format string command for unpacking a SOUP packet.
The SOUP packet file name will be inserted at the %s.")

(defvoo nnsoup-packet-directory "~/"
  "*Where nnsoup will look for incoming packets.")

(defvoo nnsoup-packet-regexp "Soupout"
  "*Regular expression matching SOUP packets in `nnsoup-packet-directory'.")

(defvoo nnsoup-always-save t
  "If non nil commit the reply buffer on each message send.
This is necessary if using message mode outside Gnus with nnsoup as a
backend for the messages.")



(defconst nnsoup-version "nnsoup 0.0"
  "nnsoup version.")

(defvoo nnsoup-status-string "")
(defvoo nnsoup-group-alist nil)
(defvoo nnsoup-current-prefix 0)
(defvoo nnsoup-replies-list nil)
(defvoo nnsoup-buffers nil)
(defvoo nnsoup-current-group nil)
(defvoo nnsoup-group-alist-touched nil)
(defvoo nnsoup-article-alist nil)


;;; Interface functions.

(nnoo-define-basics nnsoup)

(deffoo nnsoup-retrieve-headers (sequence &optional group server fetch-old)
  (nnsoup-possibly-change-group group)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((areas (cddr (assoc nnsoup-current-group nnsoup-group-alist)))
	  (articles sequence)
	  (use-nov t)
	  useful-areas this-area-seq msg-buf)
      (if (stringp (car sequence))
	  ;; We don't support fetching by Message-ID.
	  'headers
	;; We go through all the areas and find which files the
	;; articles in SEQUENCE come from.
	(while (and areas sequence)
	  ;; Peel off areas that are below sequence.
	  (while (and areas (< (cdar (car areas)) (car sequence)))
	    (setq areas (cdr areas)))
	  (when areas
	    ;; This is a useful area.
	    (push (car areas) useful-areas)
	    (setq this-area-seq nil)
	    ;; We take note whether this MSG has a corresponding IDX
	    ;; for later use.
	    (when (or (= (gnus-soup-encoding-index
			  (gnus-soup-area-encoding (nth 1 (car areas)))) ?n)
		      (not (file-exists-p
			    (nnsoup-file
			     (gnus-soup-area-prefix (nth 1 (car areas)))))))
	      (setq use-nov nil))
	    ;; We assign the portion of `sequence' that is relevant to
	    ;; this MSG packet to this packet.
	    (while (and sequence (<= (car sequence) (cdar (car areas))))
	      (push (car sequence) this-area-seq)
	      (setq sequence (cdr sequence)))
	    (setcar useful-areas (cons (nreverse this-area-seq)
				       (car useful-areas)))))

	;; We now have a list of article numbers and corresponding
	;; areas.
	(setq useful-areas (nreverse useful-areas))

	;; Two different approaches depending on whether all the MSG
	;; files have corresponding IDX files.  If they all do, we
	;; simply return the relevant IDX files and let Gnus sort out
	;; what lines are relevant.  If some of the IDX files are
	;; missing, we must return HEADs for all the articles.
	(if use-nov
	    ;; We have IDX files for all areas.
	    (progn
	      (while useful-areas
		(goto-char (point-max))
		(let ((b (point))
		      (number (car (nth 1 (car useful-areas))))
		      (index-buffer (nnsoup-index-buffer
				     (gnus-soup-area-prefix
				      (nth 2 (car useful-areas))))))
		  (when index-buffer
		    (insert-buffer-substring index-buffer)
		    (goto-char b)
		    ;; We have to remove the index number entries and
		    ;; insert article numbers instead.
		    (while (looking-at "[0-9]+")
		      (replace-match (int-to-string number) t t)
		      (incf number)
		      (forward-line 1))))
		(setq useful-areas (cdr useful-areas)))
	      'nov)
	  ;; We insert HEADs.
	  (while useful-areas
	    (setq articles (caar useful-areas)
		  useful-areas (cdr useful-areas))
	    (while articles
	      (when (setq msg-buf
			  (nnsoup-narrow-to-article
			   (car articles) (cdar useful-areas) 'head))
		(goto-char (point-max))
		(insert (format "221 %d Article retrieved.\n" (car articles)))
		(insert-buffer-substring msg-buf)
		(goto-char (point-max))
		(insert ".\n"))
	      (setq articles (cdr articles))))

	  (nnheader-fold-continuation-lines)
	  'headers)))))

(deffoo nnsoup-open-server (server &optional defs)
  (nnoo-change-server 'nnsoup server defs)
  (when (not (file-exists-p nnsoup-directory))
    (condition-case ()
	(make-directory nnsoup-directory t)
      (error t)))
  (cond
   ((not (file-exists-p nnsoup-directory))
    (nnsoup-close-server)
    (nnheader-report 'nnsoup "Couldn't create directory: %s" nnsoup-directory))
   ((not (file-directory-p (file-truename nnsoup-directory)))
    (nnsoup-close-server)
    (nnheader-report 'nnsoup "Not a directory: %s" nnsoup-directory))
   (t
    (nnsoup-read-active-file)
    (nnheader-report 'nnsoup "Opened server %s using directory %s"
		     server nnsoup-directory)
    t)))

(deffoo nnsoup-request-close ()
  (nnsoup-write-active-file)
  (nnsoup-write-replies)
  (gnus-soup-save-areas)
  ;; Kill all nnsoup buffers.
  (let (buffer)
    (while nnsoup-buffers
      (setq buffer (cdr (pop nnsoup-buffers)))
      (and buffer
	   (buffer-name buffer)
	   (kill-buffer buffer))))
  (setq nnsoup-group-alist nil
	nnsoup-group-alist-touched nil
	nnsoup-current-group nil
	nnsoup-replies-list nil)
  (nnoo-close-server 'nnoo)
  t)

(deffoo nnsoup-request-article (id &optional newsgroup server buffer)
  (nnsoup-possibly-change-group newsgroup)
  (let (buf)
    (save-excursion
      (set-buffer (or buffer nntp-server-buffer))
      (erase-buffer)
      (when (and (not (stringp id))
		 (setq buf (nnsoup-narrow-to-article id)))
	(insert-buffer-substring buf)
	t))))

(deffoo nnsoup-request-group (group &optional server dont-check)
  (nnsoup-possibly-change-group group)
  (if dont-check
      t
    (let ((active (cadr (assoc group nnsoup-group-alist))))
      (if (not active)
	  (nnheader-report 'nnsoup "No such group: %s" group)
	(nnheader-insert
	 "211 %d %d %d %s\n"
	 (max (1+ (- (cdr active) (car active))) 0)
	 (car active) (cdr active) group)))))

(deffoo nnsoup-request-type (group &optional article)
  (nnsoup-possibly-change-group group)
  ;; Try to guess the type based on the first article in the group.
  (when (not article)
    (setq article
	  (cdar (car (cddr (assoc group nnsoup-group-alist))))))
  (if (not article)
      'unknown
    (let ((kind (gnus-soup-encoding-kind
		 (gnus-soup-area-encoding
		  (nth 1 (nnsoup-article-to-area
			  article nnsoup-current-group))))))
      (cond ((= kind ?m) 'mail)
	    ((= kind ?n) 'news)
	    (t 'unknown)))))

(deffoo nnsoup-close-group (group &optional server)
  ;; Kill all nnsoup buffers.
  (let ((buffers nnsoup-buffers)
	elem)
    (while buffers
      (when (equal (car (setq elem (pop buffers))) group)
	(setq nnsoup-buffers (delq elem nnsoup-buffers))
	(and (cdr elem) (buffer-name (cdr elem))
	     (kill-buffer (cdr elem))))))
  t)

(deffoo nnsoup-request-list (&optional server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (unless nnsoup-group-alist
      (nnsoup-read-active-file))
    (let ((alist nnsoup-group-alist)
	  (standard-output (current-buffer))
	  entry)
      (while (setq entry (pop alist))
	(insert (car entry) " ")
	(princ (cdadr entry))
	(insert " ")
	(princ (caadr entry))
	(insert " y\n"))
      t)))

(deffoo nnsoup-request-scan (group &optional server)
  (nnsoup-unpack-packets))

(deffoo nnsoup-request-newgroups (date &optional server)
  (nnsoup-request-list))

(deffoo nnsoup-request-list-newsgroups (&optional server)
  nil)

(deffoo nnsoup-request-post (&optional server)
  (nnsoup-store-reply "news")
  t)

(deffoo nnsoup-request-mail (&optional server)
  (nnsoup-store-reply "mail")
  t)

(deffoo nnsoup-request-expire-articles (articles group &optional server force)
  (nnsoup-possibly-change-group group)
  (let* ((total-infolist (assoc group nnsoup-group-alist))
	 (active (cadr total-infolist))
	 (infolist (cddr total-infolist))
	 info range-list mod-time prefix)
    (while infolist
      (setq info (pop infolist)
	    range-list (gnus-uncompress-range (car info))
	    prefix (gnus-soup-area-prefix (nth 1 info)))
      (when;; All the articles in this file are marked for expiry.
	  (and (or (setq mod-time (nth 5 (file-attributes
					  (nnsoup-file prefix))))
		   (setq mod-time (nth 5 (file-attributes
					  (nnsoup-file prefix t)))))
	       (gnus-sublist-p articles range-list)
	       ;; This file is old enough.
	       (nnmail-expired-article-p group mod-time force))
	;; Ok, we delete this file.
	(when (ignore-errors
		(nnheader-message
		 5 "Deleting %s in group %s..." (nnsoup-file prefix)
		 group)
		(when (file-exists-p (nnsoup-file prefix))
		  (delete-file (nnsoup-file prefix)))
		(nnheader-message
		 5 "Deleting %s in group %s..." (nnsoup-file prefix t)
		 group)
		(when (file-exists-p (nnsoup-file prefix t))
		  (delete-file (nnsoup-file prefix t)))
		t)
	  (setcdr (cdr total-infolist) (delq info (cddr total-infolist)))
	  (setq articles (gnus-sorted-difference articles range-list))))
      (when (not mod-time)
	(setcdr (cdr total-infolist) (delq info (cddr total-infolist)))))
    (if (cddr total-infolist)
	(setcar active (caaadr (cdr total-infolist)))
      (setcar active (1+ (cdr active))))
    (nnsoup-write-active-file t)
    ;; Return the articles that weren't expired.
    articles))


;;; Internal functions

(defun nnsoup-possibly-change-group (group &optional force)
  (when (and group
	     (not (equal nnsoup-current-group group)))
    (setq nnsoup-article-alist nil)
    (setq nnsoup-current-group group))
  t)

(defun nnsoup-read-active-file ()
  (setq nnsoup-group-alist nil)
  (when (file-exists-p nnsoup-active-file)
    (ignore-errors
      (load nnsoup-active-file t t t))
    ;; Be backwards compatible.
    (when (and nnsoup-group-alist
	       (not (atom (caadar nnsoup-group-alist))))
      (let ((alist nnsoup-group-alist)
	    entry e min max)
	(while (setq e (cdr (setq entry (pop alist))))
	  (setq min (caaar e))
	  (while (cdr e)
	    (setq e (cdr e)))
	  (setq max (cdar (car e)))
	  (setcdr entry (cons (cons min max) (cdr entry)))))
      (setq nnsoup-group-alist-touched t))
    nnsoup-group-alist))

(defun nnsoup-write-active-file (&optional force)
  (when (and nnsoup-group-alist
	     (or force
		 nnsoup-group-alist-touched))
    (setq nnsoup-group-alist-touched nil)
    (with-temp-file nnsoup-active-file
      (gnus-prin1 `(setq nnsoup-group-alist ',nnsoup-group-alist))
      (insert "\n")
      (gnus-prin1 `(setq nnsoup-current-prefix ,nnsoup-current-prefix))
      (insert "\n"))))

(defun nnsoup-next-prefix ()
  "Return the next free prefix."
  (let (prefix)
    (while (or (file-exists-p
		(nnsoup-file (setq prefix (int-to-string
					   nnsoup-current-prefix))))
	       (file-exists-p (nnsoup-file prefix t)))
      (incf nnsoup-current-prefix))
    (incf nnsoup-current-prefix)
    prefix))

(defun nnsoup-file-name (dir file)
  "Return the full name of FILE (in any case) in DIR."
  (let* ((case-fold-search t)
	 (files (directory-files dir t))
	 (regexp (concat (regexp-quote file) "$")))
    (car (delq nil
	       (mapcar
		(lambda (file)
		  (if (string-match regexp file)
		      file
		    nil))
		files)))))

(defun nnsoup-read-areas ()
  (let ((areas-file (nnsoup-file-name nnsoup-tmp-directory "areas")))
    (when areas-file
      (save-excursion
	(set-buffer nntp-server-buffer)
	(let ((areas (gnus-soup-parse-areas areas-file))
	      entry number area lnum cur-prefix file)
	  ;; Go through all areas in the new AREAS file.
	  (while (setq area (pop areas))
	    ;; Change the name to the permanent name and move the files.
	    (setq cur-prefix (nnsoup-next-prefix))
	    (nnheader-message 5 "Incorporating file %s..." cur-prefix)
	    (when (file-exists-p
		   (setq file
			 (expand-file-name
			  (concat (gnus-soup-area-prefix area) ".IDX")
			  nnsoup-tmp-directory)))
	      (rename-file file (nnsoup-file cur-prefix)))
	    (when (file-exists-p
		   (setq file (expand-file-name
			       (concat (gnus-soup-area-prefix area) ".MSG")
			       nnsoup-tmp-directory)))
	      (rename-file file (nnsoup-file cur-prefix t))
	      (gnus-soup-set-area-prefix area cur-prefix)
	      ;; Find the number of new articles in this area.
	      (setq number (nnsoup-number-of-articles area))
	      (if (not (setq entry (assoc (gnus-soup-area-name area)
					  nnsoup-group-alist)))
		  ;; If this is a new area (group), we just add this info to
		  ;; the group alist.
		  (push (list (gnus-soup-area-name area)
			      (cons 1 number)
			      (list (cons 1 number) area))
			nnsoup-group-alist)
		;; There are already articles in this group, so we add this
		;; info to the end of the entry.
		(nconc entry (list (list (cons (1+ (setq lnum (cdadr entry)))
					       (+ lnum number))
					 area)))
		(setcdr (cadr entry) (+ lnum number))))))
	(nnsoup-write-active-file t)
	(delete-file areas-file)))))

(defun nnsoup-number-of-articles (area)
  (save-excursion
    (cond
     ;; If the number is in the area info, we just return it.
     ((gnus-soup-area-number area)
      (gnus-soup-area-number area))
     ;; If there is an index file, we just count the lines.
     ((/= (gnus-soup-encoding-index (gnus-soup-area-encoding area)) ?n)
      (set-buffer (nnsoup-index-buffer (gnus-soup-area-prefix area)))
      (count-lines (point-min) (point-max)))
     ;; We do it the hard way - re-searching through the message
     ;; buffer.
     (t
      (set-buffer (nnsoup-message-buffer (gnus-soup-area-prefix area)))
      (unless (assoc (gnus-soup-area-prefix area) nnsoup-article-alist)
	(nnsoup-dissect-buffer area))
      (length (cdr (assoc (gnus-soup-area-prefix area)
			  nnsoup-article-alist)))))))

(defun nnsoup-dissect-buffer (area)
  (let ((mbox-delim (concat "^" message-unix-mail-delimiter))
	(format (gnus-soup-encoding-format (gnus-soup-area-encoding area)))
	(i 0)
	alist len)
    (goto-char (point-min))
    (cond
     ;; rnews batch format
     ((or (= format ?u)
	  (= format ?n)) ;; Gnus back compatibility.
      (while (looking-at "^#! *rnews \\(+[0-9]+\\) *$")
	(forward-line 1)
	(push (list
	       (incf i) (point)
	       (progn
		 (forward-char (string-to-number (match-string 1)))
		 (point)))
	      alist)))
     ;; Unix mbox format
     ((= format ?m)
      (while (looking-at mbox-delim)
	(forward-line 1)
	(push (list
	       (incf i) (point)
	       (progn
		 (if (re-search-forward mbox-delim nil t)
		     (beginning-of-line)
		   (goto-char (point-max)))
		 (point)))
	      alist)))
     ;; MMDF format
     ((= format ?M)
      (while (looking-at "\^A\^A\^A\^A\n")
	(forward-line 1)
	(push (list
	       (incf i) (point)
	       (progn
		 (if (search-forward "\n\^A\^A\^A\^A\n" nil t)
		     (beginning-of-line)
		   (goto-char (point-max)))
		 (point)))
	      alist)))
     ;; Binary format
     ((or (= format ?B) (= format ?b))
      (while (not (eobp))
	(setq len (+ (* (char-after (point)) (expt 2.0 24))
		     (* (char-after (+ (point) 1)) (expt 2 16))
		     (* (char-after (+ (point) 2)) (expt 2 8))
		     (char-after (+ (point) 3))))
	(push (list
	       (incf i) (+ (point) 4)
	       (progn
		 (forward-char (floor (+ len 4)))
		 (point)))
	      alist)))
     (t
      (error "Unknown format: %c" format)))
    (push (cons (gnus-soup-area-prefix area) alist) nnsoup-article-alist)))

(defun nnsoup-index-buffer (prefix &optional message)
  (let* ((file (concat prefix (if message ".MSG" ".IDX")))
	 (buffer-name (concat " *nnsoup " file "*")))
    (or (get-buffer buffer-name)	; File already loaded.
	(when (file-exists-p (expand-file-name file nnsoup-directory))
	  (save-excursion		; Load the file.
	    (set-buffer (get-buffer-create buffer-name))
	    (buffer-disable-undo)
	    (push (cons nnsoup-current-group (current-buffer)) nnsoup-buffers)
	    (nnheader-insert-file-contents
	     (expand-file-name file nnsoup-directory))
	    (current-buffer))))))

(defun nnsoup-file (prefix &optional message)
  (expand-file-name
   (concat prefix (if message ".MSG" ".IDX"))
   nnsoup-directory))

(defun nnsoup-message-buffer (prefix)
  (nnsoup-index-buffer prefix 'msg))

(defun nnsoup-unpack-packets ()
  "Unpack all packets in `nnsoup-packet-directory'."
  (let ((packets (directory-files
		  nnsoup-packet-directory t nnsoup-packet-regexp))
	packet)
    (while (setq packet (pop packets))
      (nnheader-message 5 "nnsoup: unpacking %s..." packet)
      (if (not (gnus-soup-unpack-packet
		nnsoup-tmp-directory nnsoup-unpacker packet))
	  (nnheader-message 5 "Couldn't unpack %s" packet)
	(delete-file packet)
	(nnsoup-read-areas)
	(nnheader-message 5 "Unpacking...done")))))

(defun nnsoup-narrow-to-article (article &optional area head)
  (let* ((area (or area (nnsoup-article-to-area article nnsoup-current-group)))
	 (prefix (and area (gnus-soup-area-prefix (nth 1 area))))
	 (msg-buf (and prefix (nnsoup-index-buffer prefix 'msg)))
	 beg end)
    (when area
      (save-excursion
	(cond
	 ;; There is no MSG file.
	 ((null msg-buf)
	  nil)
	 ;; We use the index file to find out where the article
	 ;; begins and ends.
	 ((and (= (gnus-soup-encoding-index
		   (gnus-soup-area-encoding (nth 1 area)))
		  ?c)
	       (file-exists-p (nnsoup-file prefix)))
	  (set-buffer (nnsoup-index-buffer prefix))
	  (widen)
	  (goto-char (point-min))
	  (forward-line (- article (caar area)))
	  (setq beg (read (current-buffer)))
	  (forward-line 1)
	  (if (looking-at "[0-9]+")
	      (progn
		(setq end (read (current-buffer)))
		(set-buffer msg-buf)
		(widen)
		(let ((format (gnus-soup-encoding-format
			       (gnus-soup-area-encoding (nth 1 area)))))
		  (goto-char end)
		  (when (or (= format ?u) (= format ?n) (= format ?m))
		    (setq end (progn (forward-line -1) (point))))))
	    (set-buffer msg-buf))
	  (widen)
	  (narrow-to-region beg (or end (point-max))))
	 (t
	  (set-buffer msg-buf)
	  (widen)
	  (unless (assoc (gnus-soup-area-prefix (nth 1 area))
			 nnsoup-article-alist)
	    (nnsoup-dissect-buffer (nth 1 area)))
	  (let ((entry (assq article (cdr (assoc (gnus-soup-area-prefix
						  (nth 1 area))
						 nnsoup-article-alist)))))
	    (when entry
	      (narrow-to-region (cadr entry) (caddr entry))))))
	(goto-char (point-min))
	(if (not head)
	    ()
	  (narrow-to-region
	   (point-min)
	   (if (search-forward "\n\n" nil t)
	       (1- (point))
	     (point-max))))
	msg-buf))))

;;;###autoload
(defun nnsoup-pack-replies ()
  "Make an outbound package of SOUP replies."
  (interactive)
  (unless (file-exists-p nnsoup-replies-directory)
    (nnheader-message 5 "No such directory: %s" nnsoup-replies-directory))
  ;; Write all data buffers.
  (gnus-soup-save-areas)
  ;; Write the active file.
  (nnsoup-write-active-file)
  ;; Write the REPLIES file.
  (nnsoup-write-replies)
  ;; Check whether there is anything here.
  (when (null (directory-files nnsoup-replies-directory nil "\\.MSG$"))
    (error "No files to pack"))
  ;; Pack all these files into a SOUP packet.
  (gnus-soup-pack nnsoup-replies-directory nnsoup-packer))

(defun nnsoup-write-replies ()
  "Write the REPLIES file."
  (when nnsoup-replies-list
    (gnus-soup-write-replies nnsoup-replies-directory nnsoup-replies-list)
    (setq nnsoup-replies-list nil)))

(defun nnsoup-article-to-area (article group)
  "Return the area that ARTICLE in GROUP is located in."
  (let ((areas (cddr (assoc group nnsoup-group-alist))))
    (while (and areas (< (cdar (car areas)) article))
      (setq areas (cdr areas)))
    (and areas (car areas))))

(defvar nnsoup-old-functions
  (list message-send-mail-real-function message-send-news-function))

;;;###autoload
(defun nnsoup-set-variables ()
  "Use the SOUP methods for posting news and mailing mail."
  (interactive)
  (setq message-send-news-function 'nnsoup-request-post)
  (setq message-send-mail-real-function 'nnsoup-request-mail))

;;;###autoload
(defun nnsoup-revert-variables ()
  "Revert posting and mailing methods to the standard Emacs methods."
  (interactive)
  (setq message-send-mail-real-function (car nnsoup-old-functions))
  (setq message-send-news-function (cadr nnsoup-old-functions)))

(defun nnsoup-store-reply (kind)
  ;; Mostly stolen from `message.el'.
  (require 'mail-utils)
  (let ((tembuf (generate-new-buffer " message temp"))
	(case-fold-search nil)
	delimline
	(mailbuf (current-buffer)))
    (unwind-protect
	(save-excursion
	  (save-restriction
	    (message-narrow-to-headers)
	    (if (equal kind "mail")
		(message-generate-headers message-required-mail-headers)
	      (message-generate-headers message-required-news-headers)))
	  (set-buffer tembuf)
	  (erase-buffer)
	  (insert-buffer-substring mailbuf)
	  ;; Remove some headers.
	  (save-restriction
	    (message-narrow-to-headers)
	    ;; Remove some headers.
	    (message-remove-header message-ignored-mail-headers t))
	  (goto-char (point-max))
	  ;; require one newline at the end.
	  (or (= (preceding-char) ?\n)
	      (insert ?\n))
	  (let ((case-fold-search t))
	    ;; Change header-delimiter to be what sendmail expects.
	    (goto-char (point-min))
	    (re-search-forward
	     (concat "^" (regexp-quote mail-header-separator) "\n"))
	    (replace-match "\n")
	    (backward-char 1)
	    (setq delimline (point-marker))
	    (goto-char (1+ delimline))
	    (let ((msg-buf
		   (gnus-soup-store
		    nnsoup-replies-directory
		    (nnsoup-kind-to-prefix kind) nil nnsoup-replies-format-type
		    nnsoup-replies-index-type))
		  (num 0))
	      (when (and msg-buf (bufferp msg-buf))
		(save-excursion
		  (set-buffer msg-buf)
		  (goto-char (point-min))
		  (while (re-search-forward "^#! *rnews" nil t)
		    (incf num))
		  (when nnsoup-always-save
		    (save-buffer)))
		(nnheader-message 5 "Stored %d messages" num)))
	    (nnsoup-write-replies)
	    (kill-buffer tembuf))))))

(defun nnsoup-kind-to-prefix (kind)
  (unless nnsoup-replies-list
    (setq nnsoup-replies-list
	  (gnus-soup-parse-replies
	   (expand-file-name "REPLIES" nnsoup-replies-directory))))
  (let ((replies nnsoup-replies-list))
    (while (and replies
		(not (string= kind (gnus-soup-reply-kind (car replies)))))
      (setq replies (cdr replies)))
    (if replies
	(gnus-soup-reply-prefix (car replies))
      (push (vector (gnus-soup-unique-prefix nnsoup-replies-directory)
		    kind
		    (format "%c%c%c"
			    nnsoup-replies-format-type
			    nnsoup-replies-index-type
			    (if (string= kind "news")
				?n ?m)))
	    nnsoup-replies-list)
      (gnus-soup-reply-prefix (car nnsoup-replies-list)))))

(defun nnsoup-make-active ()
  "(Re-)create the SOUP active file."
  (interactive)
  (let ((files (sort (directory-files nnsoup-directory t "IDX$")
		     (lambda (f1 f2)
		       (< (progn (string-match "/\\([0-9]+\\)\\." f1)
				 (string-to-number (match-string 1 f1)))
			  (progn (string-match "/\\([0-9]+\\)\\." f2)
				 (string-to-number (match-string 1 f2)))))))
	active group lines ident elem min)
    (set-buffer (get-buffer-create " *nnsoup work*"))
    (while files
      (nnheader-message 5 "Doing %s..." (car files))
      (erase-buffer)
      (nnheader-insert-file-contents (car files))
      (goto-char (point-min))
      (if (not (re-search-forward "^[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t *\\(Xref: \\)? *[^ ]* \\([^ ]+\\):[0-9]" nil t))
	  (setq group "unknown")
	(setq group (match-string 2)))
      (setq lines (count-lines (point-min) (point-max)))
      (setq ident (progn (string-match
			  "/\\([0-9]+\\)\\." (car files))
			 (substring
			  (car files) (match-beginning 1)
			  (match-end 1))))
      (if (not (setq elem (assoc group active)))
	  (push (list group (cons 1 lines)
		      (list (cons 1 lines)
			    (vector ident group "ucm" "" lines)))
		active)
	(nconc elem
	       (list
		(list (cons (1+ (setq min (cdadr elem)))
			    (+ min lines))
		      (vector ident group "ucm" "" lines))))
	(setcdr (cadr elem) (+ min lines)))
      (setq files (cdr files)))
    (nnheader-message 5 "")
    (setq nnsoup-group-alist active)
    (nnsoup-write-active-file t)))

(defun nnsoup-delete-unreferenced-message-files ()
  "Delete any *.MSG and *.IDX files that aren't known by nnsoup."
  (interactive)
  (let* ((known (apply 'nconc (mapcar
			       (lambda (ga)
				 (mapcar
				  (lambda (area)
				    (gnus-soup-area-prefix (cadr area)))
				  (cddr ga)))
			       nnsoup-group-alist)))
	 (regexp "\\.MSG$\\|\\.IDX$")
	 (files (directory-files nnsoup-directory nil regexp))
	 non-files file)
    ;; Find all files that aren't known by nnsoup.
    (while (setq file (pop files))
      (string-match regexp file)
      (unless (member (substring file 0 (match-beginning 0)) known)
	(push file non-files)))
    ;; Sort and delete the files.
    (setq non-files (sort non-files 'string<))
    (map-y-or-n-p "Delete file %s? "
		  (lambda (file) (delete-file
				  (expand-file-name file nnsoup-directory)))
		  non-files)))

(provide 'nnsoup)

;;; arch-tag: b0451389-5703-4450-9425-f66f6b38c828
;;; nnsoup.el ends here
