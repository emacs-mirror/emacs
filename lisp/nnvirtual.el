;;; nnvirtual.el --- virtual newsgroups access for Gnus
;; Copyright (C) 1994,95 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; 	Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; The other access methods (nntp, nnspool, etc) are general news
;; access methods. This module relies on Gnus and can not be used
;; separately.

;;; Code:

(require 'nntp)
(require 'nnheader)
(require 'gnus)



(defconst nnvirtual-version "nnvirtual 1.0"
  "Version number of this version of nnvirtual.")

(defvar nnvirtual-group-alist nil)
(defvar nnvirtual-current-group nil)
(defvar nnvirtual-current-groups nil)
(defvar nnvirtual-current-mapping nil)

(defvar nnvirtual-do-not-open nil)

(defvar nnvirtual-status-string "")



;;; Interface functions.

(defun nnvirtual-retrieve-headers (sequence &optional newsgroup server)
  "Retrieve the headers for the articles in SEQUENCE."
  (nnvirtual-possibly-change-newsgroups newsgroup server t)
  (save-excursion
    (set-buffer (get-buffer-create "*virtual headers*"))
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (if (stringp (car sequence))
	'headers
      (let ((map nnvirtual-current-mapping)
	    (offset 0)
	    articles beg group active top article result prefix
	    fetched-articles group-method)
	(while sequence
	  (while (< (car (car map)) (car sequence))
	    (setq offset (car (car map)))
	    (setq map (cdr map)))
	  (setq top (car (car map)))
	  (setq group (nth 1 (car map)))
	  (setq prefix (gnus-group-real-prefix group))
	  (setq active (nth 2 (car map)))
	  (setq articles nil)
	  (while (and sequence (<= (car sequence) top))
	    (setq articles (cons (- (+ active (car sequence)) offset) 
				 articles))
	    (setq sequence (cdr sequence)))
	  (setq articles (nreverse articles))
	  (if (and articles
		   (setq result 
			 (progn
			   (setq group-method 
				 (gnus-find-method-for-group group))
			   (and (or (gnus-server-opened group-method)
				    (gnus-open-server group-method))
				(gnus-request-group group t)
				(gnus-retrieve-headers articles group)))))
	      (save-excursion
		(set-buffer nntp-server-buffer)
		;; If we got HEAD headers, we convert them into NOV
		;; headers. This is slow, inefficient and, come to think
		;; of it, downright evil. So sue me. I couldn't be
		;; bothered to write a header parse routine that could
		;; parse a mixed HEAD/NOV buffer.
		(and (eq result 'headers) (nnvirtual-convert-headers))
		(goto-char (point-min))
		(setq fetched-articles nil)
		(while (not (eobp))
		  (setq beg (point)
			article (read nntp-server-buffer)
			fetched-articles (cons article fetched-articles))
		  (delete-region beg (point))
		  (insert (int-to-string (+ (- article active) offset)))
		  (beginning-of-line)
		  (looking-at 
		   "[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t")
		  (goto-char (match-end 0))
		  (or (search-forward 
		       "\t" (save-excursion (end-of-line) (point)) t)
		      (end-of-line))
		  (while (= (char-after (1- (point))) ? )
		    (forward-char -1)
		    (delete-char 1))
		  (if (eolp)
		      (progn
			(end-of-line)
			(or (= (char-after (1- (point))) ?\t)
			    (insert ?\t))
			(insert (format "Xref: %s %s:%d\t" (system-name) 
					group article)))
		    (if (not (string= "" prefix))
			(while (re-search-forward 
				"[^ ]+:[0-9]+"
				(save-excursion (end-of-line) (point)) t)
			  (save-excursion
			    (goto-char (match-beginning 0))
			    (insert prefix))))
		    (end-of-line)
		    (or (= (char-after (1- (point))) ?\t)
			(insert ?\t)))
		  (forward-line 1))))
	  (goto-char (point-max))
	  (insert-buffer-substring nntp-server-buffer)
	  ;; We have now massaged and inserted the headers from one
	  ;; group. In case some of the articles have expired or been
	  ;; cancelled, we have to mark them as read in the component
	  ;; group. 
	  (let ((unfetched (gnus-sorted-complement 
			    articles (nreverse fetched-articles))))
	    (and unfetched
		 (gnus-group-make-articles-read group unfetched nil))))
	;; The headers are ready for reading, so they are inserted into
	;; the nntp-server-buffer, which is where Gnus expects to find
	;; them.
	(prog1
	    (save-excursion
	      (set-buffer nntp-server-buffer)
	      (erase-buffer)
	      (insert-buffer-substring "*virtual headers*")
	      'nov)
	  (kill-buffer (current-buffer)))))))

(defun nnvirtual-open-server (newsgroups &optional something)
  "Open a virtual newsgroup that contains NEWSGROUPS."
  (nnheader-init-server-buffer))

(defun nnvirtual-close-server (&rest dum)
  "Close news server."
  t)

(defun nnvirtual-request-close ()
  (setq nnvirtual-current-group nil
	nnvirtual-current-groups nil
	nnvirtual-current-mapping nil
	nnvirtual-group-alist nil)
  t)

(defun nnvirtual-server-opened (&optional server)
  "Return server process status, T or NIL.
If the stream is opened, return T, otherwise return NIL."
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)))

(defun nnvirtual-status-message (&optional server)
  "Return server status response as string."
  nnvirtual-status-string)

(defun nnvirtual-request-article (article &optional newsgroup server buffer)
  "Select article by message number."
  (nnvirtual-possibly-change-newsgroups newsgroup server t)
  (and (numberp article)
       (let ((map nnvirtual-current-mapping)
	     (offset 0)
	     group-method)
	 (while (< (car (car map)) article)
	   (setq offset (car (car map)))
	   (setq map (cdr map)))
	 (setq group-method (gnus-find-method-for-group (nth 1 (car map))))
	 (or (gnus-server-opened group-method)
	     (gnus-open-server group-method))
	 (gnus-request-group (nth 1 (car map)) t)
	 (gnus-request-article (- (+ (nth 2 (car map)) article) offset)
			       (nth 1 (car map)) buffer))))

(defun nnvirtual-request-group (group &optional server dont-check)
  "Make GROUP the current newsgroup."
  (nnvirtual-possibly-change-newsgroups group server dont-check)
  (let ((map nnvirtual-current-mapping))
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (if map
	  (progn
	    (while (cdr map)
	      (setq map (cdr map)))
	    (insert (format "211 %d 1 %d %s\n" (car (car map)) 
			    (car (car map)) group))
	    t)
	(setq nnvirtual-status-string "No component groups")
	(setq nnvirtual-current-group nil)
	nil))))
    
(defun nnvirtual-close-group (group &optional server)
  (if (not nnvirtual-current-group)
      ()
    (nnvirtual-possibly-change-newsgroups group server t)
    (nnvirtual-update-marked)
    (setq nnvirtual-current-group nil
	  nnvirtual-current-groups nil
	  nnvirtual-current-mapping nil)
    (setq nnvirtual-group-alist 
	  (delq (assoc group nnvirtual-group-alist) nnvirtual-group-alist))))

(defun nnvirtual-request-list (&optional server) 
  (setq nnvirtual-status-string "nnvirtual: LIST is not implemented.")
  nil)

(defun nnvirtual-request-newgroups (date &optional server)
  "List new groups."
  (setq nnvirtual-status-string "NEWGROUPS is not supported.")
  nil)

(defun nnvirtual-request-list-newsgroups (&optional server)
  (setq nnvirtual-status-string
	"nnvirtual: LIST NEWSGROUPS is not implemented.")
  nil)

(defalias 'nnvirtual-request-post 'nntp-request-post)

(defun nnvirtual-request-post-buffer 
  (post group subject header article-buffer info follow-to respect-poster)
  (nntp-request-post-buffer post "" subject header article-buffer
			    info follow-to respect-poster))


;;; Internal functions.

;; Convert HEAD headers into NOV headers.
(defun nnvirtual-convert-headers ()
  (save-excursion
    (set-buffer nntp-server-buffer)
    (let* ((gnus-newsgroup-dependencies (make-vector 100 0))
	   (headers (gnus-get-newsgroup-headers))
	   header)
      (erase-buffer)
      (while headers
	(setq header (car headers)
	      headers (cdr headers))
	(insert (int-to-string (mail-header-number header)) "\t"
		(or (mail-header-subject header) "") "\t"
		(or (mail-header-from header) "") "\t"
		(or (mail-header-date header) "") "\t"
		(or (mail-header-id header) "") "\t"
		(or (mail-header-references header) "") "\t"
		(int-to-string (or (mail-header-chars header) 0)) "\t"
		(int-to-string (or (mail-header-lines header) 0)) "\t"
		(if (mail-header-xref header) 
		    (concat "Xref: " (mail-header-xref header) "\t")
		  "") "\n")))))

(defun nnvirtual-possibly-change-newsgroups (group regexp &optional check)
  (let ((inf t))
    (or (not group)
	(and nnvirtual-current-group
	     (string= group nnvirtual-current-group))
	(and (setq inf (assoc group nnvirtual-group-alist))
	     (string= (nth 3 inf) regexp)
	     (progn
	       (setq nnvirtual-current-group (car inf))
	       (setq nnvirtual-current-groups (nth 1 inf))
	       (setq nnvirtual-current-mapping (nth 2 inf)))))
    (if (or (not check) (not inf))
	(progn
	  (and inf (setq nnvirtual-group-alist 
			 (delq inf nnvirtual-group-alist)))
	  (setq nnvirtual-current-mapping nil)
	  (setq nnvirtual-current-group group)
	  (let ((newsrc gnus-newsrc-alist)
		(virt-group (gnus-group-prefixed-name 
			     nnvirtual-current-group '(nnvirtual ""))))
	    (setq nnvirtual-current-groups nil)
	    (while newsrc
	      (and (string-match regexp (car (car newsrc)))
		   (not (string= (car (car newsrc)) virt-group))
		   (setq nnvirtual-current-groups
			 (cons (car (car newsrc)) nnvirtual-current-groups)))
	      (setq newsrc (cdr newsrc))))
	  (if nnvirtual-current-groups
	      (progn
		(nnvirtual-create-mapping group)
		(setq nnvirtual-group-alist
		      (cons (list group nnvirtual-current-groups 
				  nnvirtual-current-mapping regexp)
			    nnvirtual-group-alist)))
	    (setq nnvirtual-status-string 
		  (format 
		   "nnvirtual: No newsgroups for this virtual newsgroup"))))))
  nnvirtual-current-groups)

(defun nnvirtual-create-mapping (group)
  (let* ((group (gnus-group-prefixed-name group (list 'nnvirtual "")))
	 (info (nth 2 (gnus-gethash group gnus-newsrc-hashtb)))
	 (groups nnvirtual-current-groups)
	 (offset 0)
	 reads unread igroup itotal ireads)
    ;; The virtual group doesn't exist. (?)
    (or info (error "No such group: %s" group))
    (setq nnvirtual-current-mapping nil)
    (while groups
      ;; Added by Sudish Joseph <joseph@cis.ohio-state.edu>.
      (setq igroup (car groups))
      (let ((info (nth 2 (gnus-gethash igroup gnus-newsrc-hashtb)))
	    (active (gnus-gethash igroup gnus-active-hashtb)))
	;; See if the group has had its active list read this session
	;; if not, we do it now.
	(if (null active)
	    (if (gnus-activate-group igroup)
		(progn
		  (gnus-get-unread-articles-in-group
		   info (gnus-gethash igroup gnus-active-hashtb))
		  (setq active (gnus-gethash igroup gnus-active-hashtb)))
	      (message "Couldn't open component group %s" igroup)))
	(if (null active)
	    ()
	  ;; And then we do the mapping for this component group. If
	  ;; you feel tempted to cast your eyes to the soup below -
	  ;; don't. It'll hurt your soul. Suffice to say that it
	  ;; assigns ranges of nnvirtual article numbers to the
	  ;; different component groups. To get the article number
	  ;; from the nnvirtual number, one does something like
	  ;; (+ (- number offset) (car active)), where `offset' is the
	  ;; slice the mess below assigns, and active is the lowest
	  ;; active article in the component group. 
	  (setq itotal (1+ (- (cdr active) (car active))))
 	  (if (setq ireads (nth 2 info))
	      (let ((itreads
		     (if (not (listp (cdr ireads)))
			 (setq ireads (list (cons (car ireads) (cdr ireads))))
		       (setq ireads (copy-alist ireads)))))
		(if (< (or (and (numberp (car ireads)) (car ireads))
			   (cdr (car ireads))) (car active))
		    (setq ireads (setq itreads (cdr ireads))))
		(if (and ireads (< (or (and (numberp (car ireads))
					    (car ireads))
				       (car (car ireads))) (car active)))
		    (setcar (or (and (numberp (car ireads)) ireads)
				(car ireads)) (1+ (car active))))
		(while itreads
		  (setcar (or (and (numberp (car itreads)) itreads)
			      (car itreads))
			  (+ (max 
			      1 (- (if (numberp (car itreads)) 
				       (car itreads)
				     (car (car itreads)))
				   (car active)))
			     offset))
		  (if (not (numberp (car itreads)))
		      (setcdr (car itreads)
			      (+ (- (cdr (car itreads)) (car active)) offset)))
		  (setq itreads (cdr itreads)))
		(setq reads (nconc reads ireads))))
	  (setq offset (+ offset (1- itotal)))
	  (setq nnvirtual-current-mapping
		(cons (list offset igroup (car active)) 
		      nnvirtual-current-mapping)))
	(setq groups (cdr groups))))
    (setq nnvirtual-current-mapping
	  (nreverse nnvirtual-current-mapping))
    ;; Set Gnus active info.
    (gnus-sethash group (cons 1 (1- offset)) gnus-active-hashtb)
    ;; Set Gnus read info.
    (setcar (nthcdr 2 info) reads)

    ;; Then we deal with the marks.
    (let ((map nnvirtual-current-mapping)
	  (marks '(tick dormant reply expire score))
	  (offset 0)
	  tick dormant reply expire score marked active)
      (while map
	(setq igroup (nth 1 (car map)))
	(setq active (nth 2 (car map)))
	(setq marked (nth 3 (nth 2 (gnus-gethash igroup gnus-newsrc-hashtb))))
	(let ((m marks))
	  (while m
	    (and (assq (car m) marked)
		 (set (car m) 
		      (nconc (mapcar 
			      (lambda (art) 
				(if (numberp art)
				    (if (< art active)
					0 (+ (- art active) offset))
				  (cons (+ (- (car art) active) offset)
					(cdr art))))
			      (cdr (assq (car m) marked)))
			     (symbol-value (car m)))))
	    (setq m (cdr m))))
	(setq offset (car (car map)))
	(setq map (cdr map)))
      ;; Put the list of marked articles in the info of the virtual group.
      (let ((m marks)
	    marked)
	(while m
	  (and (symbol-value (car m))
	       (setq marked (cons (cons (car m) (symbol-value (car m)))
				  marked)))
	  (setq m (cdr m)))
	(if (nthcdr 3 info)
	    (setcar (nthcdr 3 info) marked)
	  (setcdr (nthcdr 2 info) (list marked)))))))

(defun nnvirtual-update-marked ()
  (let ((mark-lists '((gnus-newsgroup-marked . tick)
		      (gnus-newsgroup-dormant . dormant)
		      (gnus-newsgroup-expirable . expire)
		      (gnus-newsgroup-replied . reply)))
	marks art-group group-alist g)
    (while mark-lists
      (setq marks (symbol-value (car (car mark-lists))))
      ;; Find out what groups the mark belong to.
      (while marks
	(setq art-group (nnvirtual-art-group (car marks)))
	(if (setq g (assoc (car art-group) group-alist))
	    (nconc g (list (cdr art-group)))
	  (setq group-alist (cons (list (car art-group) (cdr art-group)) 
				  group-alist)))
	(setq marks (cdr marks)))
      ;; The groups that don't have marks must have no marks. (Yup.)
      (let ((groups nnvirtual-current-groups))
	(while groups
	  (or (assoc (car groups) group-alist)
	      (setq group-alist (cons (list (car groups)) group-alist)))
	  (setq groups (cdr groups))))
      ;; The we update the list of marks.
      (while group-alist
	(gnus-add-marked-articles 
	 (car (car group-alist)) (cdr (car mark-lists)) 
	 (cdr (car group-alist)) nil t)
	(gnus-group-update-group (car (car group-alist)) t)
	(setq group-alist (cdr group-alist)))
      (setq mark-lists (cdr mark-lists)))))

(defun nnvirtual-art-group (article) 
  (let ((map nnvirtual-current-mapping)
	(offset 0))
    (while (< (car (car map)) (if (numberp article) article (car article)))
      (setq offset (car (car map))
	    map (cdr map)))
    (cons (nth 1 (car map))
	  (if (numberp article)
	      (- (+ article (nth 2 (car map))) offset)
	    (cons (- (+ (car article) (nth 2 (car map))) offset)
		  (cdr article))))))

(defun nnvirtual-catchup-group (group &optional server all)
  (nnvirtual-possibly-change-newsgroups group server)
  (let ((gnus-group-marked nnvirtual-current-groups)
	(gnus-expert-user t))
    (save-excursion
      (set-buffer gnus-group-buffer)
      (gnus-group-catchup-current nil all))))

(provide 'nnvirtual)

;;; nnvirtual.el ends here
