;;; nnvirtual.el --- virtual newsgroups access for Gnus
;; Copyright (C) 1994,95,96 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The other access methods (nntp, nnspool, etc) are general news
;; access methods. This module relies on Gnus and can not be used
;; separately.

;;; Code:

(require 'nntp)
(require 'nnheader)
(require 'gnus)
(require 'nnoo)
(eval-when-compile (require 'cl))

(nnoo-declare nnvirtual)

(defvoo nnvirtual-always-rescan nil
  "*If non-nil, always scan groups for unread articles when entering a group.
If this variable is nil (which is the default) and you read articles
in a component group after the virtual group has been activated, the
read articles from the component group will show up when you enter the
virtual group.")

(defvoo nnvirtual-component-regexp nil
  "*Regexp to match component groups.")



(defconst nnvirtual-version "nnvirtual 1.0")

(defvoo nnvirtual-current-group nil)
(defvoo nnvirtual-component-groups nil)
(defvoo nnvirtual-mapping nil)

(defvoo nnvirtual-status-string "")

(eval-and-compile
  (autoload 'gnus-cache-articles-in-group "gnus-cache"))



;;; Interface functions.

(nnoo-define-basics nnvirtual)

(deffoo nnvirtual-retrieve-headers (articles &optional newsgroup
					     server fetch-old)
  (when (nnvirtual-possibly-change-server server)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (if (stringp (car articles))
	  'headers
	(let ((vbuf (nnheader-set-temp-buffer 
		     (get-buffer-create " *virtual headers*")))
	      (unfetched (mapcar (lambda (g) (list g))
				 nnvirtual-component-groups))
	      (system-name (system-name))
	      cgroup article result prefix)
	  (while articles
	    (setq article (assq (pop articles) nnvirtual-mapping))
	    (when (and (setq cgroup (cadr article))
		       (gnus-check-server
			(gnus-find-method-for-group cgroup) t)
		       (gnus-request-group cgroup t))
	      (setq prefix (gnus-group-real-prefix cgroup))
	      (when (setq result (gnus-retrieve-headers 
				  (list (caddr article)) cgroup nil))
		(set-buffer nntp-server-buffer)
		(if (zerop (buffer-size))
		    (nconc (assq cgroup unfetched) (list (caddr article)))
		  ;; If we got HEAD headers, we convert them into NOV
		  ;; headers.  This is slow, inefficient and, come to think
		  ;; of it, downright evil.  So sue me.  I couldn't be
		  ;; bothered to write a header parse routine that could
		  ;; parse a mixed HEAD/NOV buffer.
		  (when (eq result 'headers)
		    (nnvirtual-convert-headers))
		  (goto-char (point-min))
		  (while (not (eobp))
		    (delete-region 
		     (point) (progn (read nntp-server-buffer) (point)))
		    (princ (car article) (current-buffer))
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
			  (insert "Xref: " system-name " " cgroup ":")
			  (princ (caddr article) (current-buffer))
			  (insert "\t"))
		      (insert "Xref: " system-name " " cgroup ":")
		      (princ (caddr article) (current-buffer))
		      (insert " ")
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
		    (forward-line 1))
		  (set-buffer vbuf)
		  (goto-char (point-max))
		  (insert-buffer-substring nntp-server-buffer)))))
	  
	  ;; In case some of the articles have expired or been
	  ;; cancelled, we have to mark them as read in the
	  ;; component group.
	  (while unfetched
	    (when (cdar unfetched)
	      (gnus-group-make-articles-read 
	       (caar unfetched) (sort (cdar unfetched) '<)))
	    (setq unfetched (cdr unfetched)))

	  ;; The headers are ready for reading, so they are inserted into
	  ;; the nntp-server-buffer, which is where Gnus expects to find
	  ;; them.
	  (prog1
	      (save-excursion
		(set-buffer nntp-server-buffer)
		(erase-buffer)
		(insert-buffer-substring vbuf)
		'nov)
	    (kill-buffer vbuf)))))))

(deffoo nnvirtual-request-article (article &optional group server buffer)
  (when (and (nnvirtual-possibly-change-server server)
	     (numberp article))
    (let* ((amap (assq article nnvirtual-mapping))
	   (cgroup (cadr amap)))
      (cond
       ((not amap)
	(nnheader-report 'nnvirtual "No such article: %s" article))
       ((not (gnus-check-group cgroup))
	(nnheader-report
	 'nnvirtual "Can't open server where %s exists" cgroup))
       ((not (gnus-request-group cgroup t))
	(nnheader-report 'nnvirtual "Can't open component group %s" cgroup))
       (t
	(if buffer 
	    (save-excursion
	      (set-buffer buffer)
	      (gnus-request-article-this-buffer (caddr amap) cgroup))
	  (gnus-request-article (caddr amap) cgroup)))))))

(deffoo nnvirtual-open-server (server &optional defs)
  (unless (assq 'nnvirtual-component-regexp defs)
    (push `(nnvirtual-component-regexp ,server)
	  defs))
  (nnoo-change-server 'nnvirtual server defs)
  (if nnvirtual-component-groups
      t
    (setq nnvirtual-mapping nil)
    ;; Go through the newsrc alist and find all component groups.
    (let ((newsrc (cdr gnus-newsrc-alist))
	  group)
      (while (setq group (car (pop newsrc)))
	(when (string-match nnvirtual-component-regexp group) ; Match
	  ;; Add this group to the list of component groups.
	  (setq nnvirtual-component-groups
		(cons group (delete group nnvirtual-component-groups))))))
    (if (not nnvirtual-component-groups)
	(nnheader-report 'nnvirtual "No component groups: %s" server)
      t)))

(deffoo nnvirtual-request-group (group &optional server dont-check)
  (nnvirtual-possibly-change-server server)
  (setq nnvirtual-component-groups
	(delete (nnvirtual-current-group) nnvirtual-component-groups))
  (cond
   ((null nnvirtual-component-groups)
    (setq nnvirtual-current-group nil)
    (nnheader-report 'nnvirtual "No component groups in %s" group))
   (t
    (unless dont-check
      (nnvirtual-create-mapping))
    (setq nnvirtual-current-group group)
    (let ((len (length nnvirtual-mapping)))
      (nnheader-insert "211 %d 1 %d %s\n" len len group)))))

(deffoo nnvirtual-request-type (group &optional article)
  (if (not article)
      'unknown
    (let ((mart (assq article nnvirtual-mapping)))
      (when mart
	(gnus-request-type (cadr mart) (car mart))))))

(deffoo nnvirtual-request-update-mark (group article mark)
  (let* ((nart (assq article nnvirtual-mapping))
	 (cgroup (cadr nart))
	 ;; The component group might be a virtual group.
	 (nmark (gnus-request-update-mark cgroup (caddr nart) mark)))
    (when (and nart
	       (= mark nmark)
	       (gnus-group-auto-expirable-p cgroup))
      (setq mark gnus-expirable-mark)))
  mark)
    
(deffoo nnvirtual-close-group (group &optional server)
  (when (nnvirtual-possibly-change-server server)
    ;; Copy (un)read articles.
    (nnvirtual-update-reads)
    ;; We copy the marks from this group to the component
    ;; groups here.
    (nnvirtual-update-marked))
  t)
    
(deffoo nnvirtual-request-list (&optional server) 
  (nnheader-report 'nnvirtual "LIST is not implemented."))

(deffoo nnvirtual-request-newgroups (date &optional server)
  (nnheader-report 'nnvirtual "NEWGROUPS is not supported."))

(deffoo nnvirtual-request-list-newsgroups (&optional server)
  (nnheader-report 'nnvirtual "LIST NEWSGROUPS is not implemented."))

(deffoo nnvirtual-request-update-info (group info &optional server)
  (when (nnvirtual-possibly-change-server server)
    (let ((map nnvirtual-mapping)
	  (marks (mapcar (lambda (m) (list (cdr m))) gnus-article-mark-lists))
	  reads mr m op)
      ;; Go through the mapping.
      (while map
	(unless (nth 3 (setq m (pop map)))
	  ;; Read article.
	  (push (car m) reads))
	;; Copy marks.
	(when (setq mr (nth 4 m))
	  (while mr
	    (setcdr (setq op (assq (pop mr) marks)) (cons (car m) (cdr op))))))
      ;; Compress the marks and the reads.
      (setq mr marks)
      (while mr
	(setcdr (car mr) (gnus-compress-sequence (sort (cdr (pop mr)) '<))))
      (setcar (cddr info) (gnus-compress-sequence (nreverse reads)))
      ;; Remove empty marks lists.
      (while (and marks (not (cdar marks)))
	(setq marks (cdr marks)))
      (setq mr marks)
      (while (cdr mr)
	(if (cdadr mr)
	    (setq mr (cdr mr))
	  (setcdr mr (cddr mr))))

      ;; Enter these new marks into the info of the group.
      (if (nthcdr 3 info)
	  (setcar (nthcdr 3 info) marks)
	;; Add the marks lists to the end of the info.
	(when marks
	  (setcdr (nthcdr 2 info) (list marks))))
      t)))

(deffoo nnvirtual-catchup-group (group &optional server all)
  (nnvirtual-possibly-change-server server)
  (let ((gnus-group-marked (copy-sequence nnvirtual-component-groups))
	(gnus-expert-user t))
    ;; Make sure all groups are activated.
    (mapcar
     (lambda (g)
       (when (not (numberp (car (gnus-gethash g gnus-newsrc-hashtb))))
	 (gnus-activate-group g)))
     nnvirtual-component-groups)
    (save-excursion
      (set-buffer gnus-group-buffer)
      (gnus-group-catchup-current nil all))))

(deffoo nnvirtual-find-group-art (group article)
  "Return the real group and article for virtual GROUP and ARTICLE."
  (let ((mart (assq article nnvirtual-mapping)))
    (when mart
      (cons (cadr mart) (caddr mart)))))


;;; Internal functions.

(defun nnvirtual-convert-headers ()
  "Convert HEAD headers into NOV headers."
  (save-excursion
    (set-buffer nntp-server-buffer)
    (let* ((dependencies (make-vector 100 0))
	   (headers (gnus-get-newsgroup-headers dependencies))
	   header)
      (erase-buffer)
      (while (setq header (pop headers))
	(nnheader-insert-nov header)))))

(defun nnvirtual-possibly-change-server (server)
  (or (not server)
      (nnoo-current-server-p 'nnvirtual server)
      (nnvirtual-open-server server)))

(defun nnvirtual-update-marked ()
  "Copy marks from the virtual group to the component groups."
  (let ((mark-lists gnus-article-mark-lists)
	(marks (gnus-info-marks (gnus-get-info (nnvirtual-current-group))))
	type list mart cgroups)
    (while (setq type (cdr (pop mark-lists)))
      (setq list (gnus-uncompress-range (cdr (assq type marks))))
      (setq cgroups 
	    (mapcar (lambda (g) (list g)) nnvirtual-component-groups))
      (while list
	(nconc (assoc (cadr (setq mart (assq (pop list) nnvirtual-mapping)))
		      cgroups)
	       (list (caddr mart))))
      (while cgroups
	(gnus-add-marked-articles 
	 (caar cgroups) type (cdar cgroups) nil t)
	(gnus-group-update-group (car (pop cgroups)) t)))))

(defun nnvirtual-update-reads ()
  "Copy (un)reads from the current group to the component groups."
  (let ((groups (mapcar (lambda (g) (list g)) nnvirtual-component-groups))
	(articles (gnus-list-of-unread-articles
		   (nnvirtual-current-group)))
	m)
    (while articles
      (setq m (assq (pop articles) nnvirtual-mapping))
      (nconc (assoc (nth 1 m) groups) (list (nth 2 m))))
    (while groups
      (gnus-update-read-articles (caar groups) (cdr (pop groups))))))

(defun nnvirtual-current-group ()
  "Return the prefixed name of the current nnvirtual group."
  (concat "nnvirtual:" nnvirtual-current-group))

(defsubst nnvirtual-marks (article marks)
  "Return a list of mark types for ARTICLE."
  (let (out)
    (while marks
      (when (memq article (cdar marks))
	(push (caar marks) out))
      (setq marks (cdr marks)))
    out))

(defun nnvirtual-create-mapping ()
  "Create an article mapping for the current group."
  (let* ((div nil)
	 m marks list article unreads marks active
	 (map (sort
	       (apply 
		'nconc
		(mapcar
		 (lambda (g)
		   (when (and (setq active (gnus-activate-group g))
			      (> (cdr active) (car active)))
		     (setq unreads (gnus-list-of-unread-articles g)
			   marks (gnus-uncompress-marks
				  (gnus-info-marks (gnus-get-info g))))
		     (when gnus-use-cache
		       (push (cons 'cache (gnus-cache-articles-in-group g))
			     marks))
		     (setq div (/ (float (car active)) 
				  (if (zerop (cdr active))
				      1 (cdr active))))
		     (mapcar (lambda (n) 
			       (list (* div (- n (car active)))
				     g n (and (memq n unreads) t)
				     (inline (nnvirtual-marks n marks))))
			     (gnus-uncompress-range active))))
		 nnvirtual-component-groups))
	       (lambda (m1 m2)
		 (< (car m1) (car m2)))))
	 (i 0))
    (setq nnvirtual-mapping map)
    ;; Set the virtual article numbers.
    (while (setq m (pop map))
      (setcar m (setq article (incf i))))))

(provide 'nnvirtual)

;;; nnvirtual.el ends here
