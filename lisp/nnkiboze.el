;;; nnkiboze.el --- select virtual news access for Gnus
;; Copyright (C) 1995,96 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
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
(require 'gnus-score)
(require 'nnoo)
(eval-when-compile (require 'cl))

(nnoo-declare nnkiboze)
(defvoo nnkiboze-directory gnus-directory
  "nnkiboze will put its files in this directory.")

(defvoo nnkiboze-level 9
  "*The maximum level to be searched for articles.")

(defvoo nnkiboze-remove-read-articles t
  "*If non-nil, nnkiboze will remove read articles from the kiboze group.")



(defconst nnkiboze-version "nnkiboze 1.0"
  "Version numbers of this version of nnkiboze.")

(defvoo nnkiboze-current-group nil)
(defvoo nnkiboze-current-score-group "")
(defvoo nnkiboze-status-string "")



;;; Interface functions.

(nnoo-define-basics nnkiboze)

(deffoo nnkiboze-retrieve-headers (articles &optional group server fetch-old)
  (nnkiboze-possibly-change-newsgroups group)
  (if gnus-nov-is-evil
      nil
    (if (stringp (car articles))
	'headers
      (let ((first (car articles))
	    (last (progn (while (cdr articles) (setq articles (cdr articles)))
			 (car articles)))
	    (nov (nnkiboze-nov-file-name)))
	(if (file-exists-p nov)
	    (save-excursion
	      (set-buffer nntp-server-buffer)
	      (erase-buffer)
	      (insert-file-contents nov)
	      (goto-char (point-min))
	      (while (and (not (eobp)) (< first (read (current-buffer))))
		(forward-line 1))
	      (beginning-of-line)
	      (if (not (eobp)) (delete-region 1 (point)))
	      (while (and (not (eobp)) (>= last (read (current-buffer))))
		(forward-line 1))
	      (beginning-of-line)
	      (if (not (eobp)) (delete-region (point) (point-max)))
	      'nov))))))

(deffoo nnkiboze-open-server (newsgroups &optional something)
  (gnus-make-directory nnkiboze-directory)
  (nnheader-init-server-buffer))

(deffoo nnkiboze-server-opened (&optional server)
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)))

(deffoo nnkiboze-request-article (article &optional newsgroup server buffer)
  (nnkiboze-possibly-change-newsgroups newsgroup)
  (if (not (numberp article))
      ;; This is a real kludge. It might not work at times, but it
      ;; does no harm I think. The only alternative is to offer no
      ;; article fetching by message-id at all.
      (nntp-request-article article newsgroup gnus-nntp-server buffer)
    (let* ((header (gnus-summary-article-header article))
	   (xref (mail-header-xref header))
	   igroup iarticle)
      (or xref (error "nnkiboze: No xref"))
      (or (string-match " \\([^ ]+\\):\\([0-9]+\\)" xref)
	  (error "nnkiboze: Malformed xref"))
      (setq igroup (substring xref (match-beginning 1) (match-end 1)))
      (setq iarticle (string-to-int 
		      (substring xref (match-beginning 2) (match-end 2))))
      (and (gnus-request-group igroup t)
	   (gnus-request-article iarticle igroup buffer)))))

(deffoo nnkiboze-request-group (group &optional server dont-check)
  "Make GROUP the current newsgroup."
  (nnkiboze-possibly-change-newsgroups group)
  (if dont-check
      ()
    (let ((nov-file (nnkiboze-nov-file-name))
	  beg end total)
      (save-excursion
	(set-buffer nntp-server-buffer)
	(erase-buffer)
	(if (not (file-exists-p nov-file))
	    (insert (format "211 0 0 0 %s\n" group))
	  (insert-file-contents nov-file)
	  (if (zerop (buffer-size))
	      (insert (format "211 0 0 0 %s\n" group))
	    (goto-char (point-min))
	    (and (looking-at "[0-9]+") (setq beg (read (current-buffer))))
	    (goto-char (point-max))
	    (and (re-search-backward "^[0-9]" nil t)
		 (setq end (read (current-buffer))))
	    (setq total (count-lines (point-min) (point-max)))
	    (erase-buffer)
	    (insert (format "211 %d %d %d %s\n" total beg end group)))))))
  t)

(deffoo nnkiboze-close-group (group &optional server)
  (nnkiboze-possibly-change-newsgroups group)
  ;; Remove NOV lines of articles that are marked as read.
  (when (and (file-exists-p (nnkiboze-nov-file-name))
	     nnkiboze-remove-read-articles
	     (eq major-mode 'gnus-summary-mode))
    (save-excursion
      (let ((unreads gnus-newsgroup-unreads)
	    (unselected gnus-newsgroup-unselected)
            (version-control 'never))
	(set-buffer (get-buffer-create "*nnkiboze work*"))
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(let ((cur (current-buffer))
	      article)
	  (insert-file-contents (nnkiboze-nov-file-name))
	  (goto-char (point-min))
	  (while (looking-at "[0-9]+")
	    (if (or (memq (setq article (read cur)) unreads)
		    (memq article unselected))
		(forward-line 1)
	      (delete-region (progn (beginning-of-line) (point))
			     (progn (forward-line 1) (point)))))
	  (write-file (nnkiboze-nov-file-name))
	  (kill-buffer (current-buffer)))))
    (setq nnkiboze-current-group nil)))

(deffoo nnkiboze-request-list (&optional server) 
  (nnheader-report 'nnkiboze "LIST is not implemented."))

(deffoo nnkiboze-request-newgroups (date &optional server)
  "List new groups."
  (nnheader-report 'nnkiboze "NEWGROUPS is not supported."))

(deffoo nnkiboze-request-list-newsgroups (&optional server)
  (nnheader-report 'nnkiboze "LIST NEWSGROUPS is not implemented."))

(deffoo nnkiboze-request-delete-group (group &optional force server)
  (nnkiboze-possibly-change-newsgroups group)
  (when force
     (let ((files (list (nnkiboze-nov-file-name)
			(concat nnkiboze-directory group ".newsrc")
			(nnkiboze-score-file group))))
       (while files
	 (and (file-exists-p (car files))
	      (file-writable-p (car files))
	      (delete-file (car files)))
	 (setq files (cdr files)))))
  (setq nnkiboze-current-group nil))


;;; Internal functions.

(defun nnkiboze-possibly-change-newsgroups (group)
  (setq nnkiboze-current-group group))

(defun nnkiboze-prefixed-name (group)
  (gnus-group-prefixed-name group '(nnkiboze "")))

;;;###autoload
(defun nnkiboze-generate-groups ()
  "Usage: emacs -batch -l nnkiboze -f nnkiboze-generate-groups
Finds out what articles are to be part of the nnkiboze groups."
  (interactive)
  (let ((nnmail-spool-file nil)
	(gnus-use-dribble-file nil)
	(gnus-read-active-file t)
	(gnus-expert-user t))
    (gnus))
  (let* ((gnus-newsrc-alist (gnus-copy-sequence gnus-newsrc-alist))
	 (newsrc gnus-newsrc-alist)
	 gnus-newsrc-hashtb)
    (gnus-make-hashtable-from-newsrc-alist)
    ;; We have copied all the newsrc alist info over to local copies
    ;; so that we can mess all we want with these lists.
    (while newsrc
      (if (string-match "nnkiboze" (caar newsrc))
	  ;; For each kiboze group, we call this function to generate
	  ;; it.  
	  (nnkiboze-generate-group (caar newsrc)))
      (setq newsrc (cdr newsrc)))))

(defun nnkiboze-score-file (group)
  (list (expand-file-name
	 (concat (file-name-as-directory gnus-kill-files-directory)
		 (nnheader-translate-file-chars
		  (concat nnkiboze-current-score-group 
			  "." gnus-score-file-suffix))))))

(defun nnkiboze-generate-group (group) 
  (let* ((info (nth 2 (gnus-gethash group gnus-newsrc-hashtb)))
	 (newsrc-file (concat nnkiboze-directory group ".newsrc"))
	 (nov-file (concat nnkiboze-directory group ".nov"))
	 (regexp (nth 1 (nth 4 info)))
	 (gnus-expert-user t)
	 (gnus-large-newsgroup nil)
	 (version-control 'never)
	 (gnus-score-find-score-files-function 'nnkiboze-score-file)
 	 gnus-select-group-hook gnus-summary-prepare-hook 
	 gnus-thread-sort-functions gnus-show-threads 
	 gnus-visual
	 method nnkiboze-newsrc nov-buffer gname newsrc active
	 ginfo lowest glevel)
    (setq nnkiboze-current-score-group group)
    (or info (error "No such group: %s" group))
    ;; Load the kiboze newsrc file for this group.
    (and (file-exists-p newsrc-file) (load newsrc-file))
    ;; We also load the nov file for this group.
    (save-excursion
      (set-buffer (setq nov-buffer (find-file-noselect nov-file)))
      (buffer-disable-undo (current-buffer)))
    ;; Go through the active hashtb and add new all groups that match the 
    ;; kiboze regexp.
    (mapatoms
     (lambda (group)
       (and (string-match regexp (setq gname (symbol-name group))) ; Match
	    (not (assoc gname nnkiboze-newsrc)) ; It isn't registered
	    (numberp (car (symbol-value group))) ; It is active
	    (or (> nnkiboze-level 7)
		(and (setq glevel (nth 1 (nth 2 (gnus-gethash
						 gname gnus-newsrc-hashtb))))
		     (>= nnkiboze-level glevel)))
	    (not (string-match "^nnkiboze:" gname)) ; Exclude kibozes
	    (setq nnkiboze-newsrc 
		  (cons (cons gname (1- (car (symbol-value group))))
			nnkiboze-newsrc))))
     gnus-active-hashtb)
    ;; `newsrc' is set to the list of groups that possibly are
    ;; component groups to this kiboze group.  This list has elements
    ;; on the form `(GROUP . NUMBER)', where NUMBER is the highest
    ;; number that has been kibozed in GROUP in this kiboze group.
    (setq newsrc nnkiboze-newsrc)
    (while newsrc
      (if (not (setq active (gnus-gethash 
			     (caar newsrc) gnus-active-hashtb)))
	  ;; This group isn't active after all, so we remove it from
	  ;; the list of component groups.
	  (setq nnkiboze-newsrc (delq (car newsrc) nnkiboze-newsrc))
	(setq lowest (cdar newsrc))
	;; Ok, we have a valid component group, so we jump to it. 
	(switch-to-buffer gnus-group-buffer)
	(gnus-group-jump-to-group (caar newsrc))
	;; We set all list of article marks to nil.  Since we operate
	;; on copies of the real lists, we can destroy anything we
	;; want here.
	(and (setq ginfo (nth 2 (gnus-gethash (gnus-group-group-name)
					      gnus-newsrc-hashtb)))
	     (nth 3 ginfo)
	     (setcar (nthcdr 3 ginfo) nil))
	;; We set the list of read articles to be what we expect for
	;; this kiboze group -- either nil or `(1 . LOWEST)'. 
	(and ginfo (setcar (nthcdr 2 ginfo)
			   (and (not (= lowest 1)) (cons 1 lowest))))
	(if (not (and (or (not ginfo)
			  (> (length (gnus-list-of-unread-articles 
				      (car ginfo))) 0))
		      (progn
			(gnus-group-select-group nil)
			(eq major-mode 'gnus-summary-mode))))
	    () ; No unread articles, or we couldn't enter this group.
	  ;; We are now in the group where we want to be.
	  (setq method (gnus-find-method-for-group gnus-newsgroup-name))
	  (and (eq method gnus-select-method) (setq method nil))
	  ;; We go through the list of scored articles.
	  (while gnus-newsgroup-scored
	    (if (> (caar gnus-newsgroup-scored) lowest)
		;; If it has a good score, then we enter this article
		;; into the kiboze group.
		(nnkiboze-enter-nov 
		 nov-buffer
		 (gnus-summary-article-header 
		  (caar gnus-newsgroup-scored))
		 (if method
		     (gnus-group-prefixed-name gnus-newsgroup-name method)
		   gnus-newsgroup-name)))
	    (setq gnus-newsgroup-scored (cdr gnus-newsgroup-scored)))
	  ;; That's it.  We exit this group.
	  (gnus-summary-exit-no-update)))
      (setcdr (car newsrc) (car active))
      (setq newsrc (cdr newsrc)))
    ;; We save the nov file.
    (set-buffer nov-buffer)
    (save-buffer)
    (kill-buffer (current-buffer))
    ;; We save the kiboze newsrc for this group.
    (set-buffer (get-buffer-create "*nnkiboze work*"))
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (insert "(setq nnkiboze-newsrc '" (prin1-to-string nnkiboze-newsrc)
	    ")\n")
    (write-file newsrc-file)
    (kill-buffer (current-buffer))
    (switch-to-buffer gnus-group-buffer)
    (gnus-group-list-groups 5 nil)))
    
(defun nnkiboze-enter-nov (buffer header group)
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-max))
    (let ((xref (mail-header-xref header))
	  (prefix (gnus-group-real-prefix group))
	  (first t)
	  article)
      (if (zerop (forward-line -1))
	  (progn
	    (setq article (1+ (read (current-buffer))))
	    (forward-line 1))
	(setq article 1))
      (insert (int-to-string article) "\t"
	      (or (mail-header-subject header) "") "\t"
	      (or (mail-header-from header) "") "\t"
	      (or (mail-header-date header) "") "\t"
	      (or (mail-header-id header) "") "\t"
	      (or (mail-header-references header) "") "\t"
	      (int-to-string (or (mail-header-chars header) 0)) "\t"
	      (int-to-string (or (mail-header-lines header) 0)) "\t")
      (if (or (not xref) (equal "" xref))
	  (insert "Xref: " (system-name) " " group ":" 
		  (int-to-string (mail-header-number header))
		  "\t\n")
	(insert (mail-header-xref header) "\t\n")
	(search-backward "\t" nil t)
	(search-backward "\t" nil t)
	(while (re-search-forward 
		"[^ ]+:[0-9]+"
		(save-excursion (end-of-line) (point)) t)
	  (if first
	      ;; The first xref has to be the group this article
	      ;; really came for - this is the article nnkiboze
	      ;; will request when it is asked for the article.
	      (save-excursion
		(goto-char (match-beginning 0))
		(insert prefix group ":" 
			(int-to-string (mail-header-number header)) " ")
		(setq first nil)))
	  (save-excursion
	    (goto-char (match-beginning 0))
	    (insert prefix)))))))

(defun nnkiboze-nov-file-name ()
  (concat (file-name-as-directory nnkiboze-directory)
	  (nnheader-translate-file-chars
	   (concat (nnkiboze-prefixed-name nnkiboze-current-group) ".nov"))))

(provide 'nnkiboze)

;;; nnkiboze.el ends here
