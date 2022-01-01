;;; nnselect.el --- a virtual group backend   -*- lexical-binding:t -*-

;; Copyright (C) 2020-2022 Free Software Foundation, Inc.

;; Author: Andrew Cohen <cohen@andy.bu.edu>
;; Keywords: news mail

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

;; This is a "virtual" backend that allows an arbitrary list of
;; articles to be treated as a Gnus group.  An nnselect group uses an
;; `nnselect-spec' group parameter to specify this list of
;; articles.  `nnselect-spec' is an alist with two keys:
;; `nnselect-function', whose value should be a function that returns
;; the list of articles, and `nnselect-args'.  The function will be
;; applied to the arguments to generate the list of articles.  The
;; return value should be a vector, each element of which should in
;; turn be a vector of three elements: a real prefixed group name, an
;; article number in that group, and an integer score.  The score is
;; not used by nnselect but may be used by other code to help in
;; sorting.  Most functions will just choose a fixed number, such as
;; 100, for this score.

;; For example the search function `gnus-search-run-query' applied to
;; arguments specifying a search query (see "gnus-search.el") can be
;; used to return a list of articles from a search.  Or the function
;; can be the identity and the args a vector of articles.


;;; Code:

;;; Setup:

(require 'gnus-art)
(require 'gnus-search)

(eval-when-compile (require 'cl-lib))

;; Set up the backend

(nnoo-declare nnselect)

(nnoo-define-basics nnselect)

(gnus-declare-backend "nnselect" 'post-mail 'virtual)

;;; Internal Variables:

(defvar gnus-inhibit-demon)
(defvar gnus-message-group-art)

;; For future use
(defvoo nnselect-directory gnus-directory
  "Directory for the nnselect backend.")

(defvoo nnselect-active-file
    (expand-file-name "nnselect-active" nnselect-directory)
  "nnselect active file.")

(defvoo nnselect-groups-file
    (expand-file-name "nnselect-newsgroups" nnselect-directory)
  "nnselect groups description file.")

;;; Helper routines.
(defun nnselect-compress-artlist (artlist)
  "Compress ARTLIST."
  (let (selection)
    (pcase-dolist (`(,artgroup . ,arts)
                   (nnselect-categorize artlist #'nnselect-artitem-group))
      (let (list)
        (pcase-dolist (`(,rsv . ,articles)
                       (nnselect-categorize
                        arts #'nnselect-artitem-rsv #'nnselect-artitem-number))
          (push (cons rsv (gnus-compress-sequence (sort articles #'<)))
                list))
        (push (cons artgroup list) selection)))
    selection))

(defun nnselect-uncompress-artlist (artlist)
  "Uncompress ARTLIST."
  (if (vectorp artlist)
      artlist
    (let (selection)
      (pcase-dolist (`(,artgroup (,artrsv . ,artseq)) artlist)
	(setq selection
	      (vconcat
	       (cl-map 'vector
                       (lambda (art)
                         (vector artgroup art artrsv))
		    (gnus-uncompress-sequence artseq)) selection)))
      selection)))

(make-obsolete 'nnselect-group-server 'gnus-group-server "28.1")

;; Data type article list.

(define-inline nnselect-artlist-length (artlist)
    (inline-quote (length ,artlist)))

(define-inline nnselect-artlist-article (artlist n)
  "Return from ARTLIST the Nth artitem (counting starting at 1)."
  (inline-quote (when (> ,n 0)
		  (elt ,artlist (1- ,n)))))

(define-inline nnselect-artitem-group (artitem)
  "Return the group from the ARTITEM."
  (inline-quote (elt ,artitem 0)))

(define-inline nnselect-artitem-number (artitem)
  "Return the number from the ARTITEM."
  (inline-quote (elt ,artitem 1)))

(define-inline nnselect-artitem-rsv (artitem)
  "Return the Retrieval Status Value (RSV, score) from the ARTITEM."
  (inline-quote (elt ,artitem 2)))

(define-inline nnselect-article-group (article)
  "Return the group for ARTICLE."
  (inline-quote
   (nnselect-artitem-group  (nnselect-artlist-article
			     gnus-newsgroup-selection ,article))))

(define-inline nnselect-article-number (article)
  "Return the number for ARTICLE."
  (inline-quote (nnselect-artitem-number
		 (nnselect-artlist-article
		  gnus-newsgroup-selection ,article))))

(define-inline nnselect-article-rsv (article)
  "Return the rsv for ARTICLE."
  (inline-quote (nnselect-artitem-rsv
		 (nnselect-artlist-article
		  gnus-newsgroup-selection ,article))))

(define-inline nnselect-article-id (article)
  "Return the pair `(nnselect id . real id)' of ARTICLE."
  (inline-quote (cons ,article (nnselect-article-number ,article))))

(define-inline nnselect-categorize (sequence keyfunc &optional valuefunc)
  "Sorts a sequence into categories.
Returns a list of the form
`((key1 (element11 element12)) (key2 (element21 element22))'.
The category key for a member of the sequence is obtained
as `(keyfunc member)' and the corresponding element is just
`member' (or `(valuefunc member)' if `valuefunc' is non-nil)."
  (inline-letevals (sequence keyfunc valuefunc)
    (inline-quote  (let ((valuefunc (or ,valuefunc 'identity))
			 result)
		     (unless (null ,sequence)
		      (mapc
		       (lambda (member)
			 (let* ((key (funcall ,keyfunc member))
				(value  (funcall valuefunc member))
				(kr (assoc key result)))
			   (if kr
			       (push value (cdr kr))
			     (push (list key value) result))))
		       (reverse ,sequence))
		      result)))))


;; Unclear whether a macro or an inline function is best.
;; (defmacro nnselect-categorize (sequence keyfunc &optional valuefunc)
;;   "Sorts a sequence into categories and returns a list of the form
;; `((key1 (element11 element12)) (key2 (element21 element22))'.
;; The category key for a member of the sequence is obtained
;; as `(keyfunc member)' and the corresponding element is just
;; `member' (or `(valuefunc member)' if `valuefunc' is non-nil)."
;;   (let ((key (make-symbol "key"))
;; 	(value (make-symbol "value"))
;; 	(result (make-symbol "result"))
;; 	(valuefunc (or valuefunc 'identity)))
;;     `(unless (null ,sequence)
;;        (let (,result)
;; 	 (mapc
;; 	  (lambda (member)
;; 	    (let* ((,key (,keyfunc member))
;; 		   (,value  (,valuefunc member))
;; 		   (kr (assoc ,key ,result)))
;; 	      (if kr
;; 		  (push ,value (cdr kr))
;; 		(push (list ,key ,value) ,result))))
;; 	  (reverse ,sequence))
;; 	 ,result))))

(define-inline ids-by-group (articles)
  (inline-quote
   (nnselect-categorize ,articles #'nnselect-article-group
			#'nnselect-article-id)))

(define-inline numbers-by-group (articles &optional type)
  (inline-quote
   (cond
    ((eq ,type 'range)
     (nnselect-categorize (gnus-uncompress-range ,articles)
			  #'nnselect-article-group #'nnselect-article-number))
    ((eq ,type 'tuple)
     (nnselect-categorize ,articles
                  (lambda (elem)
                    (nnselect-article-group (car elem)))
                  (lambda (elem)
                    (cons (nnselect-article-number
                           (car elem))
                          (cdr elem)))))
    (t
     (nnselect-categorize ,articles
			  #'nnselect-article-group
			  #'nnselect-article-number)))))

(defmacro nnselect-add-prefix (group)
  "Ensures that the GROUP has an nnselect prefix."
  `(gnus-group-prefixed-name
   (gnus-group-short-name ,group) '(nnselect "nnselect")))

(defmacro nnselect-get-artlist (group)
  "Retrieve the list of articles for GROUP."
  `(when (gnus-nnselect-group-p ,group)
     (nnselect-uncompress-artlist
      (gnus-group-get-parameter ,group 'nnselect-artlist t))))

(defmacro nnselect-add-novitem (novitem)
  "Add NOVITEM to the list of headers."
  `(let* ((novitem ,novitem)
	  (artno (and novitem
		      (mail-header-number novitem)))
	  (art (car-safe (rassq artno artids))))
     (when art
       (setf (mail-header-number novitem) art)
       (push novitem headers))))

;;; User Customizable Variables:

(defgroup nnselect nil
  "Virtual groups in Gnus with arbitrary selection methods."
  :group 'gnus)

(define-obsolete-variable-alias 'nnir-retrieve-headers-override-function
  'nnselect-retrieve-headers-override-function "28.1")

(defcustom nnselect-retrieve-headers-override-function nil
  "A function that retrieves article headers for ARTICLES from GROUP.
The retrieved headers should populate the `nntp-server-buffer'.
Returns either the retrieved header format 'nov or 'headers.

If this variable is nil, or if the provided function returns nil,
  `gnus-retrieve-headers' will be called instead."
  :version "28.1"
  :type '(repeat function))

;; Gnus backend interface functions.

(deffoo nnselect-open-server (server &optional definitions)
  ;; Just set the server variables appropriately.
  (let ((backend (or (car (gnus-server-to-method server)) 'nnselect)))
    (nnoo-change-server backend server definitions)))

;; (deffoo nnselect-server-opened (&optional server)
;;   "Is SERVER the current virtual server?"
;;   (if (string-empty-p server)
;;       t
;;     (let ((backend (car (gnus-server-to-method server))))
;; 	(nnoo-current-server-p (or backend 'nnselect) server))))

(deffoo nnselect-server-opened (&optional _server)
  t)


(deffoo nnselect-request-group (group &optional _server _dont-check info)
  (let* ((group (nnselect-add-prefix group))
	 (nnselect-artlist (nnselect-get-artlist group))
	 length)
    ;; Check for cached select result or run the selection and cache
    ;; the result.
    (unless nnselect-artlist
      (gnus-group-set-parameter
       group 'nnselect-artlist
       (nnselect-compress-artlist (setq nnselect-artlist
	     (nnselect-run
	      (gnus-group-get-parameter group 'nnselect-specs t)))))
      (nnselect-request-update-info
       group (or info (gnus-get-info group))))
    (if (zerop (setq length (nnselect-artlist-length nnselect-artlist)))
	(progn
	  (nnheader-report 'nnselect "Selection produced empty results.")
	  (when (gnus-ephemeral-group-p group)
	    (gnus-kill-ephemeral-group group)
	    (setq gnus-ephemeral-servers
		  (assq-delete-all 'nnselect gnus-ephemeral-servers)))
	  (nnheader-insert ""))
      (with-current-buffer nntp-server-buffer
	(nnheader-insert "211 %d %d %d %s\n"
                         length    ; total #
                         1         ; first #
                         length    ; last #
                         group))) ; group name
  nnselect-artlist))


(deffoo nnselect-retrieve-headers (articles group &optional _server fetch-old)
  (let ((group (nnselect-add-prefix group)))
    (with-current-buffer (gnus-summary-buffer-name group)
      (setq gnus-newsgroup-selection (or gnus-newsgroup-selection
					 (nnselect-get-artlist group)))
      (let ((gnus-inhibit-demon t)
	    (gartids (ids-by-group articles))
	    headers)
	(with-current-buffer nntp-server-buffer
	  (pcase-dolist (`(,artgroup . ,artids) gartids)
	    (let ((artlist (sort (mapcar #'cdr artids) #'<))
		  (gnus-override-method (gnus-find-method-for-group artgroup))
		  (fetch-old
		   (or
		    (car-safe
		     (gnus-group-find-parameter artgroup
						'gnus-fetch-old-headers t))
		    fetch-old)))
	      (erase-buffer)
	      (pcase (setq gnus-headers-retrieved-by
			   (or
			    (and
			     nnselect-retrieve-headers-override-function
			     (funcall
			      nnselect-retrieve-headers-override-function
			      artlist artgroup))
			    (gnus-retrieve-headers
			     artlist artgroup fetch-old)))
		('nov
		 (goto-char (point-min))
		 (while (not (eobp))
		   (nnselect-add-novitem
		    (nnheader-parse-nov))
		   (forward-line 1)))
		('headers
		 (gnus-run-hooks 'gnus-parse-headers-hook)
		 (let ((nnmail-extra-headers gnus-extra-headers))
		   (goto-char (point-min))
		   (while (not (eobp))
		     (nnselect-add-novitem
		      (nnheader-parse-head))
		     (forward-line 1))))
		((pred listp)
		 (dolist (novitem gnus-headers-retrieved-by)
		   (nnselect-add-novitem novitem)))
		(_ (error "Unknown header type %s while requesting articles \
                    of group %s" gnus-headers-retrieved-by artgroup)))))
	  (setq headers
		(sort
		 headers
		 (lambda (x y)
		   (< (mail-header-number x) (mail-header-number y))))))))))


(deffoo nnselect-request-article (article &optional _group server to-buffer)
  (let* ((gnus-override-method nil)
	 servers group-art artlist)
    (if (numberp article)
	(with-current-buffer gnus-summary-buffer
	  (unless (zerop (nnselect-artlist-length
			  gnus-newsgroup-selection))
	    (setq group-art (cons (nnselect-article-group article)
				  (nnselect-article-number article)))))
      ;; message-id: either coming from a referral or a pseudo-article
      ;; find the servers for a pseudo-article
      (if (eq 'nnselect (car (gnus-server-to-method server)))
	  (with-current-buffer gnus-summary-buffer
	    (let ((thread (gnus-id-to-thread article)))
	      (when thread
		(mapc
		 (lambda (x)
		   (when (and x (> x 0))
		     (cl-pushnew
		      (list
		       (gnus-method-to-server
			(gnus-find-method-for-group
			 (nnselect-article-group x))))
		      servers :test 'equal)))
		 (gnus-articles-in-thread thread)))))
	(setq servers (list (list server))))
      (setq artlist
	    (gnus-search-run-query
	     (list
	      (cons 'search-query-spec
		    (list (cons 'query `((id . ,article)))
			  (cons 'criteria "")  (cons 'shortcut t)))
	      (cons 'search-group-spec servers))))
      (unless (zerop (nnselect-artlist-length artlist))
	(setq
	 group-art
	 (cons
	  (nnselect-artitem-group (nnselect-artlist-article  artlist 1))
	  (nnselect-artitem-number (nnselect-artlist-article  artlist 1))))))
    (when (numberp (cdr group-art))
      (message "Requesting article %d from group %s"
	       (cdr group-art) (car group-art))
      (if to-buffer
	  (with-current-buffer to-buffer
	    (let ((gnus-article-decode-hook nil))
	      (gnus-request-article-this-buffer
	       (cdr group-art) (car group-art))))
	(gnus-request-article (cdr group-art) (car group-art)))
      group-art)))


(deffoo nnselect-request-move-article
    (article _group _server accept-form &optional last _internal-move-group)
  (let* ((artgroup (nnselect-article-group article))
	 (artnumber (nnselect-article-number article))
	 (to-newsgroup (nth 1 accept-form))
	 (to-method (gnus-find-method-for-group to-newsgroup))
	 (from-method (gnus-find-method-for-group artgroup))
	 (move-is-internal (gnus-server-equal from-method to-method)))
    (unless (gnus-check-backend-function
	     'request-move-article artgroup)
      (error "The group %s does not support article moving" artgroup))
    (gnus-request-move-article
     artnumber
     artgroup
     (nth 1 from-method)
     accept-form
     last
     (and move-is-internal
	  to-newsgroup		; Not respooling
	  (gnus-group-real-name to-newsgroup)))))

(deffoo nnselect-request-replace-article
    (article _group buffer &optional no-encode)
  (pcase-let ((`[,artgroup ,artnumber ,artrsv]
	       (with-current-buffer gnus-summary-buffer
		 (nnselect-artlist-article gnus-newsgroup-selection article))))
    (unless (gnus-check-backend-function
	     'request-replace-article artgroup)
      (user-error "The group %s does not support article editing" artgroup))
    (let ((newart
	   (gnus-request-replace-article artnumber artgroup buffer no-encode)))
      (with-current-buffer gnus-summary-buffer
	(cl-nsubstitute `[,artgroup ,newart ,artrsv]
			`[,artgroup ,artnumber ,artrsv]
			gnus-newsgroup-selection
			:test #'equal :count 1)))))

(deffoo nnselect-request-expire-articles
    (articles _group &optional _server force)
  (if force
      (let (not-expired)
	(pcase-dolist (`(,artgroup . ,artids) (ids-by-group articles))
	  (let ((artlist (sort (mapcar #'cdr artids) #'<)))
	    (unless (gnus-check-backend-function 'request-expire-articles
						 artgroup)
	      (error "Group %s does not support article expiration" artgroup))
	    (unless (gnus-check-server (gnus-find-method-for-group artgroup))
	      (error "Couldn't open server for group %s" artgroup))
            (push (mapcar (lambda (art)
                            (car (rassq art artids)))
			  (let ((nnimap-expunge 'immediately))
			    (gnus-request-expire-articles
			     artlist artgroup force)))
		  not-expired)))
	(sort (delq nil not-expired) #'<))
    articles))


(deffoo nnselect-warp-to-article ()
  (let* ((cur (if (> (gnus-summary-article-number) 0)
		  (gnus-summary-article-number)
		(error "Can't warp to a pseudo-article")))
	 (artgroup (nnselect-article-group cur))
         (artnumber (nnselect-article-number cur))
	 (_quit-config (gnus-ephemeral-group-p gnus-newsgroup-name)))

    ;; what should we do here? we could leave all the buffers around
    ;; and assume that we have to exit from them one by one. or we can
    ;; try to clean up directly

    ;;first exit from the nnselect summary buffer.
    ;;(gnus-summary-exit)
    ;; and if the nnselect summary buffer in turn came from another
    ;; summary buffer we have to clean that summary up too.
    ;;(when (not (eq (cdr quit-config) 'group))
    ;;  (gnus-summary-exit))
    (gnus-summary-read-group-1 artgroup t t  nil
                               nil (list artnumber))))


;; we pass this through to the real group in case it wants to adjust
;; the mark. We also use this to mark an article expirable iff it is
;; expirable in the real group.
(deffoo nnselect-request-update-mark (_group article mark)
  (let* ((artgroup (nnselect-article-group article))
	 (artnumber (nnselect-article-number article))
	 (gmark (gnus-request-update-mark artgroup artnumber mark)))
    (when (and artnumber
	       (memq mark gnus-auto-expirable-marks)
	       (= mark gmark)
	       (gnus-group-auto-expirable-p artgroup))
      (setq gmark gnus-expirable-mark))
    gmark))


(deffoo nnselect-request-set-mark (_group actions &optional _server)
  (mapc
   (lambda (request) (gnus-request-set-mark (car request) (cdr request)))
   (nnselect-categorize
    (cl-mapcan
     (lambda (act)
       (cl-destructuring-bind (range action marks) act
	 (mapcar
	  (lambda (artgroup)
	    (list (car artgroup)
		  (gnus-compress-sequence (sort (cdr artgroup) #'<))
		  action marks))
	  (numbers-by-group range 'range))))
     actions)
    #'car #'cdr)))

(deffoo nnselect-request-update-info (group info &optional _server)
  (let* ((group (nnselect-add-prefix group))
	 (gnus-newsgroup-selection
	  (or gnus-newsgroup-selection (nnselect-get-artlist group)))
	 newmarks)
    (gnus-info-set-marks info nil)
    (setf (gnus-info-read info) nil)
    (pcase-dolist (`(,artgroup . ,nartids)
		   (ids-by-group
		    (number-sequence 1 (nnselect-artlist-length
					gnus-newsgroup-selection))))
      (let* ((gnus-newsgroup-active nil)
	     (artids (cl-sort nartids #'< :key 'car))
	     (group-info (gnus-get-info artgroup))
	     (marks (gnus-info-marks group-info))
	     (unread (gnus-uncompress-sequence
		      (gnus-range-difference (gnus-active artgroup)
					     (gnus-info-read group-info)))))
	(setf (gnus-info-read info)
	      (gnus-add-to-range
	       (gnus-info-read info)
	       (delq nil (mapcar
                          (lambda (art)
                            (unless (memq (cdr art) unread) (car art)))
			  artids))))
	(pcase-dolist (`(,type . ,mark-list) marks)
	  (let ((mark-type (gnus-article-mark-to-type type)) new)
	    (when
		(setq new
		      (delq nil
			    (cond
			     ((eq mark-type 'tuple)
			      (mapcar
                               (lambda (id)
                                 (let (mark)
                                   (when
                                       (setq mark (assq (cdr id) mark-list))
                                     (cons (car id) (cdr mark)))))
			       artids))
			     (t
			      (setq mark-list
				    (gnus-uncompress-range mark-list))
			      (mapcar
                               (lambda (id)
                                 (when (memq (cdr id) mark-list)
                                   (car id)))  artids)))))
	      (let ((previous (alist-get type newmarks)))
		(if previous
		    (nconc previous new)
		  (push (cons type new) newmarks))))))))

    ;; Clean up the marks: compress lists;
    (pcase-dolist (`(,type . ,mark-list) newmarks)
      (let ((mark-type (gnus-article-mark-to-type type)))
	(unless (eq mark-type 'tuple)
	  (setf (alist-get type newmarks)
		(gnus-compress-sequence mark-list)))))
    ;; and ensure an unexist key.
    (unless (assq 'unexist newmarks)
      (push (cons 'unexist nil) newmarks))

    (gnus-info-set-marks info newmarks)
    (gnus-set-active group (cons 1 (nnselect-artlist-length
				    gnus-newsgroup-selection)))))


(deffoo nnselect-request-thread (header &optional group server)
  (with-current-buffer gnus-summary-buffer
    (let ((group (nnselect-add-prefix group))
	  ;; find the best group for the originating article. if its a
	  ;; pseudo-article look for real articles in the same thread
	  ;; and see where they come from.
	  (artgroup (nnselect-article-group
		     (if (> (mail-header-number header) 0)
			 (mail-header-number header)
		       (if (> (gnus-summary-article-number) 0)
			   (gnus-summary-article-number)
			 (let ((thread
				(gnus-id-to-thread (mail-header-id header))))
			   (when thread
                             (cl-some (lambda (x)
                                        (when (and x (> x 0)) x))
				      (gnus-articles-in-thread thread)))))))))
      ;; Check if search-based thread referral is permitted, and
      ;; available.
      (if (and gnus-refer-thread-use-search
	       (gnus-search-server-to-engine
		(gnus-method-to-server
		 (gnus-find-method-for-group artgroup))))
	  ;; If so we perform the query, massage the result, and return
	  ;; the new headers back to the caller to incorporate into the
	  ;; current summary buffer.
	  (let* ((group-spec
		  (list (delq nil (list
				   (or server (gnus-group-server artgroup))
				   (unless gnus-refer-thread-use-search
				     artgroup)))))
		 (ids (cons (mail-header-id header)
			    (split-string
			     (or (mail-header-references header)
				 ""))))
		 (query-spec
		  (list (cons 'query (mapconcat (lambda (i)
						  (format "id:%s" i))
						ids " or "))
			(cons 'thread t)))
		 (last (nnselect-artlist-length gnus-newsgroup-selection))
		 (first (1+ last))
		 (new-nnselect-artlist
		  (gnus-search-run-query
		   (list (cons 'search-query-spec query-spec)
			 (cons 'search-group-spec group-spec))))
		 old-arts seq
		 headers)
	    (mapc
             (lambda (article)
               (if
                   (setq seq
                         (cl-position article
                                      gnus-newsgroup-selection :test 'equal))
                   (push (1+ seq) old-arts)
                 (setq gnus-newsgroup-selection
                       (vconcat gnus-newsgroup-selection (vector article)))
                 (cl-incf last)))
	     new-nnselect-artlist)
	    (setq headers
		  (gnus-fetch-headers
		   (append (sort old-arts #'<)
			   (number-sequence first last))
		   nil t))
	    (gnus-group-set-parameter
	     group
	     'nnselect-artlist
	     (nnselect-compress-artlist gnus-newsgroup-selection))
	    (when (>= last first)
	      (let (new-marks)
		(pcase-dolist (`(,artgroup . ,artids)
			       (ids-by-group (number-sequence first last)))
		  (pcase-dolist (`(,type . ,marked)
				 (gnus-info-marks (gnus-get-info artgroup)))
		    (setq marked (gnus-uncompress-sequence marked))
		    (when (setq new-marks
				(delq nil
				      (mapcar
                                       (lambda (art)
                                         (when (memq (cdr art) marked)
                                           (car art)))
				       artids)))
		      (nconc
		       (symbol-value
			(intern
			 (format "gnus-newsgroup-%s"
				 (car (rassq type gnus-article-mark-lists)))))
		       new-marks)))))
	      (setq gnus-newsgroup-active
		    (cons 1 (nnselect-artlist-length gnus-newsgroup-selection)))
	      (gnus-set-active
	       group
	       (cons 1 (nnselect-artlist-length gnus-newsgroup-selection))))
	    headers)
	;; If we can't or won't use search, just warp to the original
	;; group and punt back to gnus-summary-refer-thread.
	(and (gnus-warp-to-article) (gnus-summary-refer-thread))))))


(deffoo nnselect-close-group (group &optional _server)
  (let ((group (nnselect-add-prefix group)))
    (unless gnus-group-is-exiting-without-update-p
      (nnselect-push-info group))
    (setq gnus-newsgroup-selection nil)
    (when (gnus-ephemeral-group-p group)
      (gnus-kill-ephemeral-group group)
      (setq gnus-ephemeral-servers
	    (assq-delete-all 'nnselect gnus-ephemeral-servers)))))


(deffoo nnselect-request-create-group (group &optional _server args)
  (message "Creating nnselect group %s" group)
  (let* ((group (gnus-group-prefixed-name  group '(nnselect "nnselect")))
         (specs (assq 'nnselect-specs args))
         (function-spec
          (or  (alist-get 'nnselect-function specs)
	       (intern (completing-read "Function: " obarray #'functionp))))
         (args-spec
          (or  (alist-get 'nnselect-args specs)
               (read-from-minibuffer "Args: " nil nil t nil "nil")))
         (nnselect-specs (list (cons 'nnselect-function function-spec)
			       (cons 'nnselect-args args-spec))))
    (gnus-group-set-parameter group 'nnselect-specs nnselect-specs)
    (gnus-group-set-parameter
     group 'nnselect-artlist
     (nnselect-compress-artlist (or  (alist-get 'nnselect-artlist args)
         (nnselect-run nnselect-specs))))
    (nnselect-request-update-info group (gnus-get-info group)))
  t)


(deffoo nnselect-request-type (_group &optional article)
  (if (and (numberp article) (> article 0))
      (gnus-request-type
       (nnselect-article-group article) (nnselect-article-number article))
    'unknown))

(deffoo nnselect-request-post (&optional _server)
  (if (not gnus-message-group-art)
      (nnheader-report 'nnselect "Can't post to an nnselect group")
    (gnus-request-post
     (gnus-find-method-for-group
      (nnselect-article-group (cdr gnus-message-group-art))))))


(deffoo nnselect-request-rename-group (_group _new-name &optional _server)
  t)


(deffoo nnselect-request-scan (group _method)
  (when (and group
	     (gnus-group-get-parameter  (nnselect-add-prefix group)
					'nnselect-rescan t))
    (nnselect-request-group-scan group)))


(deffoo nnselect-request-group-scan (group &optional _server _info)
  (let* ((group (nnselect-add-prefix group))
	 (artlist (nnselect-run
		   (gnus-group-get-parameter group 'nnselect-specs t))))
    (gnus-set-active group (cons 1 (nnselect-artlist-length
				    artlist)))
    (gnus-group-set-parameter
     group 'nnselect-artlist
     (nnselect-compress-artlist artlist))))

;; Add any undefined required backend functions

;; (nnoo-define-skeleton nnselect)

;;; Util Code:

(defun gnus-nnselect-group-p (group)
  "Say whether GROUP is nnselect or not."
  (or (and (gnus-group-prefixed-p group)
	   (eq 'nnselect (car (gnus-find-method-for-group group))))
      (eq 'nnselect (car gnus-command-method))))


(defun nnselect-run (specs)
  "Apply nnselect-function to nnselect-args from SPECS.
Return an article list."
  (let ((func (alist-get 'nnselect-function specs))
	(args (alist-get 'nnselect-args specs)))
    (condition-case-unless-debug err
	(funcall func args)
      (error (gnus-error 3 "nnselect-run: %s on %s gave error %s" func args err)
	     []))))

(defun nnselect-search-thread (header)
  "Make an nnselect group containing the thread with article HEADER.
The current server will be searched.  If the registry is
installed, the server that the registry reports the current
article came from is also searched."
  (let* ((ids (cons (mail-header-id header)
		    (split-string
		     (or (mail-header-references header)
			 ""))))
	 (query
	  (list (cons 'query (mapconcat (lambda (i)
					  (format "id:%s" i))
					ids " or "))
		(cons 'thread t)))
	 (server
	  (list (list (gnus-method-to-server
	   (gnus-find-method-for-group gnus-newsgroup-name)))))
	 (registry-group (and
			  (bound-and-true-p gnus-registry-enabled)
			  (car (gnus-registry-get-id-key
				(mail-header-id header) 'group))))
	 (registry-server
	  (and registry-group
	       (gnus-method-to-server
		(gnus-find-method-for-group registry-group)))))
    (when registry-server (cl-pushnew (list registry-server) server
				      :test 'equal))
    (gnus-group-read-ephemeral-group
     (concat "nnselect-" (message-unique-id))
     (list 'nnselect "nnselect")
     nil
     (cons (current-buffer) gnus-current-window-configuration)
					;     nil
     nil nil
     (list
      (cons 'nnselect-specs
	    (list
	     (cons 'nnselect-function 'gnus-search-run-query)
	     (cons 'nnselect-args
		   (list (cons 'search-query-spec query)
			 (cons 'search-group-spec server)))))
      (cons 'nnselect-artlist nil)))
    (gnus-summary-goto-subject (gnus-id-to-article (mail-header-id header)))))



(defun nnselect-push-info (group)
  "Copy mark-lists from GROUP to the originating groups."
  (let ((select-unreads (numbers-by-group gnus-newsgroup-unreads))
	(select-reads (numbers-by-group
		       (gnus-info-read (gnus-get-info group)) 'range))
	(select-unseen (numbers-by-group gnus-newsgroup-unseen))
	(gnus-newsgroup-active nil) mark-list)
    ;; collect the set of marked article lists categorized by
    ;; originating groups
    (pcase-dolist (`(,mark . ,type) gnus-article-mark-lists)
      (let (type-list)
	(when (setq type-list
		    (symbol-value (intern (format "gnus-newsgroup-%s" mark))))
	  (push (cons
		 type
		 (numbers-by-group type-list (gnus-article-mark-to-type type)))
		mark-list))))
    ;; now work on each originating group one at a time
    (pcase-dolist (`(,artgroup . ,artlist)
		   (numbers-by-group gnus-newsgroup-articles))
      (let* ((group-info (gnus-get-info artgroup))
	     (old-unread (gnus-list-of-unread-articles artgroup))
	     newmarked delta-marks)
	(when group-info
	  ;; iterate over mark lists for this group
	  (pcase-dolist (`(,_mark . ,type) gnus-article-mark-lists)
	    (let ((list (cdr (assoc artgroup  (alist-get type mark-list))))
		  (mark-type (gnus-article-mark-to-type type)))

	      ;; When the backend can store marks we collect any
	      ;; changes.  Unlike a normal group the mark lists only
	      ;; include marks for articles we retrieved.
	      (when (and (gnus-check-backend-function
			  'request-set-mark artgroup)
			 (not (gnus-article-unpropagatable-p type)))
		(let* ((old (gnus-list-range-intersection
			     artlist
			     (alist-get type (gnus-info-marks group-info))))
		       (del (gnus-remove-from-range (copy-tree old) list))
		       (add (gnus-remove-from-range (copy-tree list) old)))
		  (when add (push (list add 'add (list type)) delta-marks))
		  (when del
		    ;; Don't delete marks from outside the active range.
		    ;; This shouldn't happen, but is a sanity check.
		    (setq del (gnus-sorted-range-intersection
			       (gnus-active artgroup) del))
		    (push (list del 'del (list type)) delta-marks))))

	      ;; Marked sets are of mark-type 'tuple, 'list, or
	      ;; 'range. We merge the lists with what is already in
	      ;; the original info to get full list of new marks. We
	      ;; do this by removing all the articles we retrieved
	      ;; from the full list, and then add back in the newly
	      ;; marked ones.
	      (cond
	       ((eq mark-type 'tuple)
		;; Get rid of the entries that have the default
		;; score.
		(when (and list (eq type 'score) gnus-save-score)
		  (let* ((arts list)
			 (prev (cons nil list))
			 (all prev))
		    (while arts
		      (if (or (not (consp (car arts)))
			      (= (cdar arts) gnus-summary-default-score))
			  (setcdr prev (cdr arts))
			(setq prev arts))
		      (setq arts (cdr arts)))
		    (setq list (cdr all))))
		;; now merge with the original list and sort just to
		;; make sure
		(setq list
		      (sort (map-merge
			     'list list
			     (alist-get type (gnus-info-marks group-info)))
			    (lambda (elt1 elt2)
			      (< (car elt1) (car elt2))))))
	       (t
		(setq list
		      (gnus-compress-sequence
		       (gnus-sorted-union
			(gnus-sorted-difference
			 (gnus-uncompress-sequence
			  (alist-get type (gnus-info-marks group-info)))
			 artlist)
			(sort list #'<)) t)))

	       ;; When exiting the group, everything that's previously been
	       ;; unseen is now seen.
	       (when (eq  type 'seen)
		 (setq list (gnus-range-add
			     list (cdr (assoc artgroup select-unseen))))))

	      (when (or list (eq  type 'unexist))
		(push (cons  type list) newmarked)))) ;; end of mark-type loop

	  (when delta-marks
	    (unless (gnus-check-group artgroup)
	      (error "Can't open server for %s" artgroup))
	    (gnus-request-set-mark artgroup delta-marks))

	  (gnus-atomic-progn
	    (gnus-info-set-marks group-info newmarked)
	    ;; Cut off the end of the info if there's nothing else there.
	    (let ((i 5))
	      (while (and (> i 2)
			  (not (nth i group-info)))
		(when (nthcdr (cl-decf i) group-info)
		  (setcdr (nthcdr i group-info) nil))))

	    ;; update read and unread
	    (gnus-update-read-articles
	     artgroup
	     (gnus-uncompress-range
	      (gnus-add-to-range
	       (gnus-remove-from-range
		old-unread
		(cdr (assoc artgroup select-reads)))
	       (sort (cdr (assoc artgroup select-unreads)) #'<))))
	    (gnus-get-unread-articles-in-group
	     group-info (gnus-active artgroup) t)
	    (gnus-group-update-group artgroup t t)))))))


(declare-function gnus-registry-get-id-key "gnus-registry" (id key))

(defun gnus-summary-make-search-group (no-parse)
  "Search a group from the summary buffer.
Pass NO-PARSE on to the search engine."
  (interactive "P")
  (gnus-warp-to-article)
  (let ((spec
	 (list
	  (cons 'search-group-spec
		(list (list
		       (gnus-group-server gnus-newsgroup-name)
		       gnus-newsgroup-name))))))
    (gnus-group-make-search-group no-parse spec)))


(provide 'nnselect)

;;; nnselect.el ends here
