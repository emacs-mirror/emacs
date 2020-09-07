;;; nnselect.el --- a virtual group backend   -*- lexical-binding:t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

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
;; sorting.  Most functions will just chose a fixed number, such as
;; 100, for this score.

;; For example the search function `nnir-run-query' applied to
;; arguments specifying a search query (see "nnir.el") can be used to
;; return a list of articles from a search.  Or the function can be the
;; identity and the args a vector of articles.


;;; Code:

;;; Setup:

(require 'gnus-art)
(require 'nnir)

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
                   (nnselect-categorize artlist 'nnselect-artitem-group))
      (let (list)
        (pcase-dolist (`(,rsv . ,articles)
                       (nnselect-categorize
                        arts 'nnselect-artitem-rsv 'nnselect-artitem-number))
          (push (cons rsv (gnus-compress-sequence (sort articles '<)))
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
		    #'(lambda (art)
			(vector artgroup art artrsv))
		    (gnus-uncompress-sequence artseq)) selection)))
      selection)))

(defun nnselect-group-server (group)
  "Return the server for GROUP."
  (gnus-group-server group))

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
   (nnselect-categorize ,articles 'nnselect-article-group
			'nnselect-article-id)))

(define-inline numbers-by-group (articles)
  (inline-quote
   (nnselect-categorize
    ,articles 'nnselect-article-group 'nnselect-article-number)))


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
  :version "28.1" :type '(function) :group 'nnselect)

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
	    (let ((artlist (sort (mapcar 'cdr artids) '<))
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
		 (goto-char (point-min))
		 (while (not (eobp))
		   (nnselect-add-novitem
		    (nnheader-parse-head))
		   (forward-line 1)))
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
	    (let ((thread  (gnus-id-to-thread article)))
	      (when thread
		(mapc
		 #'(lambda (x)
		     (when (and x (> x 0))
		       (cl-pushnew
			(list
			 (gnus-method-to-server
			  (gnus-find-method-for-group
			   (nnselect-article-group x)))) servers :test 'equal)))
		 (gnus-articles-in-thread thread)))))
	(setq servers (list (list server))))
      (setq artlist
	    (nnir-run-query
	     (list
	      (cons 'nnir-query-spec
		    (list (cons 'query  (format "HEADER Message-ID %s" article))
		    (cons 'criteria "")  (cons 'shortcut t)))
	      (cons 'nnir-group-spec servers))))
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


(deffoo nnselect-request-expire-articles
    (articles _group &optional _server force)
  (if force
      (let (not-expired)
	(pcase-dolist (`(,artgroup . ,artids) (ids-by-group articles))
	  (let ((artlist (sort (mapcar 'cdr artids) '<)))
	    (unless (gnus-check-backend-function 'request-expire-articles
						 artgroup)
	      (error "Group %s does not support article expiration" artgroup))
	    (unless (gnus-check-server (gnus-find-method-for-group artgroup))
	      (error "Couldn't open server for group %s" artgroup))
	    (push (mapcar #'(lambda (art)
			      (car (rassq art artids)))
			  (let ((nnimap-expunge 'immediately))
			    (gnus-request-expire-articles
			     artlist artgroup force)))
		  not-expired)))
	(sort (delq nil not-expired) '<))
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
		  (gnus-compress-sequence (sort (cdr artgroup) '<))
		  action marks))
	  (numbers-by-group
	   (gnus-uncompress-range range)))))
     actions)
    'car 'cdr)))

(deffoo nnselect-request-update-info (group info &optional _server)
  (let* ((group  (nnselect-add-prefix group))
	 (gnus-newsgroup-selection (or gnus-newsgroup-selection
				       (nnselect-get-artlist group))))
    (gnus-info-set-marks info nil)
    (setf (gnus-info-read info) nil)
    (pcase-dolist (`(,artgroup . ,nartids)
		   (ids-by-group
		    (number-sequence 1 (nnselect-artlist-length
					gnus-newsgroup-selection))))
      (let* ((gnus-newsgroup-active nil)
	     (artids (cl-sort nartids '< :key 'car))
	     (group-info (gnus-get-info artgroup))
	     (marks (gnus-info-marks group-info))
	     (unread (gnus-uncompress-sequence
		      (gnus-range-difference (gnus-active artgroup)
					     (gnus-info-read group-info)))))
	(gnus-atomic-progn
	  (setf (gnus-info-read info)
		(gnus-add-to-range
		 (gnus-info-read info)
		 (delq nil
		       (mapcar
			#'(lambda (art)
			    (unless (memq (cdr art) unread) (car art)))
			artids))))
	  (pcase-dolist (`(,type . ,range) marks)
	    (setq range (gnus-uncompress-sequence range))
	    (gnus-add-marked-articles
	     group type
	     (delq nil
		   (mapcar
		    #'(lambda (art)
			(when (memq (cdr art) range)
			  (car art)))  artids)))))))
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
			     (cl-some #'(lambda (x)
					  (when (and x (> x 0)) x))
				      (gnus-articles-in-thread thread)))))))))
      ;; Check if we are dealing with an imap backend.
      (if (eq 'nnimap
	      (car (gnus-find-method-for-group artgroup)))
	  ;; If so we perform the query, massage the result, and return
	  ;; the new headers back to the caller to incorporate into the
	  ;; current summary buffer.
	  (let* ((group-spec
		  (list (delq nil (list
				   (or server (gnus-group-server artgroup))
				   (unless  gnus-refer-thread-use-search
				     artgroup)))))
		 (query-spec
		  (list (cons 'query (nnimap-make-thread-query header))
			(cons 'criteria "")))
		 (last (nnselect-artlist-length gnus-newsgroup-selection))
		 (first (1+ last))
		 (new-nnselect-artlist
		  (nnir-run-query
		   (list (cons 'nnir-query-spec query-spec)
			 (cons 'nnir-group-spec group-spec))))
		 old-arts seq
		 headers)
	    (mapc
	     #'(lambda (article)
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
		   (append (sort old-arts '<)
			   (number-sequence first last)) nil t))
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
				       #'(lambda (art)
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
	;; If not an imap backend just warp to the original article
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
    (funcall func args)))


(defun nnselect-search-thread (header)
  "Make an nnselect group containing the thread with article HEADER.
The current server will be searched.  If the registry is
installed, the server that the registry reports the current
article came from is also searched."
  (let* ((query
	  (list (cons 'query (nnimap-make-thread-query header))
		(cons 'criteria "")))
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
	     (cons 'nnselect-function 'nnir-run-query)
	     (cons 'nnselect-args
		   (list (cons 'nnir-query-spec query)
			 (cons 'nnir-group-spec server)))))
      (cons 'nnselect-artlist nil)))
    (gnus-summary-goto-subject (gnus-id-to-article (mail-header-id header)))))



(defun nnselect-push-info (group)
  "Copy mark-lists from GROUP to the originating groups."
  (let ((select-unreads (numbers-by-group gnus-newsgroup-unreads))
	(select-reads (numbers-by-group
		       (gnus-uncompress-range
			(gnus-info-read (gnus-get-info group)))))
	(select-unseen (numbers-by-group gnus-newsgroup-unseen))
	(gnus-newsgroup-active nil)
	mark-list type-list)
    (pcase-dolist (`(,mark . ,type) gnus-article-mark-lists)
      (when (setq type-list
		  (symbol-value (intern (format "gnus-newsgroup-%s" mark))))
	(push (cons type
		    (numbers-by-group
		     (gnus-uncompress-range type-list))) mark-list)))
    (pcase-dolist (`(,artgroup . ,artlist)
		   (numbers-by-group gnus-newsgroup-articles))
      (let* ((group-info (gnus-get-info artgroup))
	     (old-unread (gnus-list-of-unread-articles artgroup))
	     newmarked)
	(when group-info
	  (pcase-dolist (`(,_mark . ,type) gnus-article-mark-lists)
	    (let ((select-type
		   (sort
		    (cdr (assoc artgroup  (alist-get type mark-list)))
		    '<))  list)
	      (setq list
		    (gnus-uncompress-range
		     (gnus-add-to-range
		      (gnus-remove-from-range
		       (alist-get type (gnus-info-marks group-info))
		       artlist)
		      select-type)))

	      (when list
		;; Get rid of the entries of the articles that have the
		;; default score.
		(when (and (eq type 'score)
			   gnus-save-score
			   list)
		  (let* ((arts list)
			 (prev (cons nil list))
			 (all prev))
		    (while arts
		      (if (or (not (consp (car arts)))
			      (= (cdar arts) gnus-summary-default-score))
			  (setcdr prev (cdr arts))
			(setq prev arts))
		      (setq arts (cdr arts)))
		    (setq list (cdr all)))))

	      (when  (or (eq (gnus-article-mark-to-type type) 'list)
			 (eq (gnus-article-mark-to-type type) 'range))
		(setq list
		      (gnus-compress-sequence  (sort list '<) t)))

	      ;; When exiting the group, everything that's previously been
	      ;; unseen is now seen.
	      (when (eq  type 'seen)
		(setq list (gnus-range-add
			    list (cdr (assoc artgroup select-unseen)))))

	      (when (or list (eq  type 'unexist))
		(push (cons  type list) newmarked))))

	  (gnus-atomic-progn
	    ;; Enter these new marks into the info of the group.
	    (if (nthcdr 3 group-info)
		(setcar (nthcdr 3 group-info) newmarked)
	      ;; Add the marks lists to the end of the info.
	      (when newmarked
		(setcdr (nthcdr 2 group-info) (list newmarked))))

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
	       (sort (cdr (assoc artgroup select-unreads)) '<))))
	    (gnus-get-unread-articles-in-group
	     group-info (gnus-active artgroup) t)
	    (gnus-group-update-group artgroup t t)))))))


(declare-function gnus-registry-get-id-key "gnus-registry" (id key))

(defun gnus-summary-make-search-group (nnir-extra-parms)
  "Search a group from the summary buffer.
Pass NNIR-EXTRA-PARMS on to the search engine."
  (interactive "P")
  (gnus-warp-to-article)
  (let ((spec
	 (list
	  (cons 'nnir-group-spec
		(list (list
		       (gnus-group-server gnus-newsgroup-name)
		       gnus-newsgroup-name))))))
    (gnus-group-make-search-group nnir-extra-parms spec)))


;; The end.
(provide 'nnselect)

;;; nnselect.el ends here
