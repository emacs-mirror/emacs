;;; nnselect.el --- a virtual group backend   -*- lexical-binding:t -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a "virtual" backend that allows an aribtrary list of
;; articles to be treated as a gnus group. An nnselect group uses an
;; nnselect-spec group parameter to specify this list of
;; articles. nnselect-spec is an alist with two keys:
;; nnselect-function, whose value should be a function that returns
;; the list of articles, and nnselect-args.  The function will be
;; applied to the arguments to generate the list of articles. The
;; return value should be a vector, each element of which should in
;; turn be a vector of three elements: a real prefixed group name, an
;; article number in that group, and an integer score. The score is
;; not used by nnselect but may be used by other code to help in
;; sorting. Most functions will just chose a fixed number, such as
;; 100, for this score.

;; For example the search function `nnir-run-query' applied to
;; arguments specifying a search query (see "nnir.el") can be used to
;; return a list of articles from a search. Or the function can be the
;; identity and the args a vector of articles.


;;; Code:

;;; Setup:


(require 'gnus-art)


;(require 'nnoo)
;(require 'gnus-group)
;(require 'message)
;(require 'gnus-util)
;(require 'gnus-sum)

(eval-when-compile (require 'cl-lib))

;; Set up the backend

(nnoo-declare nnselect)

(nnoo-define-basics nnselect)

(gnus-declare-backend "nnselect" 'mail 'virtual)

;;; Internal Variables:

(defvar gnus-inhibit-demon)
(defvar gnus-message-group-art)

;; (defvar nnselect-artlist nil
;;   "Internal: stores the list of articles.")


;; For future use
(defvoo nnselect-directory gnus-directory
  "Directory for the nnselect backend.")

(defvoo nnselect-active-file
    (expand-file-name "nnselect-active" nnselect-directory)
  "nnselect active file.")

(defvoo nnselect-groups-file
    (expand-file-name "nnselect-newsgroups" nnselect-directory)
  "nnselect groups description file.")


;;; Helper macros

;; Data type article list.

(defmacro nnselect-artlist-length (artlist)
  "Return number of articles in ARTLIST."
  `(length ,artlist))

(defmacro nnselect-artlist-article (artlist n)
  "Return from ARTLIST the Nth artitem (counting starting at 1)."
  `(when (> ,n 0)
     (elt ,artlist (1- ,n))))

(defmacro nnselect-artitem-group (artitem)
  "Return the group from the ARTITEM."
  `(elt ,artitem 0))

(defmacro nnselect-artitem-number (artitem)
  "Return the number from the ARTITEM."
  `(elt ,artitem 1))

(defmacro nnselect-artitem-rsv (artitem)
  "Return the Retrieval Status Value (RSV, score) from the ARTITEM."
  `(elt ,artitem 2))

(defmacro nnselect-article-group (article)
  "Return the group for ARTICLE."
  `(nnselect-artitem-group (nnselect-artlist-article nnselect-artlist ,article)))

(defmacro nnselect-article-number (article)
  "Return the number for ARTICLE."
  `(nnselect-artitem-number (nnselect-artlist-article nnselect-artlist ,article)))

(defmacro nnselect-article-rsv (article)
  "Return the rsv for ARTICLE."
  `(nnselect-artitem-rsv (nnselect-artlist-article nnselect-artlist ,article)))

(defmacro nnselect-article-id (article)
  "Return the pair `(nnselect id . real id)' of ARTICLE."
  `(cons ,article (nnselect-article-number ,article)))

(defmacro ids-by-group (articles)
  `(nnselect-categorize ,articles nnselect-article-group nnselect-article-id))

(defmacro numbers-by-group (articles)
  `(nnselect-categorize ,articles nnselect-article-group nnselect-article-number))

(defmacro nnselect-categorize (sequence keyfunc &optional valuefunc)
  "Sorts a sequence into categories and returns a list of the form
`((key1 (element11 element12)) (key2 (element21 element22))'.
The category key for a member of the sequence is obtained
as `(keyfunc member)' and the corresponding element is just
`member' (or `(valuefunc member)' if `valuefunc' is non-nil)."
  (let ((key (make-symbol "key"))
	(value (make-symbol "value"))
	(result (make-symbol "result"))
	(valuefunc (or valuefunc 'identity)))
    `(unless (null ,sequence)
       (let (,result)
	 (mapc
	  (lambda (member)
	    (let* ((,key (,keyfunc member))
		   (,value  (,valuefunc member))
		   (kr (assoc ,key ,result)))
	      (if kr
		  (push ,value (cadr kr))
		(push (list ,key  (list ,value)) ,result))))
	  ,sequence)
	 ,result))))


;;; User Customizable Variables:

(defgroup nnselect nil
  "Virtual groups in Gnus with arbitrary selection methods."
  :group 'gnus)

(defcustom nnselect-summary-line-format nil
  "The format specification of the lines in an nnselect summary buffer.

All the items from `gnus-summary-line-format' are available, along
with three items unique to nnselect summary buffers:

%Z    Retrieval score value (integer)
%G    Article original full group name (string)
%g    Article original short group name (string)

If nil this will use `gnus-summary-line-format'."
  :version "24.1"
  :type '(string)
  :group 'nnselect)

(defcustom nnselect-retrieve-headers-override-function nil
  "A function that retrieves article headers for ARTICLES from GROUP.
The retrieved headers should populate the `nntp-server-buffer'.
Returns either the retrieved header format 'nov or 'headers.

If this variable is nil, or if the provided function returns nil,
  `gnus-retrieve-headers' will be called instead."
  :version "24.1" :type '(function) :group 'nnselect)


;; Gnus backend interface functions.

(deffoo nnselect-open-server (server &optional definitions)
  ;; Just set the server variables appropriately.
  (let ((backend (car (gnus-server-to-method server))))
    (if backend
	(nnoo-change-server backend server definitions)
      (nnoo-change-server 'nnselect server definitions))))

(deffoo nnselect-request-group (group &optional server dont-check _info)
  (let ((group (nnselect-possibly-change-group group server))
	length)
    ;; Check for cached select result or run the selection and cache
    ;; the result.
    (unless (and nnselect-artlist dont-check)
      (gnus-group-set-parameter
       group 'nnselect-artlist
       (setq nnselect-artlist
	     (nnselect-run
	      (gnus-group-get-parameter group 'nnselect-specs t))))
      (nnselect-request-update-info group (gnus-get-info group)))
    (if (zerop (setq length (nnselect-artlist-length nnselect-artlist)))
	(progn
	  (nnselect-close-group group)
	  (nnheader-report 'nnselect "Selection produced empty results."))
      (with-current-buffer nntp-server-buffer
	(nnheader-insert "211 %d %d %d %s\n"
                         length    ; total #
                         1         ; first #
                         length    ; last #
                         group)))) ; group name
  nnselect-artlist)

(deffoo nnselect-retrieve-headers (articles &optional _group _server fetch-old)
  (let ((gnus-inhibit-demon t)
	(gartids (ids-by-group articles))
	headers)
    (with-current-buffer nntp-server-buffer
      (pcase-dolist (`(,artgroup ,artids) gartids)
	(let ((artlist (sort (mapcar 'cdr artids) '<))
	      (gnus-override-method (gnus-find-method-for-group artgroup))
	      parsefunc)
	  (erase-buffer)
	  (pcase (setq gnus-headers-retrieved-by
		       (or
			(and
			 nnselect-retrieve-headers-override-function
			 (funcall nnselect-retrieve-headers-override-function
				  artlist artgroup))
			(gnus-retrieve-headers artlist artgroup fetch-old)))
	    ('nov
	     (setq parsefunc 'nnheader-parse-nov))
	    ('headers
	     (setq parsefunc 'nnheader-parse-head))
	    (_ (error "Unknown header type %s while requesting articles \
                    of group %s" gnus-headers-retrieved-by artgroup)))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (let* ((novitem (funcall parsefunc))
		   (artno (and novitem
			       (mail-header-number novitem)))
		   (art (car (rassq artno artids))))
	      (when art
		(mail-header-set-number novitem art)
		(push novitem headers))
	      (forward-line 1)))))
    (setq headers
	  (sort headers
		(lambda (x y)
		  (< (mail-header-number x) (mail-header-number y)))))
    (erase-buffer)
    (mapc 'nnheader-insert-nov headers)
    'nov)))

(deffoo nnselect-request-article (article &optional group server to-buffer)
  (nnselect-possibly-change-group group server)
  ;; We shoud only arrive here if we are in an nnselect group and we
  ;; are requesting a real article. Just find the originating
  ;; group for the article and pass the request on.
  (when (numberp article)
    (unless (zerop (nnselect-artlist-length nnselect-artlist))
      (let ((artgroup (nnselect-article-group article))
	    (artnumber (nnselect-article-number article)))
	(message "Requesting article %d from group %s"
		 artnumber artgroup)
	(if to-buffer
	    (with-current-buffer to-buffer
	      (let ((gnus-article-decode-hook nil))
		(gnus-request-article-this-buffer artnumber artgroup)))
	  (gnus-request-article artnumber artgroup))
	(cons artgroup artnumber)))))


(deffoo nnselect-request-move-article (article group server accept-form
					   &optional last _internal-move-group)
  (nnselect-possibly-change-group group server)
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


(deffoo nnselect-request-expire-articles (articles group &optional server force)
  (nnselect-possibly-change-group group server)
  (if force
      (let (not-expired)
	(pcase-dolist (`(,artgroup ,artids) (ids-by-group articles))
	  (let ((artlist (sort (mapcar 'cdr artids) '<)))
	    (unless (gnus-check-backend-function 'request-expire-articles
						 artgroup)
	      (error "Group %s does not support article expiration" artgroup))
	    (unless (gnus-check-server (gnus-find-method-for-group artgroup))
	      (error "Couldn't open server for group %s" artgroup))
	    (push (mapcar #'(lambda (art)
			      (car (rassq art artids)))
			  (gnus-request-expire-articles
			   artlist artgroup force))
		  not-expired)))
	(sort (delq nil not-expired) '<))
    articles))

(deffoo nnselect-warp-to-article ()
  (nnselect-possibly-change-group gnus-newsgroup-name)
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
;    (gnus-summary-exit)
    ;; and if the nnselect summary buffer in turn came from another
    ;; summary buffer we have to clean that summary up too.
 ;   (when (not (eq (cdr quit-config) 'group))
;      (gnus-summary-exit))
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

(deffoo nnselect-request-set-mark (group actions &optional server)
  (nnselect-possibly-change-group group server)
  (mapc
   (lambda (request) (gnus-request-set-mark (car request) (cadr request)))
   (nnselect-categorize
    (cl-mapcan
     (lambda (act)
       (cl-destructuring-bind (range action marks) act
	 (mapcar
	  (lambda (artgroup)
	    (list (car artgroup)
		  (list (gnus-compress-sequence (sort (cadr artgroup) '<))
			action marks)))
	  (numbers-by-group
	   (gnus-uncompress-range range)))))
     actions)
    car cadr)))

(deffoo nnselect-request-update-info (group info &optional server)
  (let ((group  (nnselect-possibly-change-group group server)))
    (gnus-info-set-marks info nil)
    (gnus-info-set-read info nil)
    (pcase-dolist (`(,artgroup ,nartids)
		   (ids-by-group
		    (number-sequence
		     1 (nnselect-artlist-length nnselect-artlist))))
      (let* ((gnus-newsgroup-active nil)
	     (artids (cl-sort nartids '< :key 'car))
	     (group-info (gnus-get-info artgroup))
	     (marks (gnus-info-marks group-info))
	     (read (gnus-uncompress-sequence (gnus-info-read group-info))))
	(gnus-atomic-progn
	  (gnus-info-set-read
	   info
	   (gnus-add-to-range
	    (gnus-info-read info)
	    (delq nil
		  (mapcar
		   #'(lambda (art)
		       (when (member (cdr art) read) (car art)))
		   artids))))
	  (pcase-dolist (`(,type . ,range) marks)
	    (setq range (gnus-uncompress-sequence range))
	    (gnus-add-marked-articles
	     group type
	     (delq nil
		   (mapcar
		    #'(lambda (art)
			(when (member (cdr art) range)
			  (car art)))  artids))))))))
  (gnus-set-active group (cons 1 (nnselect-artlist-length nnselect-artlist))))

(declare-function nnir-run-query "nnir" (specs))
(deffoo nnselect-request-thread (header &optional group server)
  (let ((group (nnselect-possibly-change-group group server))
	(artgroup (nnselect-article-group
		   (if (> (mail-header-number header) 0)
		       (mail-header-number header)
		     (with-current-buffer gnus-summary-buffer
		       (if (> (gnus-summary-article-number) 0)
			   (gnus-summary-article-number)
			 (let ((thread
				(gnus-id-to-thread (mail-header-id header))))
			   (when thread
			     (cl-some #'(lambda (x)
					  (when (and x (> x 0)) x))
				      (gnus-articles-in-thread thread))))))))))
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
				   (list artgroup))))))
	       (query-spec
		(list (cons 'query (nnimap-make-thread-query header))
		      (cons 'criteria "")))
	       (last (nnselect-artlist-length nnselect-artlist))
	       (first (1+ last))
	       (new-nnselect-artlist
		(nnir-run-query
		 (list (cons 'nnir-query-spec query-spec)
		       (cons 'nnir-group-spec group-spec))))
	       old-arts seq
	       headers)
	  ;; The search will likely find articles that are already
	  ;; present in the nnselect summary buffer. We remove these from
	  ;; the search result. However even though these articles are
	  ;; in the original article list their headers may not have
	  ;; been retrieved, so we retrieve them just in case. We
	  ;; could identify and skip the ones that have been retrieved
	  ;; but its probably faster to just get them all.
	  (mapc
	   #'(lambda (article)
	       (if
		   (setq seq
			 (cl-position article  nnselect-artlist :test 'equal))
		   (push (1+ seq) old-arts)
		 (setq nnselect-artlist
		       (vconcat nnselect-artlist (vector article)))
		 (cl-incf last)))
	   new-nnselect-artlist)
	  (setq headers
		(gnus-fetch-headers
		 (append (sort old-arts '<)
			 (gnus-uncompress-range (cons first last))) nil t))
	  (gnus-group-set-parameter
	   group
	   'nnselect-artlist
	   nnselect-artlist)

	  (when (>= last first)
	    (let (new-marks)
	      (pcase-dolist (`(,artgroup ,artids)
			     (ids-by-group (number-sequence first last)))
		(pcase-dolist (`(,type . ,marked)
			       (gnus-info-marks (gnus-get-info artgroup)))
		  (setq marked (gnus-uncompress-sequence marked))
		  (when (setq new-marks
			      (delq nil
				    (mapcar
				     #'(lambda (art)
					 (when (member (cdr art) marked)
					   (car art)))
				     artids)))
		  (nconc
		   (symbol-value (intern (format "gnus-newsgroup-%s"
				   (car (rassq type gnus-article-mark-lists)))))
		   new-marks)))))
	    (setq gnus-newsgroup-active
		  (cons 1 (nnselect-artlist-length nnselect-artlist)))
	    (gnus-set-active
	     group
	     (cons 1 (nnselect-artlist-length nnselect-artlist))))
	  headers)
      ;; If not an imap backend just warp to the original article
      ;; group and punt back to gnus-summary-refer-thread.
      (and (gnus-warp-to-article) (gnus-summary-refer-thread)))))



(deffoo nnselect-close-group (group &optional server)
  (let ((group (nnselect-possibly-change-group group server)))
    (unless gnus-group-is-exiting-without-update-p
      (nnselect-push-info group))
    (setq nnselect-artlist nil)
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
	       (read-from-minibuffer "Function: " nil nil t)))
         (args-spec
          (or  (alist-get 'nnselect-args specs)
               (read-from-minibuffer "Args: " nil nil t nil "nil")))
         (nnselect-specs (list (cons 'nnselect-function function-spec)
			       (cons 'nnselect-args args-spec))))
    (gnus-group-set-parameter group 'nnselect-specs nnselect-specs)
    (gnus-group-set-parameter
     group 'nnselect-artlist
     (or  (alist-get 'nnselect-artlist args)
         (nnselect-run nnselect-specs)))
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


(deffoo nnselect-request-scan (_group _method)
  t)

(deffoo nnselect-request-list (&optional _server)
  t)

;; Add any undefined required backend functions

(nnoo-define-skeleton nnselect)

;;; Util Code:

(defun gnus-nnselect-group-p (group)
  "Say whether GROUP is nnselect or not."
  (or (and (gnus-group-prefixed-p group)
	   (eq 'nnselect (car (gnus-find-method-for-group group))))
      (eq 'nnselect (car gnus-command-method))))


(defun nnselect-run (specs)
  "Apply FUNCTION to ARGS and return an article list."
  (let ((func (alist-get 'nnselect-function specs))
	(args (alist-get 'nnselect-args specs)))
    (funcall func args)))


(defun nnselect-possibly-change-group (group &optional server)
  "If GROUP method for SERVER is `nnselect' install the
`nnselect-artlist'. Return the fully prefixed group name."
  (or (not server) (nnselect-server-opened server)
      (nnselect-open-server server))
  (let ((group  (gnus-group-prefixed-name
		 (gnus-group-short-name group) '(nnselect "nnselect"))))
    (when (gnus-nnselect-group-p group)
      (setq nnselect-artlist (gnus-group-get-parameter
			      group
			      'nnselect-artlist t)))
    group))


(defun nnselect-server-opened (&optional server)
  "Open SERVER if not yet opened."
  (let ((backend (car (gnus-server-to-method server))))
    (nnoo-current-server-p (or backend 'nnselect) server)))

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
  "Copy read and article mark info from the nnselect group to the
originating groups."
  (let ((select-unreads (numbers-by-group gnus-newsgroup-unreads))
	(select-reads (numbers-by-group
		       (gnus-uncompress-range
			(gnus-info-read (gnus-get-info group)))))
	(gnus-newsgroup-active nil)
	mark-list type-list)
    (pcase-dolist (`(,mark . ,type) gnus-article-mark-lists)
      (when (setq type-list
		  (symbol-value (intern (format "gnus-newsgroup-%s" mark))))
	(push (cons type
		    (numbers-by-group
		     (reverse (gnus-uncompress-range type-list)))) mark-list)))
    (pcase-dolist (`(,artgroup ,artlist)
		   (numbers-by-group gnus-newsgroup-articles))
      (let* ((group-info (gnus-get-info artgroup))
	     (old-unread (gnus-list-of-unread-articles artgroup))
	     newmarked)
	(pcase-dolist (`(,_mark . ,type) gnus-article-mark-lists)
	  (let ((select-type
		 (sort
		  (cadr (assoc artgroup  (alist-get type mark-list)))
		  '<))  list)
	    (setq list
		  (gnus-uncompress-range
		   (gnus-add-to-range
		    (gnus-remove-from-range
		     (alist-get type (gnus-info-marks group-info))
		     artlist)
		    select-type)))

	    ;; When exiting the group, everything that's previously been
	    ;; unseen is now seen.
	    (when (eq  type 'seen)
	      (setq list (gnus-range-add list gnus-newsgroup-unseen)))

	    ;; (when (or (eq (gnus-article-mark-to-type  type) 'list)
	    ;; 	      (eq (gnus-article-mark-to-type  type) 'range))
	    ;;   (setq list (gnus-compress-sequence  (sort list '<) t)))

	    (when (eq (gnus-article-mark-to-type type) 'list)
	      (setq list
		    (gnus-compress-sequence  (sort list '<) t)))

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
	      (cadr (assoc artgroup select-reads)))
	     (sort (cadr (assoc artgroup select-unreads)) '<))))
	  (gnus-get-unread-articles-in-group
	   group-info (gnus-active artgroup) t)
	  (gnus-group-update-group artgroup t))))))


(declare-function gnus-registry-get-id-key "gnus-registry" (id key))
(declare-function gnus-group-topic-name "gnus-topic" ())
(declare-function nnir-read-parms "nnir" (search-engine))
(declare-function nnir-server-to-search-engine "nnir" (server))


;; Temporary to make group creation easier

(defun gnus-group-make-permanent-search-group (nnir-extra-parms &optional specs)
  (interactive "P")
  (gnus-group-make-search-group nnir-extra-parms specs t))

(defun gnus-group-make-search-group (nnir-extra-parms &optional specs perm)
  "Create an nnselect group based on a search.  Prompt for a
search query and determine the groups to search as follows: if
called from the *Server* buffer search all groups belonging to
the server on the current line; if called from the *Group* buffer
search any marked groups, or the group on the current line, or
all the groups under the current topic. Calling with a prefix-arg
prompts for additional search-engine specific constraints. A
non-nil `specs' arg must be an alist with `nnir-query-spec' and
`nnir-group-spec' keys, and skips all prompting."
  (interactive "P")
  (let* ((group-spec
	  (or (cdr (assq 'nnir-group-spec specs))
	    (if (gnus-server-server-name)
		(list (list (gnus-server-server-name)))
	      (nnselect-categorize
	       (or gnus-group-marked
		   (if (gnus-group-group-name)
		       (list (gnus-group-group-name))
		     (cdr (assoc (gnus-group-topic-name) gnus-topic-alist))))
	       gnus-group-server))))
	 (query-spec
	  (or (cdr (assq 'nnir-query-spec specs))
	    (apply
	     'append
	     (list (cons 'query
			 (read-string "Query: " nil 'nnir-search-history)))
	     (when nnir-extra-parms
	       (mapcar
		(lambda (x)
		  (nnir-read-parms (nnir-server-to-search-engine (car x))))
		group-spec))))))
    (if perm
	(let ((name (read-string "Group name: " nil)))
	  (gnus-group-make-group
	   name
	   (list 'nnselect "nnselect")
	   nil
	   (list
	    (cons 'nnselect-specs
		  (list
		   (cons 'nnselect-function 'nnir-run-query)
		   (cons 'nnselect-args
			 (list (cons 'nnir-query-spec query-spec)
			       (cons 'nnir-group-spec group-spec))))))))
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
		     (list (cons 'nnir-query-spec query-spec)
			   (cons 'nnir-group-spec group-spec)))))
	(cons 'nnselect-artlist nil))))))


;; The end.
(provide 'nnselect)

;;; nnselect.el ends here
