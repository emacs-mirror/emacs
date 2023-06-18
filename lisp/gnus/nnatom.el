;;; nnatom.el --- Atom backend for Gnus -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Daniel Semyonov <daniel@dsemy.com>

;; This file is not part of GNU Emacs.

;; nnatom is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; nnatom is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with nnatom.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Gnus backend for Atom feeds.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'gv)
  (require 'subr-x))

(require 'gnus)
(require 'nnheader)
(require 'nnoo)
(require 'gnus-group)
(require 'mm-url)

(defgroup nnatom nil
  "Atom backend for Gnus."
  :group 'gnus)

(nnoo-declare nnatom)

(defvoo nnatom-backend 'nnatom
  "Symbol which identifies this backend.")

(defvoo nnatom-status-string nil
  "Last status message reported by this backend.")

(defsubst nnatom--backend-prefix (backend)
  (concat (symbol-name backend) ":"))

;;;; Atom feed parser:

(defsubst nnatom--node-attributes (node subnode)
  (cadr (assq subnode node)))

(defsubst nnatom--node-subnodes (node subnode)
  (let ((sub (cddr (assq subnode node))))
    (or (and (atom (car sub)) (car sub)) sub)))

(defun nnatom--parse-feed (feed)
  "Return a list structure representing FEED, or nil."
  (if (string-match-p "^https?://" feed)
      (nnheader-report
       nnatom-backend
       "Address shouldn't start with \"http://\" or \"https://\"")
    (with-temp-buffer
      (condition-case e
          (if (file-readable-p feed)
              (insert-file-contents feed)
            (mm-url-insert-file-contents (concat "https://" feed)))
        (file-error (nnheader-report nnatom-backend (cdr e)))
        (:success (cddr (libxml-parse-xml-region (point-min) (point-max))))))))

(defun nnatom--read-article (data _)
  "Return the next article and the remaining DATA in a cons cell, or nil."
  (and data
       `(,(let (article)
            (while (not (eq (car (setq article (car data))) 'entry))
              (setq data (cdr data)))
            (setq data (cdr data))
            article)
         . ,data)))

(defun nnatom--read-title (feed)
  "Return the title of FEED, or nil."
  (nnatom--node-subnodes feed 'title))

(defun nnatom--read-article-or-feed-author (article-or-feed)
  "Return the author of ARTICLE-OR-FEED, or nil."
  (let* ((author (nnatom--node-subnodes article-or-feed 'author))
         (name (nnatom--node-subnodes author 'name))
         (email (nnatom--node-subnodes author 'email)))
    (or (and name email (format "%s <%s>" name email))
        name email)))

(defun nnatom--read-id (article)
  "Return the ID of ARTICLE.
If the ARTICLE doesn't contain an ID but it does contain a subject,
return the subject.  Otherwise, return nil."
  (or (nnatom--node-subnodes article 'id)
      (nnatom--read-subject article)))

(defun nnatom--read-subject (article)
  "Return the subject of ARTICLE, or nil."
  (nnatom--node-subnodes article 'title))

(defun nnatom--read-publish (article)
  "Return the date and time ARTICLE was published, or nil."
  (when-let ((pub (nnatom--node-subnodes article 'published)))
    (and (stringp pub) (date-to-time pub))))

(defun nnatom--read-update (article)
  "Return the date and time of the last update to ARTICLE, or nil."
  (when-let ((update (nnatom--node-subnodes article 'updated)))
    (and (stringp update) (date-to-time update))))

(defun nnatom--read-link (article)
  "Return the link to ARTICLE, or nil."
  (alist-get 'href (or (nnatom--node-attributes article 'link)
                       (nnatom--node-subnodes article 'link))))

(defun nnatom--read-summary-or-content-type (article summary-or-content)
  "Return the type of SUMMARY-OR-CONTENT of ARTICLE, or \"plain\"."
  (or (cdr (assq 'type
                 (nnatom--node-attributes article summary-or-content)))
      "plain"))

(defun nnatom--read-summary-or-content (article summary-or-content)
  "Return SUMMARY-OR-CONTENT of ARTICLE, or nil."
  (nnatom--node-subnodes article summary-or-content))

;;;; Feed I/O:

(defvoo nnatom-read-feed-function #'nnatom--parse-feed
  "Function returning a Lisp object representing a feed (or part of it).
It should accept a single argument, the address of a feed.")

(defvoo nnatom-read-article-function #'nnatom--read-article
  "Function returning a cons cell of an article and remaining data from a feed.
It should accept a two arguments, a Lisp object representing a feed,
and a flag indicating whether the last article was stale (not new or updated).
If there are no remaining articles, it should return nil.")

(defvoo nnatom-read-title-function #'nnatom--read-title
  "Function returning the title of a feed (a string).
It should accept a single argument, a Lisp object representing a feed.")

(defvoo nnatom-read-feed-author-function #'nnatom--read-article-or-feed-author
  "Function returning the author of a feed (a string).
It should accept a single argument, a Lisp object representing a feed.")

(defvoo nnatom-read-id-function #'nnatom--read-id
  "Function returning the ID of an article.
It should accept a single argument, a Lisp object representing an article.")

(defvoo nnatom-read-subject-function #'nnatom--read-subject
  "Function returning the subject of an article (a string).
It should accept a single argument, a Lisp object representing an article.")

(defvoo nnatom-read-publish-date-function #'nnatom--read-publish
  "Function returning the publish date of an article (a time value).
It should accept a single argument, a Lisp object representing an article.")

(defvoo nnatom-read-update-date-function #'nnatom--read-update
  "Function returning the update date of an article (a time value).
It should accept a single argument, a Lisp object representing an article.")

(defvoo nnatom-read-author-function #'nnatom--read-article-or-feed-author
  "Function returning the author of an article (a string).
It should accept a single argument, a Lisp object representing an article.")

(defvoo nnatom-read-link-function #'nnatom--read-link
  "Function returning the link contained in an article (a string).
It should accept a single argument, a Lisp object representing an article.")

(defvoo nnatom-read-summary-type-function
    (lambda (article)
      (nnatom--read-summary-or-content-type article 'summary))
  "Function returning the type of the summary of an article (a string).
This is appended to \"text/\" to form the MIME type of the summary.
It should accept a single argument, a Lisp object representing an article.")

(defvoo nnatom-read-content-type-function
    (lambda (article)
      (nnatom--read-summary-or-content-type article 'content))
  "Function returning the type of the content of an article (a string).
This is appended to \"text/\" to form the MIME type of the content.
It should accept a single argument, a Lisp object representing an article.")

(defvoo nnatom-read-summary-function
    (lambda (article)
      (nnatom--read-summary-or-content article 'summary))
  "Function returning the summary of an article (a string).
It should accept a single argument, a Lisp object representing an article.")

(defvoo nnatom-read-content-function
    (lambda (article)
      (nnatom--read-summary-or-content article 'content))
  "Function returning the content of an article (a string).
It should accept a single argument, a Lisp object representing an article.")

(defvoo nnatom-groups (make-hash-table :test 'equal)
  "Hash table mapping known group names to their data.

Each value in this table should be a vector of the form
[GROUP IDS ARTICLES MAX MIN], where:
- GROUP is the name of the group.
- IDS is a hash table mapping article IDs to their numbers.
- ARTICLES is a hash table mapping article numbers to articles and
  their attributes (see `nnatom-group-articles').
- MAX is the maximum article number.
- MIN is the minimum article number.")

(defun nnatom-group-file (group)
  "Return the file containing feed data for GROUP."
  (expand-file-name (format "%s/%s.eld"
                            (string-trim (symbol-name nnatom-backend)
                                         "nn")
                            (gnus-newsgroup-savable-name group))
                    gnus-directory))

(defun nnatom--read-feed (feed &optional group)
  "Read FEED into a new or existing GROUP."
  (let ((prefix (nnatom--backend-prefix nnatom-backend)))
    (when (string-suffix-p "-ephemeral" feed)
      (setq feed (or (cadr (assq (nnoo-symbol nnatom-backend 'address)
                                 (cddr (gnus-server-to-method
                                        (concat prefix feed)))))
                     feed)))
    (when-let ((data (funcall nnatom-read-feed-function feed))
               (group (or (and group (string-trim group prefix))
                          (cadr (assq (nnoo-symbol nnatom-backend 'name)
                                      (gnus-server-to-method
                                       (concat prefix feed))))
                          (funcall nnatom-read-title-function data))))
      (let* ((info (gnus-get-info (concat prefix group)))
             (g (or (gethash group nnatom-groups)
                    (nnatom-read-group group)
                    `[ ,group ,(make-hash-table :test 'equal)
                       ,(make-hash-table :test 'eql) nil 1]))
             (ids (aref g 1))
             (articles (aref g 2))
             (max (aref g 3))
             (max (if max max
                    (setq max 0) ; Find max article number
                    (dolist      ; remembered by Gnus.
                        ( r (cons (gnus-info-read info)
                                  (gnus-info-marks info))
                          max)
                      (mapc (lambda (x)
                              (let ((x (if (consp x)
                                           (if (< (car x) (cdr x))
                                               (cdr x) (car x))
                                         x)))
                                (when (< max x) (setq max x))))
                            (if (symbolp (car r)) (cdr r) r)))))
             (feed-author (funcall nnatom-read-feed-author-function data))
             article stale)
        (while (setq article (funcall nnatom-read-article-function data stale)
                     data (cdr article)
                     article (car article))
          (when-let ((id (funcall nnatom-read-id-function article))
                     (id (format "<%s@%s.%s>" id group nnatom-backend)))
            (let* ((num (gethash id ids))
                   (update (funcall nnatom-read-update-date-function article))
                   (prev-update (aref (gethash num articles
                                               '[nil nil nil nil nil])
                                      4)))
              (if (or (null num) ; New article ID.
                      (and (null prev-update) update)
                      (and prev-update update
                           (time-less-p prev-update update)))
                  (let* ((num (or num (aset g 3 (cl-incf max))))
                         (publish (funcall nnatom-read-publish-date-function
                                           article)))
                    (setf
                     (gethash id (aref g 1)) num
                     (gethash num (aref g 2))
                     `[ ,id
                        ,(or (funcall nnatom-read-author-function article)
                             feed-author group)
                        ,(or (funcall nnatom-read-subject-function article)
                             "no subject")
                        ,(or publish update '(0 0)) ; published
                        ,(or update publish '(0 0)) ; updated
                        ,(funcall nnatom-read-link-function article)
                        (,(funcall nnatom-read-summary-function article)
                         . ,(funcall nnatom-read-summary-type-function article))
                        (,(funcall nnatom-read-content-function article)
                         . ,(funcall
                             nnatom-read-content-type-function
                             article))]
                     stale nil))
                (setq stale t)))))
        (puthash group g nnatom-groups)))))

(defun nnatom-read-group (group)
  "Read GROUP's information from `nnatom-directory'."
  (if-let ((f (nnatom-group-file group))
           ((file-readable-p f)))
      (with-temp-buffer
        (insert-file-contents f)
        (goto-char (point-min))
        (puthash group (read (current-buffer)) nnatom-groups))
    (nnheader-report nnatom-backend "Can't read %s")))

(defun nnatom-write-group (group)
  "Write GROUP's information to `nnatom-directory'."
  (if-let ((f (nnatom-group-file group))
           ((file-writable-p f)))
      (if-let ((g (gethash group nnatom-groups))
               ((vectorp g)))
          (with-temp-file f
            (insert ";;;; -*- mode: lisp-data -*- DO NOT EDIT\n"
                    (prin1-to-string g))
            t)
        t)
    (nnheader-report nnatom-backend "Can't write %s" f)))

;;;; Gnus backend functions:

(defvoo nnatom-group nil
  "Name of the current group.")

(defvoo nnatom-group-article-ids (make-hash-table :test 'equal)
  "Hash table mapping article IDs to their article number.")

(defvoo nnatom-group-articles (make-hash-table :test 'eql)
  "Hash table mapping article numbers to articles and their attributes.

Each value in this table should be a vector of the form
[ID FROM SUBJECT DATE UPDATED LINK SUMMARY CONTENT], where:
- ID is the ID of the article.
- FROM is the author of the article or group.
- SUBJECT is the subject of the article.
- DATE is the date the article was published, or last updated (time value).
- UPDATE is the date the article was last updated, or published (time value).
- LINK is the URL to the full (remote) version of the article.
- SUMMARY and CONTENT are cons cells where the car is the raw
  summary/content of the article and the cdr is the \"type\" of
  the summary/content (appended to \"text/\" to form the MIME type).")

(defvoo nnatom-group-article-max-num 0
  "Maximum article number for the current group.")

(defvoo nnatom-group-article-min-num 1
  "Minimum article number for the current group.")

(defvar nnatom-date-format "%F %X"
  "Format of displayed dates.")

(nnoo-define-basics nnatom)

(defun nnatom-retrieve-article (article group)
  (if-let ((a (gethash article (aref group 2))))
      (insert (format "221 %s Article retrieved.
From: %s\nSubject: %s\nDate: %s\nMessage-ID: %s\n.\n"
                      article
                      (aref a 1)
                      (aref a 2)
                      (format-time-string nnatom-date-format (aref a 3))
                      (aref a 0)))
    (insert "404 Article not found.\n.\n")))

(deffoo nnatom-retrieve-headers (articles &optional group _server _fetch-old)
  (if-let ((g (or (gethash group nnatom-groups)
                  (nnatom-read-group group)
                  `[ nil ,nnatom-group-article-ids
                     ,nnatom-group-articles nil nil])))
      (with-current-buffer nntp-server-buffer
        (erase-buffer)
        (or (and (stringp (car articles))
                 (mapc (lambda (a)
                         (nnatom-retrieve-article
                          (gethash a (aref g 1)) g))
                       articles))
            (and (numberp (car articles))
                 (range-map (lambda (a) (nnatom-retrieve-article a g))
                            articles)))
        'headers)
    (nnheader-report nnatom-backend "Group %s not found" (or group ""))))

(deffoo nnatom-request-close ()
  (setq nnatom-groups (make-hash-table :test 'equal)
        nnatom-status-string nil)
  t)

(defun nnatom--print-content (content type link)
  "Return CONTENT with LINK appended and formatted according to TYPE."
  (when (and content type)
    (let ((html (or (string= type "html") (string= type "xhtml"))))
      (concat (when html "<html><head></head><body>") content "\n\n"
              (and (stringp link)
                   (if html (format "<p><a href=\"%s\">[Link]</a></p>" link)
                     link))
              (when html "</body></html>")))))

(deffoo nnatom-request-article (article &optional group _server to-buffer)
  (if-let ((g (or (gethash group nnatom-groups)
                  (and nnatom-group
                       `[ nil ,nnatom-group-article-ids
                          ,nnatom-group-articles
                          ,nnatom-group-article-max-num
                          ,nnatom-group-article-min-num])))
           (num (or (and (stringp article)
                         (gethash article (aref g 1)))
                    (and (numberp article) article)))
           ((and (<= num (aref g 3))
                 (>= num (aref g 4))))
           (a (gethash num (aref g 2))))
      (with-current-buffer (or to-buffer nntp-server-buffer)
        (erase-buffer)
        (let* ((boundary (format "-_%s_-" nnatom-backend))
               (link (aref a 5))
               (summary (aref a 6))
               (summary-type (cdr summary))
               (summary
                (nnatom--print-content (car summary) summary-type link))
               (content (aref a 7))
               (content-type (cdr content))
               (content
                (nnatom--print-content (car content) content-type link)))
          (insert (format
                   "Subject: %s\nFrom: %s\nDate: %s\nMessage-ID: %s\n"
                   (aref a 2) (aref a 1)
                   (format-time-string
                    nnatom-date-format (or (aref a 3) '(0 0)))
                   (aref a 0))
                  "MIME-Version: 1.0\nContent-Type: "
                  (concat (cond ((and summary content)
                                 (format "multipart/alternative; boundary=%s
--%s\nContent-Type: text/%s\n\n%s\n--%s\nContent-Type: text/%s\n\n%s\n--%s--\n"
                                         boundary boundary summary-type
                                         summary boundary content-type
                                         content boundary))
                                (summary (format "text/%s\n\n%s\n"
                                                 summary-type summary))
                                (content (format "text/%s\n\n%s\n"
                                                 content-type content))
                                (link (format "text/plain\n\n%s\n" link))
                                (t "text/plain\n\n")))))
        `(,group . ,num))
    (nnheader-report nnatom-backend "No such article")))

(deffoo nnatom-request-group (group &optional server fast _info)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (if-let ((s (or server (nnoo-current-server nnatom-backend)))
             (g (or (if fast
                        (or (gethash group nnatom-groups)
                            (nnatom-read-group group))
                      (nnatom--read-feed s group))
                    `[ ,group ,(make-hash-table :test 'equal)
                       ,(make-hash-table :test 'eql) 0 1])))
        (progn
          (setq nnatom-group group
                nnatom-group-article-ids (aref g 1)
                nnatom-group-articles (aref g 2)
                nnatom-group-article-max-num (aref g 3)
                nnatom-group-article-min-num (aref g 4))
          (insert (format "211 %s %s %s \"%s\""
                          (hash-table-count nnatom-group-article-ids)
                          nnatom-group-article-min-num
                          nnatom-group-article-max-num group))
          t)
      (insert "404 group not found")
      (nnheader-report nnatom-backend "Group %s not found" group))))

(deffoo nnatom-close-group (group &optional _server)
  (if (nnatom-write-group group)
      (setq nnatom-group nil
            nnatom-group-article-ids (make-hash-table :test 'equal)
            nnatom-group-articles (make-hash-table :test 'eql)
            nnatom-group-article-max-num 0
            nnatom-group-article-min-num 1)
    (nnheader-report nnatom-backend "Couldn't write group %s" group)))

(deffoo nnatom-request-list (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (when-let ((g (nnatom--read-feed
                   (or server (nnoo-current-server nnatom-backend)))))
      (insert (format "\"%s\" %s %s y\n"
                      (aref g 0) (aref g 3) (aref g 4)))
      t)))

(deffoo nnatom-request-post (&optional _server)
  (nnheader-report nnatom-backend "%s is a read only backend" nnatom-backend))

;;;; Optional back end functions:

(deffoo nnatom-retrieve-groups (_groups &optional server)
  (nnatom-request-list (or server (nnoo-current-server nnatom-backend)))
  'active)

(deffoo nnatom-request-type (_group &optional _article)
  'unknown)

(deffoo nnatom-request-delete-group (group _force &optional _server)
  (delete-file (nnatom-group-file group))
  (remhash group nnatom-groups)
  (when (string= group nnatom-group)
    (setq nnatom-group nil
          nnatom-group-article-ids (make-hash-table :test 'equal)
          nnatom-group-articles (make-hash-table :test 'eql)
          nnatom-group-article-max-num 0
          nnatom-group-article-min-num 1))
  t)

(gnus-declare-backend (symbol-name nnatom-backend) 'address)

;;;; Utilities:

(defmacro nnatom-define-basic-backend-interface (backend)
  "Define a basic set of functions and variables for BACKEND."
  (let ((bp (symbol-name backend)))
    `(progn
       (defvoo ,(intern (concat bp "-backend")) ',backend nil nnatom-backend)
       (defvoo ,(intern (concat bp "-status-string"))
           nil nil nnatom-status-string)
       (defvoo ,(intern (concat bp "-group")) nil nil nnatom-group)
       (defvoo ,(intern (concat bp "-groups"))
           (make-hash-table :test 'equal) nil nnatom-groups)
       (defvoo ,(intern (concat bp "-group-article-ids"))
           (make-hash-table :test 'equal) nil nnatom-group-article-ids)
       (defvoo ,(intern (concat bp "-group-articles"))
           (make-hash-table :test 'eql) nil nnatom-group-articles)
       (defvoo ,(intern (concat bp "-group-article-max-num")) 0 nil
               nnatom-group-article-max-num)
       (defvoo ,(intern (concat bp "-group-article-mix-num")) 1 nil
               nnatom-group-article-min-num)
       (nnoo-define-basics ,backend)
       (nnoo-import ,backend (nnatom)))))

(provide 'nnatom)

;;; nnatom.el ends here
