;;; nnfeed.el --- Generic feed backend for Gnus -*- lexical-binding: t -*-

;; Copyright (C) 2023, 2025-2026 Free Software Foundation, Inc.
;; Author: Daniel Semyonov <daniel@dsemy.com>

;; This file is part of GNU Emacs.

;; nnfeed is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; nnfeed is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with nnfeed.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Generic Gnus backend (intended) for implementing backends for web
;; feeds (Atom, RSS).
;;
;; This backend is abstract - it doesn't implement a parser for any
;; specific web feed type, and so can't be used independently.
;;
;; Instead, it implements a generic parser, feed data storage and most
;; of the Gnus backend interface; the intended use for this backend is
;; to be a source of inheritance for backends supporting new web feed
;; types.
;;
;; To implement new backends, use `nnfeed-define-basic-backend-interface':
;;
;;    ...
;;    (require 'nnfeed)
;;
;;    (nnoo-declare nnfoo nnfeed)
;;
;;    (nnfeed-define-basic-backend-interface nnfoo)
;;    ...
;;    [  definitions of parsing functions, see the "Feed parser interface"
;;       section for more information.  ]
;;
;;    (defvoo nnfoo-read-feed-function #'nnfoo--read-feed
;;     nil nnfeed-read-feed-function)
;;    ...
;;    (gnus-declare-backend (symbol-name nnfeed-backend) 'none 'address)
;;
;;    (provide 'nnfoo)
;;
;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'gnus)
(require 'nnoo)

(defgroup nnfeed nil
  "Generic feed backend for Gnus."
  :group 'gnus)

(defcustom nnfeed-date-format "%F %X%p"
  "Format of displayed dates (see function `format-time-string')."
  :version "30.1"
  :type 'string)

(nnoo-declare nnfeed)

(defvoo nnfeed-backend nil
  "Symbol which identifies this backend.")

(defvoo nnfeed-status-string nil
  "Last status message reported by this backend.")

;; This macro should be used to define inheriting backends.

(defmacro nnfeed-define-basic-backend-interface (backend)
  "Define a basic set of functions and variables for BACKEND."
  `(progn
     (defvoo ,(nnoo-symbol backend 'backend) ',backend nil nnfeed-backend)
     (defvoo ,(nnoo-symbol backend 'status-string)
         nil nil nnfeed-status-string)
     (defvoo ,(nnoo-symbol backend 'group) nil nil nnfeed-group)
     (defvoo ,(nnoo-symbol backend 'servers)
         (make-hash-table :test 'equal) nil nnfeed-servers)
     (defvoo ,(nnoo-symbol backend 'group-article-ids)
         (make-hash-table :test 'equal) nil nnfeed-group-article-ids)
     (defvoo ,(nnoo-symbol backend 'group-articles)
         (make-hash-table :test 'eql) nil nnfeed-group-articles)
     (defvoo ,(nnoo-symbol backend 'group-article-max-num) 0 nil
             nnfeed-group-article-max-num)
     (defvoo ,(nnoo-symbol backend 'group-article-mix-num) 1 nil
             nnfeed-group-article-min-num)
     ,@(mapcar (lambda (fun)
                 `(deffoo ,(nnoo-symbol backend fun) (&optional server)
	            (,(nnoo-symbol 'nnoo fun) ',backend server)))
	       '(server-opened status-message))
     (deffoo ,(nnoo-symbol backend 'open-server) (server &optional defs)
       (nnfeed-open-server server defs ',backend))
     (nnoo-import ,backend (nnfeed))))

;;;; Feed parser interface:

;; The following set of server variables define a parser for a
;; specific web feed type.
;; An inheriting backend doesn't necessarily have to define all of
;; these functions (see the comments below for more information).
;; Apart from this set of variables there is also
;; `nnfeed-print-content-function' which can optionally be defined
;; by an inheriting backend to allow more advanced control over the
;; printing of articles.

(defvoo nnfeed-read-feed-function #'ignore
  "Function returning a Lisp object representing a feed (or part of it).

It should accept two arguments, the address of a feed and the name of
a group (or nil).
If a group name is supplied, it should return a representation of only
the group (as if it was extracted from the feed with
`nnfeed-read-group-function').")

(defvoo nnfeed-read-group-function #'ignore
  "Function returning a cons cell of a group and remaining data from a feed.

The returned group can be represented by any Lisp object.
It should accept a single argument, a Lisp object representing a feed
\(as can be returned by this function or `nnfeed-read-feed-function').")

(defvoo nnfeed-read-article-function #'ignore
  "Function returning a cons cell of an article and remaining data from a group.

The returned article can be represented by any Lisp object.
It should accept two arguments, a Lisp object representing a group
\(as can be returned by this function or `nnfeed-read-group-function'),
and a flag indicating whether the last article was not new or updated.")

(defvoo nnfeed-read-title-function #'ignore
  "Function returning the title of a group (a string).

It should accept a single argument, a Lisp object representing a group
\(as returned by `nnfeed-read-group-function').")

;; Optional.
(defvoo nnfeed-read-description-function #'ignore
  "Function returning the description of a group (a string), or nil.

It should accept a single argument, a Lisp object representing a group
\(as returned by `nnfeed-read-group-function').")

;; Either this function or `nnfeed-read-author-function' is required.
(defvoo nnfeed-read-group-author-function #'ignore
  "Function returning the author of a group (a string), or nil.

It should accept a single argument, a Lisp object representing a group
\(as returned by `nnfeed-read-group-function')..")

(defvoo nnfeed-read-id-function #'ignore
  "Function returning the ID of an article.

It should accept a single argument, a Lisp object representing an article
\(as returned by `nnfeed-read-article-function').")

(defvoo nnfeed-read-subject-function #'ignore
  "Function returning the subject of an article (a string), or nil.

It should accept a single argument, a Lisp object representing an article
\(as returned by `nnfeed-read-article-function').")

;; Either this function or `nnfeed-read-update-date-function' is required.
(defvoo nnfeed-read-publish-date-function #'ignore
  "Function returning the publish date of an article (a time value), or nil.

It should accept a single argument, a Lisp object representing an article
\(as returned by `nnfeed-read-article-function').")

;; Either this function or `nnfeed-read-publish-date-function' is required.
(defvoo nnfeed-read-update-date-function #'ignore
  "Function returning the update date of an article (a time value), or nil.

It should accept a single argument, a Lisp object representing an article
\(as returned by `nnfeed-read-article-function').")

;; Either this function or `nnfeed-read-group-author-function' is required.
(defvoo nnfeed-read-author-function #'ignore
  "Function returning the author of an article (a string), or nil.

It should accept a single argument, a Lisp object representing an article
\(as returned by `nnfeed-read-article-function').")

(defvoo nnfeed-read-headers-function #'ignore
  "Function returning an alist of article-wide MIME headers.

Each element of this alist maps a MIME header (a symbol,
i.e. `Content-Type') to its value.  As a special case, `:boundary'
maps to a string which will serve as the boundary between article
parts.  This must be supplied if a custom boundary is used in a
multipart content type header.  The default boundary is \"-_nnfeed_-\",
and is automatically modified to match the name of the back end.
It should accept a single argument, a Lisp object representing an article
\(as returned by `nnfeed-read-article-function').")

;; As mentioned in their docstrings, the last two parsing functions
;; can optionally return any Lisp representation they want, provided
;; an appropriate `nnfeed-print-content-function' is defined.  This
;; means they are also not _strictly_ required.

(defvoo nnfeed-read-links-function #'ignore
  "Function returning all links contained in an article.

With the default `nnfeed-print-content-function', it should return a
list of links, where each link is an alist mapping MIME content types
to links formatted for display in a part of that type.  Each content
type may also be a list of content types.
Otherwise, it could return any Lisp object.
It should accept a single argument, a Lisp object representing an article
\(as returned by `nnfeed-read-article-function').")

(defvoo nnfeed-read-parts-function #'ignore
  "Function returning an alist associating parts of an article to their headers.

With the default `nnfeed-print-content-function', each part should be a
string.  Otherwise, it can be any Lisp object.  The \"headers\" of
each part should be a list where each element is either a cons of a
MIME header (a symbol, i.e. `Content-Type') and its value (a string),
or any other Lisp object.  MIME headers will be printed, the rest will
be passed on to `nnfeed-print-content-function', which recognizes the
following extra data by default:
- `links', if present, will cause links to be printed in the part.
It should accept a single argument, a Lisp object representing an article
\(as returned by `nnfeed-read-article-function').")

;;;; Feed data storage:

;; This section defines the data types used to store feed data, and
;; functions to read and write it.
;; All variables in this section are automatically defined by
;; `nnfeed-define-basic-backend-interface'.

(defvoo nnfeed-servers (make-hash-table :test 'equal)
  "Hash table mapping known servers to their groups.

Each value in this table should itself be a hash table mapping known
group names to their data, which should be a vector of the form
[GROUP IDS ARTICLES MAX MIN DESCRIPTION], where:
- GROUP is the \"real\" group name (the name known to the server).
- IDS is a hash table mapping article IDs to their numbers.
- ARTICLES is a hash table mapping article numbers to articles and
  their attributes (see `nnfeed-group-articles').
- MAX is the maximum article number.
- MIN is the minimum article number.
- DESCRIPTION is the group description.")

(defvoo nnfeed-group-names (make-hash-table :test 'equal)
  "Hash table mapping real group names to their custom name.")

(defun nnfeed--server-address (server)
  "Return SERVER's real address."
  (if (string-suffix-p "-ephemeral" server)
      (setq server (or (cadr (assq (nnoo-symbol nnfeed-backend 'address)
                                   (cddr (gnus-server-to-method
                                          (concat
                                           (symbol-name nnfeed-backend) ":"
                                           server)))))
                       server))
    server))

(defun nnfeed--server-file (server)
  "Return the file containing data for SERVER."
  (expand-file-name (format "%s/%s.eld"
                            (string-trim (symbol-name nnfeed-backend)
                                         "nn")
                            (gnus-newsgroup-savable-name
                             (nnfeed--server-address server)))
                    gnus-directory))

(defun nnfeed--read-server (server)
  "Read SERVER's information from storage."
  (if-let* ((f (nnfeed--server-file server))
            ((file-readable-p f)))
      (with-temp-buffer
        (insert-file-contents f)
        (goto-char (point-min))
        (puthash server (read (current-buffer)) nnfeed-servers))
    (nnheader-report nnfeed-backend "Can't read %s" server)))

(defun nnfeed--write-server (server)
  "Write SERVER's information to storage."
  (if-let* ((f (nnfeed--server-file server))
            ((file-writable-p f)))
      (if-let* ((s (gethash server nnfeed-servers))
                ((hash-table-p s)))
          (with-temp-file f
            (insert ";;;; -*- mode: lisp-data -*- DO NOT EDIT\n")
            (prin1 s (current-buffer) t)
            (insert "\n")
            t)
        t)
    (nnheader-report nnfeed-backend "Can't write %s" f)))

;; The following function uses the parsing functions defined in the last
;; section to parse a feed (or just one group from it).
;; This is the only place where these parsing functions are used; the Gnus
;; backend interface extracts all required information from the parsed feed.

(defun nnfeed--parse-feed (feed &optional group)
  "Parse GROUP from FEED into a new or existing server.
If GROUP is omitted or nil, parse the entire FEED."
  (let* ((feed (nnfeed--server-address feed))
         (s (or (gethash feed nnfeed-servers) (nnfeed--read-server feed)
                (make-hash-table :test 'equal)))
         (name group) ; (Maybe) fake name (or nil)
         (group (aref (gethash group s `[,group]) 0)) ; Real name (or nil)
         data)
    (when (setq data (funcall nnfeed-read-feed-function feed group))
      (while-let ((cg (or (and name (cons data)) ; `data' is a single group
                          (funcall nnfeed-read-group-function data)))
                  (cg (prog1 (car cg) (setq data (cdr cg)))))
        (let* ((name (funcall nnfeed-read-title-function cg)) ; Real name
               (group (gethash name nnfeed-group-names name)) ; (Maybe) fake name
               (info (gnus-get-info
                      (concat (symbol-name nnfeed-backend) "+" feed ":" group)))
               (g (or (gethash group s)
                      `[ ,name ,(make-hash-table :test 'equal)
                         ,(make-hash-table :test 'eql) nil 1 ""]))
               (desc (funcall nnfeed-read-description-function cg))
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
               (group-author (funcall nnfeed-read-group-author-function cg))
               stale)
          (and desc (aset g 5 desc))
          (while-let ((article (funcall nnfeed-read-article-function cg stale))
                      (article (prog1 (car article) (setq cg (cdr article)))))
            (when-let* ((id (funcall nnfeed-read-id-function article))
                        (id (format "<%s@%s.%s>" id name nnfeed-backend)))
              (let* ((num (gethash id ids))
                     (update (funcall nnfeed-read-update-date-function article))
                     (prev-update (aref (gethash num articles
                                                 '[nil nil nil nil nil])
                                        4)))
                (if (or (null num) ; New article ID.
                        (and (null prev-update) update)
                        (and prev-update update
                             (time-less-p prev-update update)))
                    (let* ((num (or num (aset g 3 (setq max (1+ max)))))
                           (publish (funcall nnfeed-read-publish-date-function
                                             article)))
                      (setf
                       (gethash id (aref g 1)) num
                       (gethash num (aref g 2))
                       `[ ,id
                          ,(or (funcall nnfeed-read-author-function article)
                               group-author group)
                          ,(or (funcall nnfeed-read-subject-function article)
                               "no subject")
                          ,(or publish update '(0 0)) ; published
                          ,(or update publish '(0 0)) ; updated
                          ,(funcall nnfeed-read-links-function article)
                          ,(funcall nnfeed-read-parts-function article)
                          ,(funcall nnfeed-read-headers-function article)]
                       stale nil))
                  (setq stale t)))))
          (puthash group g s)))
      (puthash feed s nnfeed-servers))))

;;;; Gnus backend functions:

;; The following two sections define a Gnus backend interface based on
;; the parsed data from the last section.
;; All server variables in this section are automatically defined by
;; `nnfeed-define-basic-backend-interface'.
;; For more information about these functions see the "Back End
;; Interface" section of the Gnus manual.

(defvoo nnfeed-group nil
  "Name of the current group.")

(defvoo nnfeed-group-article-ids (make-hash-table :test 'equal)
  "Hash table mapping article IDs to their article number.")

(defvoo nnfeed-group-articles (make-hash-table :test 'eql)
  "Hash table mapping article numbers to articles and their attributes.

Each value in this table should be a vector of the form
[ID FROM SUBJECT DATE UPDATED LINKS PARTS HEADERS], where:
- ID is the ID of the article.
- FROM is the author of the article or group.
- SUBJECT is the subject of the article.
- DATE is the date the article was published, or last updated (time value).
- UPDATE is the date the article was last updated, or published (time value).
- LINKS is a collection of links (any Lisp object).
- PARTS is an alist associating the content of each part of the
  article to its headers.
- HEADERS is an alist associating article-wide MIME headers to their value.")

(defvoo nnfeed-group-article-max-num 0
  "Maximum article number for the current group.")

(defvoo nnfeed-group-article-min-num 1
  "Minimum article number for the current group.")

(nnoo-define-basics nnfeed)

(defun nnfeed--current-server-no-prefix ()
  "Remove the \"<backend>+\" prefix from the current server."
  (string-remove-prefix (concat (symbol-name nnfeed-backend) "+")
                        (nnoo-current-server nnfeed-backend)))

(defun nnfeed--group-data (group server)
  "Get parsed data for GROUP from SERVER."
  (when-let* ((server (nnfeed--server-address server))
              (s (gethash server nnfeed-servers))
              ((hash-table-p s)))
    (gethash group s)))

(defun nnfeed-retrieve-article (article group)
  "Retrieve headers for ARTICLE from GROUP."
  (if-let* ((a (gethash article (aref group 2))))
      (insert (format "221 %s Article retrieved.
From: %s\nSubject: %s\nDate: %s\nMessage-ID: %s\n.\n"
                      article
                      (aref a 1)
                      (aref a 2)
                      (format-time-string "%F %H:%M" (aref a 3))
                      (aref a 0)))
    (insert "404 Article not found.\n.\n")))

(deffoo nnfeed-retrieve-headers (articles &optional group server _fetch-old)
  (if-let* ((server (or server (nnfeed--current-server-no-prefix)))
            (g (or (nnfeed--group-data group server)
                   `[ nil ,nnfeed-group-article-ids ,nnfeed-group-articles
                      nil nil nil])))
      (with-current-buffer nntp-server-buffer
        (erase-buffer)
        (or (and (stringp (car articles))
                 (mapc (lambda (a)
                         (nnfeed-retrieve-article
                          (gethash a (aref g 2)) g))
                       articles))
            (and (numberp (car articles))
                 (range-map (lambda (a) (nnfeed-retrieve-article a g))
                            articles)))
        'headers)
    (nnheader-report nnfeed-backend "Group %s not found" (or group ""))))

(deffoo nnfeed-open-server (server &optional defs backend)
  (let ((backend (or backend 'nnfeed))
        (a (nnfeed--server-address server))
        s)
    (nnoo-change-server backend server defs)
    (when (setq s (or (gethash a nnfeed-servers) (nnfeed--read-server server)))
      (maphash (lambda (group g)
                 (setq g (aref g 0))
                 (unless (string= group g)
                   (puthash g group nnfeed-group-names)))
               s))
    (setq a (nnfeed--server-file server))
    (or s (condition-case _ (make-directory (file-name-parent-directory a) t)
            (:success (file-writable-p a))
            (t nil))
        (and (nnoo-close-server nnfeed-backend server)
             (nnheader-report
              nnfeed-backend "Server file %s not readable or writable"
              server)))))

(deffoo nnfeed-request-close ()
  (when (hash-table-p nnfeed-servers)
    (maphash (lambda (server _) (nnfeed--write-server server)) nnfeed-servers)
    (setq nnfeed-servers (make-hash-table :test 'equal)))
  (setq nnfeed-status-string nil)
  t)

;; The default content printing function, which should be suitable for
;; most inheriting backends.

(defun nnfeed--print-content (content attributes links)
  "Return CONTENT formatted according to ATTRIBUTES, with LINKS possibly added."
  (let ((links (and (memq 'links attributes) links)))
    (when (or content links)
      (concat
       (and content (format "%s\n\n" content))
       (mapconcat (lambda (link)
                    (cdr (assoc (cdr (assq 'Content-Type attributes)) link
                                (lambda (types type)
                                  (if (stringp types) (string= types type)
                                    (member type types))))))
                  links)))))

(defvoo nnfeed-print-content-function #'nnfeed--print-content
  "Function returning a single piece of content for an article (a string).

It should accept three arguments, a part and its attributes (as returned
by `nnfeed-read-parts-function'), and links (as returned by
`nnfeed-read-links-function').")

(defun nnfeed--print-part (content headers mime links)
  "Print part of an article using its CONTENT, HEADERS, and LINKS.
Only HEADERS of a type included in MIME are considered."
  (concat
   (mapconcat (lambda (header)
                (when-let* ((m (car-safe header))
                            ((member m mime)))
                  (format "%s: %s\n" m (cdr header))))
              headers)
   "\n"
   (funcall nnfeed-print-content-function content headers links)))

(deffoo nnfeed-request-article (article &optional group server to-buffer)
  (if-let* ((server (or server (nnfeed--current-server-no-prefix)))
            (g (or (nnfeed--group-data group server)
                   (and (setq group nnfeed-group)
                        `[ nil ,nnfeed-group-article-ids
                           ,nnfeed-group-articles
                           ,nnfeed-group-article-max-num
                           ,nnfeed-group-article-min-num nil])))
            (num (or (and (stringp article)
                          (gethash article (aref g 1)))
                     (and (numberp article) article)))
            ((and (<= num (aref g 3))
                  (>= num (aref g 4))))
            (a (gethash num (aref g 2))))
      (with-current-buffer (or to-buffer nntp-server-buffer)
        (erase-buffer)
        (let* ((links (aref a 5))
               (parts (aref a 6))
               (headers (aref a 7))
               (boundary (or (cdr (assq :boundary headers))
                             (format "-_%s_-" nnfeed-backend)))
               (multi (length> parts 1))
               (mime '( Content-Type Content-Disposition
                        Content-Transfer-Encoding)))
          (insert (format
                   "Subject: %s\nFrom: %s\nDate: %s\nMessage-ID: %s\n"
                   (aref a 2) (aref a 1)
                   (format-time-string
                    nnfeed-date-format (or (aref a 3) '(0 0)))
                   (aref a 0))
                  (if (assq 'MIME-Version headers) "" "MIME-Version: 1.0\n")
                  (mapconcat (lambda (header)
                               (unless (keywordp (car header))
                                 (format "%s: %s\n" (car header) (cdr header))))
                             headers)
                  (if multi
                      (if (assq 'Content-Type headers) ""
                        (format
                         "Content-Type: multipart/alternative; boundary=%s\n"
                         boundary))
                    (prog1 (nnfeed--print-part
                            (caar parts) (cdar parts) mime links)
                      (setq parts nil)))
                  (mapconcat (lambda (part)
                               (format "--%s\n%s\n" boundary
                                       (nnfeed--print-part
                                        (car part) (cdr part) mime links)))
                             parts)
                  (if multi (format "--%s--" boundary) "\n")))
        `(,group . ,num))
    (nnheader-report nnfeed-backend "No such article")))

(deffoo nnfeed-request-group (group &optional server fast _info)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (if-let* ((server (or server (nnfeed--current-server-no-prefix)))
              (g (or (if fast (nnfeed--group-data group server)
                       (setq server (nnfeed--parse-feed server group))
                       (and (hash-table-p server) (gethash group server)))
                     `[ ,group ,(make-hash-table :test 'equal)
                        ,(make-hash-table :test 'eql) 0 1 ""])))
        (progn
          (setq nnfeed-group group
                nnfeed-group-article-ids (aref g 1)
                nnfeed-group-articles (aref g 2)
                nnfeed-group-article-max-num (aref g 3)
                nnfeed-group-article-min-num (aref g 4))
          (insert (format "211 %s %s %s \"%s\""
                          (hash-table-count nnfeed-group-article-ids)
                          nnfeed-group-article-min-num
                          nnfeed-group-article-max-num group))
          t)
      (insert "404 group not found")
      (nnheader-report nnfeed-backend "Group %s not found" group))))

(deffoo nnfeed-close-group (group &optional server)
  (and (string= group nnfeed-group)
       (setq nnfeed-group nil
             nnfeed-group-article-ids (make-hash-table :test 'equal)
             nnfeed-group-articles (make-hash-table :test 'eql)
             nnfeed-group-article-max-num 0
             nnfeed-group-article-min-num 1))
  (setq server (or server (nnfeed--current-server-no-prefix)))
  (nnfeed--write-server server))

(deffoo nnfeed-request-list (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (if-let* ((p (point))
              (s (nnfeed--parse-feed
                  (or server (nnfeed--current-server-no-prefix))))
              ((hash-table-p s)))
        (progn
          (maphash (lambda (group g)
                     (insert (format "\"%s\" %s %s y\n"
                                     group (aref g 3) (aref g 4))))
                   s)
          (not (= (point) p)))
      (nnheader-report 'nnfeed (nnheader-get-report nnfeed-backend)))))

(deffoo nnfeed-request-post (&optional _server)
  (nnheader-report nnfeed-backend "%s is a read only backend" nnfeed-backend))

;;;; Optional back end functions:

(deffoo nnfeed-retrieve-groups (_groups &optional server)
  (nnfeed-request-list server)
  'active)

(deffoo nnfeed-request-type (_group &optional _article)
  'unknown)

;; FIXME: Works incorrectly when a group name contains spaces as Gnus actually
;; separates the group name from the description with either a tab or a space.
(defun nnfeed--group-description (name group)
  "Return a description line for a GROUP called NAME."
  (when-let* ((desc (aref group 5))
              ((not (string-blank-p desc))))
    (insert name "\t" desc "\n")))

(deffoo nnfeed-request-group-description (group &optional server)
  (when-let* ((server (or server (nnfeed--current-server-no-prefix)))
             (g (nnfeed--group-data group server)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (nnfeed--group-description group g)
      t)))

(deffoo nnfeed-request-list-newsgroups (&optional server)
  (when-let* ((server (or server (nnfeed--current-server-no-prefix)))
              (s (gethash (nnfeed--server-address server) nnfeed-servers))
              ((hash-table-p s)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (maphash #'nnfeed--group-description s)
      t)))

(deffoo nnfeed-request-rename-group (group new-name &optional server)
  (when-let* ((server (or server (nnfeed--current-server-no-prefix)))
              (a (nnfeed--server-address server))
              (s (or (gethash a nnfeed-servers)
                     (and ; Open the server to add it to `nnfeed-servers'
                      (save-match-data
                        (nnfeed-open-server
                         server
                         (cdr ; Get defs and backend.
                          (assoc a (cdr (assq nnfeed-backend nnoo-state-alist))
                                 (lambda (car key)
                                   (and (stringp car)
                                        (string-match
                                         (concat
                                          "\\`\\(\\(nn[[:alpha:]]+\\)\\+\\)?"
                                          (regexp-quote key) "\\'")
                                         car)
                                        (setq server car)))))
                         (if (match-string 1 server)
                             (intern (match-string 2 server)) 'nnfeed)))
                      (gethash a nnfeed-servers))))
              (g (or (nnfeed--group-data group a)
                     `[ ,group ,(make-hash-table :test 'equal)
                        ,(make-hash-table :test 'eql) nil 1 ""])))
    (puthash new-name g s)
    (puthash group new-name nnfeed-group-names)
    (remhash group s)
    (and (string= group nnfeed-group)
         (setq nnfeed-group new-name))
    t))

(provide 'nnfeed)

;;; nnfeed.el ends here
