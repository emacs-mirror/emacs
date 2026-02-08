;;; nnatom.el --- Atom backend for Gnus -*- lexical-binding: t -*-

;; Copyright (C) 2023, 2025-2026 Free Software Foundation, Inc.
;; Author: Daniel Semyonov <daniel@dsemy.com>

;; This file is part of GNU Emacs.

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

;; Gnus backend for HTTP or local feeds following the
;; Atom Syndication Format <https://www.ietf.org/rfc/rfc4287>.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'nnfeed)
(require 'mm-url)
(require 'dom)

(defgroup nnatom nil
  "Atom backend for Gnus."
  :group 'nnfeed)

(nnoo-declare nnatom nnfeed)

(nnfeed-define-basic-backend-interface nnatom)

;;;; Atom feed parser:

(declare-function libxml-parse-xml-region "xml.c")
(defun nnatom--read-feed (feed _)
  "Return a list structure representing FEED, or nil."
  (if (string-match-p "\\`https?://" feed)
      (nnheader-report
       nnatom-backend
       "Address shouldn't start with \"http://\" or \"https://\"")
    (with-temp-buffer
      (condition-case e
          (if (file-name-absolute-p feed)
              (insert-file-contents feed)
            (mm-url-insert-file-contents (concat "https://" feed)))
        (file-error (nnheader-report nnatom-backend (cdr e)))
        (:success (when-let* ((data (if (libxml-available-p)
                                        (libxml-parse-xml-region
                                         (point-min) (point-max))
                                      (car (xml-parse-region
                                            (point-min) (point-max)))))
                              (authors (list 'authors)))
                    (when (eq (car data) 'top)
                      (setq data (assq 'feed data)))
                    (dom-add-child-before data authors)
                    (let ((all (dom-children data)))
                      (while-let ((rest (cdr all))
                                  (child (car-safe rest))
                                  (type (car-safe child))
                                  ((not (eq type 'entry))))
                        (and (or (eq type 'author)
                                 (eq type 'contributor))
                             (dom-add-child-before authors child))
                        (setq all rest))
                      ;; Order of entries is reversed as most Atom feeds
                      ;; list only the "most recent" entries, in reverse
                      ;; chronological order.
                      (setcdr all (nreverse (cdr all))))
                    data))))))
(defvoo nnatom-read-feed-function #'nnatom--read-feed
  nil nnfeed-read-feed-function)

(defun nnatom--read-group (data)
  "Return the next group and the remaining DATA in a cons cell, or nil."
  `(,data))
(defvoo nnatom-read-group-function #'nnatom--read-group
  nil nnfeed-read-group-function)

(defun nnatom--read-article (data _)
  "Return the next article and the remaining DATA in a cons cell, or nil."
  (when (eq (car data) 'feed) (setq data (dom-children data)))
  ;; Discard any children between/after entries.
  (while (and data (not (eq (car-safe (car data)) 'entry))) (pop data))
  (when-let* ((article (car data))
              (auths (list 'authors)) (links (list 'links)))
    (dom-add-child-before article links)
    (dom-add-child-before article auths)
    (dolist (child (cddddr article) `(,article . ,(cdr data)))
      (pcase (car-safe child)                ; Authors and links can appear
        ((or 'author 'contributor)           ; anywhere in the entry so we
         (dom-add-child-before auths child)  ; collect them all here to
         (dom-add-child-before links child)) ; avoid looping over the
        ((or 'link                           ; entry multiple times later.
             (and 'content (guard (assq 'src (dom-attributes child)))))
         (dom-add-child-before links child))))))
(defvoo nnatom-read-article-function #'nnatom--read-article
  nil nnfeed-read-article-function)

(defun nnatom--dom-line (node)
  "Return NODE's text as a single, whitespace-trimmed line."
  (string-trim (replace-regexp-in-string
                "[\r\n]+" " " (dom-inner-text node) t)))

(defun nnatom--read-title (group)
  "Return the title of GROUP, or nil."
  (nnatom--dom-line (dom-child-by-tag group 'title)))
(defvoo nnatom-read-title-function #'nnatom--read-title
  nil nnfeed-read-title-function)

(defun nnatom--read-description (group)
  "Return the description of GROUP, or nil."
  (nnatom--dom-line (dom-child-by-tag group 'subtitle)))
(defvoo nnatom-read-description-function #'nnatom--read-description
  nil nnfeed-read-description-function)

(defun nnatom--read-article-or-group-authors (article-or-group)
  "Return the authors of ARTICLE-OR-GROUP, or nil."
  (when-let*
      ((a (mapconcat
           (lambda (author)
             (let* ((name (nnatom--dom-line (dom-child-by-tag author 'name)))
                    (name (unless (string-blank-p name) name))
                    (email (nnatom--dom-line (dom-child-by-tag author 'email)))
                    (email (unless (string-blank-p email) email)))
               (or (and name email (format "%s <%s>" name email)) name email)))
           (dom-children (dom-child-by-tag article-or-group 'authors))
           ", "))
       ((not (string-blank-p a))))
    a))
(defvoo nnatom-read-author-function #'nnatom--read-article-or-group-authors
  nil nnfeed-read-author-function)
(defvoo nnatom-read-group-author-function
    #'nnatom--read-article-or-group-authors
  nil nnfeed-read-group-author-function)

(defun nnatom--read-subject (article)
  "Return the subject of ARTICLE, or nil."
  (nnatom--dom-line (dom-child-by-tag article 'title)))
(defvoo nnatom-read-subject-function #'nnatom--read-subject
  nil nnfeed-read-subject-function)

(defun nnatom--read-id (article)
  "Return the ID of ARTICLE.
If the ARTICLE doesn't contain an ID but it does contain a subject,
return the subject.  Otherwise, return nil."
  (or (nnatom--dom-line (dom-child-by-tag article 'id))
      (nnatom--read-subject article)))
(defvoo nnatom-read-id-function #'nnatom--read-id
  nil nnfeed-read-id-function)

(defun nnatom--read-publish (article)
  "Return the date and time ARTICLE was published, or nil."
  (when-let* ((d (dom-child-by-tag article 'published)))
    (date-to-time (nnatom--dom-line d))))
(defvoo nnatom-read-publish-date-function #'nnatom--read-publish
  nil nnfeed-read-publish-date-function)

(defun nnatom--read-update (article)
  "Return the date and time of the last update to ARTICLE, or nil."
  (when-let* ((d (dom-child-by-tag article 'updated)))
    (date-to-time (nnatom--dom-line d))))
(defvoo nnatom-read-update-date-function #'nnatom--read-update
  nil nnfeed-read-update-date-function)

(defun nnatom--read-links (article)
  "Return all links contained in ARTICLE, or nil."
  (let ((alt 0) (rel 0) (sel 0) (enc 0) (via 0) (aut 0))
    (mapcan
     (lambda (link)
       (when-let* ((l (car-safe link)))
         (or
          (when-let* (((eq l 'content))
                      (src (dom-attr link 'src))
                      (label (concat "Link"
                                     (and (< 1 (incf alt))
                                          (format " %s" alt)))))
            `(((("text/plain") . ,(format "%s: %s\n" label src))
               (("text/html") . ,(format "<a href=\"%s\">[%s]</a> "
                                         src label)))))
          (when-let* (((or (eq l 'author) (eq l 'contributor)))
                      (name (nnatom--dom-line (dom-child-by-tag link 'name)))
                      (name (if (string-blank-p name)
                                (concat "Author"
                                        (and (< 1 (incf aut))
                                             (format " %s" aut)))
                              name))
                      (uri (nnatom--dom-line (dom-child-by-tag link 'uri)))
                      ((not (string-blank-p uri))))
            `(((("text/plain") . ,(format "%s: %s\n" name uri))
               (("text/html") . ,(format "<a href=\"%s\">[%s]</a> "
                                         uri name)))))
          (when-let* (((eq l 'link))
                      (attrs (dom-attributes link))
                      (label (or (cdr (assq 'title attrs))
                                 (pcase (cdr (assq 'rel attrs))
                                   ("related"
                                    (concat "Related"
                                            (and (< 1 (incf rel))
                                                 (format " %s" rel))))
                                   ("self"
                                    (concat "More"
                                            (and (< 1 (incf sel))
                                                 (format " %s" sel))))
                                   ("enclosure"
                                    (concat "Enclosure"
                                            (and (< 1 (incf enc))
                                                 (format " %s" enc))))
                                   ("via"
                                    (concat "Source"
                                            (and (< 1 (incf via))
                                                 (format " %s" via))))
                                   (_ (if-let*
                                          ((lang (cdr (assq 'hreflang link))))
                                          (format "Link (%s)" lang)
                                        (concat
                                         "Link"
                                         (and (< 1 (incf alt))
                                              (format " %s" alt))))))))
                      (link (cdr (assq 'href attrs))))
            `(((("text/plain") . ,(format "%s: %s\n" label link))
               (("text/html") . ,(format "<a href=\"%s\">[%s]</a> "
                                         link label))))))))
     (dom-children (dom-child-by-tag article 'links)))))
(defvoo nnatom-read-links-function #'nnatom--read-links
  nil nnfeed-read-links-function)

(defun nnatom--read-part (part type)
  (let* ((atypes '("html" "plain"))
         (mtypes '(("xhtml" . "text/html") ("text" . "text/plain")))
         (xsuff (concat "[+/]xml\\(-\\(dtd\\|external-parsed-entity\\)\\)?\\'"
                        "\\|^text"))
         (part (if (string= type "xhtml")
                   (with-temp-buffer
                     (dom-print (dom-child-by-tag part 'div) nil t)
                     (buffer-substring-no-properties
                      (point-min) (point-max)))
                 (dom-inner-text part)))
         (type (if (member type atypes) (concat "text/" type) type))
         (type (or (cdr (assoc type mtypes)) type)))
    (unless (string-blank-p part)
      `(,part (Content-Type . ,(or type (setq type "text/plain")))
              ,(and (not (string-match-p xsuff type))
                    '(Content-Transfer-Encoding . "base64"))))))

(defun nnatom--read-parts (article)
  "Return all parts contained in ARTICLE, or an empty HTML part with links."
  (let* ((summary (dom-child-by-tag article 'summary))
         (stype (cdr (assq 'type (dom-attributes summary))))
         (summary (nnatom--read-part summary stype))
         (content (dom-child-by-tag article 'content))
         (ctype (cdr (assq 'type (dom-attributes content))))
         (content (nnatom--read-part content ctype))
         (st (string= stype ctype))
         parts)
    (cond ((and summary content)
           (and st (push summary parts))
           (push (append content '(links)) parts)
           (or st (push summary parts)))
          ((setq content (or summary content))
           (push (append content '(links)) parts))
          (t (push '(nil (Content-Type . "text/html") links) parts)))
    parts))
(defvoo nnatom-read-parts-function #'nnatom--read-parts
  nil nnfeed-read-parts-function)

(gnus-declare-backend (symbol-name nnatom-backend) 'none 'address)

(provide 'nnatom)

;;; nnatom.el ends here
