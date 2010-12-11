;;; org-bibtex.el --- Org links to BibTeX entries
;;
;; Copyright (C) 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
;;
;; Author: Bastien Guerry <bzg at altern dot org>
;;         Carsten Dominik <carsten dot dominik at gmail dot com>
;; Keywords: org, wp, remember
;; Version: 7.4
;;
;; This file is part of GNU Emacs.
;;
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
;;
;;; Commentary:
;;
;; This file implements links to database entries in BibTeX files.
;; Instead of defining a special link prefix, it uses the normal file
;; links combined with a custom search mechanism to find entries
;; by reference key.  And it constructs a nice description tag for
;; the link that contains the author name, the year and a short title.
;;
;; It also stores detailed information about the entry so that
;; remember templates can access and enter this information easily.
;;
;; The available properties for each entry are listed here:
;;
;; :author        :publisher      :volume      :pages
;; :editor        :url            :number      :journal
;; :title         :year           :series      :address
;; :booktitle     :month          :annote      :abstract
;; :key           :btype
;;
;; Here is an example of a remember template that use some of this
;; information (:author :year :title :journal :pages):
;;
;; (setq org-remember-templates
;;   '((?b "* READ %?\n\n%a\n\n%:author (%:year): %:title\n   \
;;          In %:journal, %:pages.")))
;;
;; Let's say you want to remember this BibTeX entry:
;;
;; @Article{dolev83,
;;   author = 	 {Danny Dolev and Andrew C. Yao},
;;   title = 	 {On the security of public-key protocols},
;;   journal = 	 {IEEE Transaction on Information Theory},
;;   year = 	 1983,
;;   volume =	 2,
;;   number =	 29,
;;   pages =	 {198--208},
;;   month =	 {Mars}
;; }
;;
;; M-x `org-remember' on this entry will produce this buffer:
;;
;; =====================================================================
;; * READ <== [point here]
;;
;; [[file:/file.bib::dolev83][Dolev & Yao 1983: security of public key protocols]]
;;
;; Danny Dolev and Andrew C. Yao (1983): On the security of public-key protocols
;; In IEEE Transaction on Information Theory, 198--208.
;; =====================================================================
;;
;;; History:
;;
;; The link creation part has been part of Org-mode for a long time.
;;
;; Creating better remember template information was inspired by a request
;; of Austin Frank: http://article.gmane.org/gmane.emacs.orgmode/4112
;; and then implemented by Bastien Guerry.
;;
;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;;; Code:

(require 'org)

(defvar description nil) ; dynamically scoped from org.el

(declare-function bibtex-beginning-of-entry "bibtex" ())
(declare-function bibtex-generate-autokey "bibtex" ())
(declare-function bibtex-parse-entry "bibtex" (&optional content))
(declare-function bibtex-url "bibtex" (&optional pos no-browse))

(org-add-link-type "bibtex" 'org-bibtex-open)
(add-hook 'org-store-link-functions 'org-bibtex-store-link)

;; (defun org-bibtex-publish (path)
;;   "Build the description of the BibTeX entry for publishing."
;;   (let* ((search (when (string-match "::\\(.+\\)\\'" path)
;; 		   (match-string 1 path)))
;; 	 (path (substring path 0 (match-beginning 0)))
;; 	 key)
;;     (with-temp-buffer
;;       (org-open-file path t nil search)
;;       (setq key (org-create-file-search-functions)))
;;     (or description key)))

(defun org-bibtex-open (path)
  "Visit the bibliography entry on PATH."
  (let* ((search (when (string-match "::\\(.+\\)\\'" path)
		   (match-string 1 path)))
	 (path (substring path 0 (match-beginning 0))))
    (org-open-file path t nil search)))

(defun org-bibtex-store-link ()
  "Store a link to a BibTeX entry."
  (when (eq major-mode 'bibtex-mode)
    (let* ((search (org-create-file-search-in-bibtex))
	   (link (concat "file:" (abbreviate-file-name buffer-file-name)
			 "::" search))
	   (entry (mapcar ; repair strings enclosed in "..." or {...}
		   (lambda(c)
		     (if (string-match
			  "^\\(?:{\\|\"\\)\\(.*\\)\\(?:}\\|\"\\)$" (cdr c))
			 (cons (car c) (match-string 1 (cdr c))) c))
		   (save-excursion
		     (bibtex-beginning-of-entry)
		     (bibtex-parse-entry)))))
      (org-store-link-props
       :key (cdr (assoc "=key=" entry))
       :author (or (cdr (assoc "author" entry)) "[no author]")
       :editor (or (cdr (assoc "editor" entry)) "[no editor]")
       :title (or (cdr (assoc "title" entry)) "[no title]")
       :booktitle (or (cdr (assoc "booktitle" entry)) "[no booktitle]")
       :journal (or (cdr (assoc "journal" entry)) "[no journal]")
       :publisher (or (cdr (assoc "publisher" entry)) "[no publisher]")
       :pages (or (cdr (assoc "pages" entry)) "[no pages]")
       :url (or (cdr (assoc "url" entry)) "[no url]")
       :year (or (cdr (assoc "year" entry)) "[no year]")
       :month (or (cdr (assoc "month" entry)) "[no month]")
       :address (or (cdr (assoc "address" entry)) "[no address]")
       :volume (or (cdr (assoc "volume" entry)) "[no volume]")
       :number (or (cdr (assoc "number" entry)) "[no number]")
       :annote (or (cdr (assoc "annote" entry)) "[no annotation]")
       :series (or (cdr (assoc "series" entry)) "[no series]")
       :abstract (or (cdr (assoc "abstract" entry)) "[no abstract]")
       :btype (or (cdr (assoc "=type=" entry)) "[no type]")
       :type "bibtex"
       :link link
       :description description))))

(defun org-create-file-search-in-bibtex ()
  "Create the search string and description for a BibTeX database entry."
  ;; Make a good description for this entry, using names, year and the title
  ;; Put it into the `description' variable which is dynamically scoped.
  (let ((bibtex-autokey-names 1)
	(bibtex-autokey-names-stretch 1)
	(bibtex-autokey-name-case-convert-function 'identity)
	(bibtex-autokey-name-separator " & ")
	(bibtex-autokey-additional-names " et al.")
	(bibtex-autokey-year-length 4)
	(bibtex-autokey-name-year-separator " ")
	(bibtex-autokey-titlewords 3)
	(bibtex-autokey-titleword-separator " ")
	(bibtex-autokey-titleword-case-convert-function 'identity)
	(bibtex-autokey-titleword-length 'infty)
	(bibtex-autokey-year-title-separator ": "))
    (setq description (bibtex-generate-autokey)))
  ;; Now parse the entry, get the key and return it.
  (save-excursion
    (bibtex-beginning-of-entry)
    (cdr (assoc "=key=" (bibtex-parse-entry)))))

(defun org-execute-file-search-in-bibtex (s)
  "Find the link search string S as a key for a database entry."
  (when (eq major-mode 'bibtex-mode)
    ;; Yes, we want to do the search in this file.
    ;; We construct a regexp that searches for "@entrytype{" followed by the key
    (goto-char (point-min))
    (and (re-search-forward (concat "@[a-zA-Z]+[ \t\n]*{[ \t\n]*"
				    (regexp-quote s) "[ \t\n]*,") nil t)
	 (goto-char (match-beginning 0)))
    (if (and (match-beginning 0) (equal current-prefix-arg '(16)))
	;; Use double prefix to indicate that any web link should be browsed
	(let ((b (current-buffer)) (p (point)))
	  ;; Restore the window configuration because we just use the web link
	  (set-window-configuration org-window-config-before-follow-link)
	  (with-current-buffer b
	    (goto-char p)
	    (bibtex-url)))
      (recenter 0))  ; Move entry start to beginning of window
  ;; return t to indicate that the search is done.
    t))

;; Finally add the link search function to the right hook.
(add-hook 'org-execute-file-search-functions 'org-execute-file-search-in-bibtex)

(provide 'org-bibtex)

;; arch-tag: 83987d5a-01b8-41c7-85bc-77700f1285f5

;;; org-bibtex.el ends here
