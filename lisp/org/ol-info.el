;;; ol-info.el --- Links to Info Nodes               -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2023 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, wp
;; URL: https://orgmode.org
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements links to Info nodes from within Org mode.
;; Org mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ol)

;; Declare external functions and variables

(declare-function Info-find-node "info"
                  (filename nodename &optional no-going-back strict-case))
(defvar Info-current-file)
(defvar Info-current-node)

;; Install the link type
(org-link-set-parameters "info"
			 :follow #'org-info-open
			 :export #'org-info-export
			 :store #'org-info-store-link
                         :insert-description #'org-info-description-as-command)

;; Implementation
(defun org-info-store-link ()
  "Store a link to an Info file and node."
  (when (eq major-mode 'Info-mode)
    (let ((link (concat "info:"
			(file-name-nondirectory Info-current-file)
			"#" Info-current-node))
	  (desc (concat (file-name-nondirectory Info-current-file)
			"#" Info-current-node)))
      (org-link-store-props :type "info" :file Info-current-file
			    :node Info-current-node
			    :link link :description desc)
      link)))

(defun org-info-open (path _)
  "Follow an Info file and node link specified by PATH."
  (org-info-follow-link path))

(defun org-info--link-file-node (path)
  "Extract file name and node from info link PATH.

Return cons consisting of file name and node name or \"Top\" if node
part is not specified.  Components may be separated by \":\" or by \"#\".
File may be a virtual one, see `Info-virtual-files'."
  (if (not path)
      '("dir" . "Top")
    (string-match "\\`\\([^#:]*\\)\\(?:[#:]:?\\(.*\\)\\)?\\'" path)
    (let* ((node (match-string 2 path))
           ;; Do not reorder, `org-trim' modifies match.
           (file (org-trim (match-string 1 path))))
      (cons
       (if (org-string-nw-p file) file "dir")
       (if (org-string-nw-p node) (org-trim node) "Top")))))

(defun org-info-description-as-command (link desc)
  "Info link description that can be pasted as command.

For the following LINK

    \"info:elisp#Non-ASCII in Strings\"

the result is

    info \"(elisp) Non-ASCII in Strings\"

that may be executed as shell command or evaluated by
\\[eval-expression] (wrapped with parenthesis) to read the manual
in Emacs.

Calling convention is similar to `org-link-make-description-function'.
DESC has higher priority and returned when it is not nil or empty string.
If LINK is not an info link then DESC is returned."
  (let* ((prefix "info:")
         (need-file-node (and (not (org-string-nw-p desc))
                              (string-prefix-p prefix link))))
    (pcase (and need-file-node
                (org-info--link-file-node (org-unbracket-string prefix "" link)))
      ;; Unlike (info "dir"), "info dir" shell command opens "(coreutils)dir invocation".
      (`("dir" . "Top") "info \"(dir)\"")
      (`(,file . "Top") (format "info %s" file))
      (`(,file . ,node) (format "info \"(%s) %s\"" file node))
      (_ desc))))

(defun org-info-follow-link (name)
  "Follow an Info file and node link specified by NAME."
  (pcase-let ((`(,filename . ,nodename-or-index)
	       (org-info--link-file-node name)))
    (require 'info)
    ;; If nodename-or-index is invalid node name, then look it up
    ;; in the index.
    (condition-case nil
        (Info-find-node filename nodename-or-index)
      (user-error (Info-find-node filename "Top")
                  (condition-case nil
                      (Info-index nodename-or-index)
                    (user-error "Could not find '%s' node or index entry"
                                nodename-or-index))))))

(defconst org-info-emacs-documents
  '("ada-mode" "auth" "autotype" "bovine" "calc" "ccmode" "cl" "dbus" "dired-x"
    "ebrowse" "ede" "ediff" "edt" "efaq-w32" "efaq" "eieio" "eintr" "elisp"
    "emacs-gnutls" "emacs-mime" "emacs" "epa" "erc" "ert" "eshell" "eudc" "eww"
    "flymake" "forms" "gnus" "htmlfontify" "idlwave" "ido" "info" "mairix-el"
    "message" "mh-e" "newsticker" "nxml-mode" "octave-mode" "org" "pcl-cvs"
    "pgg" "rcirc" "reftex" "remember" "sasl" "sc" "semantic" "ses" "sieve"
    "smtpmail" "speedbar" "srecode" "todo-mode" "tramp" "url" "vip" "viper"
    "widget" "wisent" "woman")
  "List of Emacs documents available.
Taken from <https://www.gnu.org/software/emacs/manual/html_mono/.>")

(defconst org-info-other-documents
  '(("dir" . "https://www.gnu.org/manual/manual.html") ; index
    ("libc" . "https://www.gnu.org/software/libc/manual/html_mono/libc.html")
    ("make" . "https://www.gnu.org/software/make/manual/make.html"))
  "Alist of documents generated from Texinfo source.
When converting info links to HTML, links to any one of these manuals are
converted to use these URL.")

(defun org-info-map-html-url (filename)
  "Return URL or HTML file associated to Info FILENAME.
If FILENAME refers to an official GNU document, return a URL pointing to
the official page for that document, e.g., use \"gnu.org\" for all Emacs
related documents.  Otherwise, append \".html\" extension to FILENAME.
See `org-info-emacs-documents' and `org-info-other-documents' for details."
  (cond ((member filename org-info-emacs-documents)
	 (format "https://www.gnu.org/software/emacs/manual/html_mono/%s.html"
		 filename))
	((cdr (assoc filename org-info-other-documents)))
	(t (concat filename ".html"))))

(defun org-info--expand-node-name (node)
  "Expand Info NODE to HTML cross reference."
  ;; See (info "(texinfo) HTML Xref Node Name Expansion") for the
  ;; expansion rule.
  (let ((node (replace-regexp-in-string
	       "\\([ \t\n\r]+\\)\\|\\([^a-zA-Z0-9]\\)"
	       (lambda (m)
		 (if (match-end 1) "-" (format "_%04x" (string-to-char m))))
	       (org-trim node))))
    (cond ((string= node "") "")
	  ((string-match-p "\\`[0-9]" node) (concat "g_t" node))
	  (t node))))

(defun org-info-export (path desc format)
  "Export an info link.
See `org-link-parameters' for details about PATH, DESC and FORMAT."
  (pcase-let ((`(,manual . ,node) (org-info--link-file-node path)))
    (pcase format
      (`html
       (format "<a href=\"%s#%s\">%s</a>"
	       (org-info-map-html-url manual)
	       (org-info--expand-node-name node)
	       (or desc path)))
      (`texinfo
       (let ((title (or desc "")))
	 (format "@ref{%s,%s,,%s,}" node title manual)))
      (_ nil))))

(provide 'ol-info)

;;; ol-info.el ends here
