;;; gnus-picon.el --- displaying pretty icons in Gnus

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news xpm annotation glyph faces

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; There are three picon types relevant to Gnus:
;;
;; Persons: person@subdomain.dom
;;          users/dom/subdomain/person/face.gif
;;          usenix/dom/subdomain/person/face.gif
;;          misc/MISC/person/face.gif
;; Domains: subdomain.dom
;;          domain/dom/subdomain/unknown/face.gif
;; Groups:  comp.lang.lisp
;;          news/comp/lang/lisp/unknown/face.gif
;;
;; Original implementation by Wes Hardaker <hardaker@ece.ucdavis.edu>.
;;
;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'gnus-art)

;;; User variables:

(defcustom gnus-picon-news-directories '("news")
  "*List of directories to search for newsgroups faces."
  :type '(repeat string)
  :group 'gnus-picon)

(defcustom gnus-picon-user-directories '("users" "usenix" "local" "misc")
  "*List of directories to search for user faces."
  :type '(repeat string)
  :group 'gnus-picon)

(defcustom gnus-picon-domain-directories '("domains")
  "*List of directories to search for domain faces.
Some people may want to add \"unknown\" to this list."
  :type '(repeat string)
  :group 'gnus-picon)

(defcustom gnus-picon-file-types
  (let ((types (list "xbm")))
    (when (gnus-image-type-available-p 'gif)
      (push "gif" types))
    (when (gnus-image-type-available-p 'xpm)
      (push "xpm" types))
    types)
  "*List of suffixes on picon file names to try."
  :type '(repeat string)
  :group 'gnus-picon)

(defface gnus-picon-xbm '((t (:foreground "black" :background "white")))
  "Face to show xbm picon in."
  :group 'gnus-picon)
;; backward-compatibility alias
(put 'gnus-picon-xbm-face 'face-alias 'gnus-picon-xbm)

(defface gnus-picon '((t (:foreground "black" :background "white")))
  "Face to show picon in."
  :group 'gnus-picon)
;; backward-compatibility alias
(put 'gnus-picon-face 'face-alias 'gnus-picon)

;;; Internal variables:

(defvar gnus-picon-setup-p nil)
(defvar gnus-picon-glyph-alist nil
  "Picon glyphs cache.
List of pairs (KEY . GLYPH) where KEY is either a filename or an URL.")
(defvar gnus-picon-cache nil)

;;; Functions:

(defsubst gnus-picon-split-address (address)
  (setq address (split-string address "@"))
  (if (stringp (cadr address))
      (cons (car address) (split-string (cadr address) "\\."))
    (if (stringp (car address))
	(split-string (car address) "\\."))))

(defun gnus-picon-find-face (address directories &optional exact)
  (let* ((address (gnus-picon-split-address address))
	 (user (pop address))
	 (faddress address)
	 database directory result instance base)
    (catch 'found
      (dolist (database gnus-picon-databases)
	(dolist (directory directories)
	  (setq address faddress
		base (expand-file-name directory database))
	  (while address
	    (when (setq result (gnus-picon-find-image
				(concat base "/" (mapconcat 'downcase
							    (reverse address)
							    "/")
					"/" (downcase user) "/")))
	      (throw 'found result))
	    (if exact
		(setq address nil)
	      (pop address)))
	  ;; Kludge to search MISC as well.  But not in "news".
	  (unless (string= directory "news")
	    (when (setq result (gnus-picon-find-image
				(concat base "/MISC/" user "/")))
	      (throw 'found result))))))))

(defun gnus-picon-find-image (directory)
  (let ((types gnus-picon-file-types)
	found type file)
    (while (and (not found)
		(setq type (pop types)))
      (setq found (file-exists-p (setq file (concat directory "face." type)))))
    (if found
	file
      nil)))

(defun gnus-picon-insert-glyph (glyph category)
  "Insert GLYPH into the buffer.
GLYPH can be either a glyph or a string."
  (if (stringp glyph)
      (insert glyph)
    (gnus-add-wash-type category)
    (gnus-add-image category (car glyph))
    (gnus-put-image (car glyph) (cdr glyph) category)))

(defun gnus-picon-create-glyph (file)
  (or (cdr (assoc file gnus-picon-glyph-alist))
      (cdar (push (cons file (gnus-create-image file))
		  gnus-picon-glyph-alist))))

;;; Functions that does picon transformations:

(defun gnus-picon-transform-address (header category)
  (gnus-with-article-headers
    (let ((addresses
	   (mail-header-parse-addresses
	    ;; mail-header-parse-addresses does not work (reliably) on
	    ;; decoded headers.
	    (or
	     (ignore-errors
	       (mail-encode-encoded-word-string
		(or (mail-fetch-field header) "")))
	     (mail-fetch-field header))))
	  spec file point cache)
      (dolist (address addresses)
	(setq address (car address))
	(when (and (stringp address)
		   (setq spec (gnus-picon-split-address address)))
	  (if (setq cache (cdr (assoc address gnus-picon-cache)))
	      (setq spec cache)
	    (when (setq file (or (gnus-picon-find-face
				  address gnus-picon-user-directories)
				 (gnus-picon-find-face
				  (concat "unknown@"
					  (mapconcat
					   'identity (cdr spec) "."))
				  gnus-picon-user-directories)))
	      (setcar spec (cons (gnus-picon-create-glyph file)
				 (car spec))))

	    (dotimes (i (1- (length spec)))
	      (when (setq file (gnus-picon-find-face
				(concat "unknown@"
					(mapconcat
					 'identity (nthcdr (1+ i) spec) "."))
				gnus-picon-domain-directories t))
		(setcar (nthcdr (1+ i) spec)
			(cons (gnus-picon-create-glyph file)
			      (nth (1+ i) spec)))))
	    (setq spec (nreverse spec))
	    (push (cons address spec) gnus-picon-cache))

	  (gnus-article-goto-header header)
	  (mail-header-narrow-to-field)
	  (when (search-forward address nil t)
	    (delete-region (match-beginning 0) (match-end 0))
	    (setq point (point))
	    (while spec
	      (goto-char point)
	      (if (> (length spec) 2)
		  (insert ".")
		(if (= (length spec) 2)
		  (insert "@")))
	      (gnus-picon-insert-glyph (pop spec) category))))))))

(defun gnus-picon-transform-newsgroups (header)
  (interactive)
  (gnus-with-article-headers
    (gnus-article-goto-header header)
    (mail-header-narrow-to-field)
    (let ((groups (message-tokenize-header (mail-fetch-field header)))
	  spec file point)
      (dolist (group groups)
	(unless (setq spec (cdr (assoc group gnus-picon-cache)))
	  (setq spec (nreverse (split-string group "[.]")))
	  (dotimes (i (length spec))
	    (when (setq file (gnus-picon-find-face
			      (concat "unknown@"
				      (mapconcat
				       'identity (nthcdr i spec) "."))
			      gnus-picon-news-directories t))
	      (setcar (nthcdr i spec)
		      (cons (gnus-picon-create-glyph file)
			    (nth i spec)))))
	    (push (cons group spec) gnus-picon-cache))
	(when (search-forward group nil t)
	  (delete-region (match-beginning 0) (match-end 0))
	  (save-restriction
	    (narrow-to-region (point) (point))
	    (while spec
	      (goto-char (point-min))
	      (if (> (length spec) 1)
		  (insert "."))
	      (gnus-picon-insert-glyph (pop spec) 'newsgroups-picon))
	    (goto-char (point-max))))))))

;;; Commands:

;; #### NOTE: the test for buffer-read-only is the same as in
;; article-display-[x-]face. See the comment up there.

;;;###autoload
(defun gnus-treat-from-picon ()
  "Display picons in the From header.
If picons are already displayed, remove them."
  (interactive)
  (let ((wash-picon-p buffer-read-only))
    (gnus-with-article-buffer
      (if (and wash-picon-p (memq 'from-picon gnus-article-wash-types))
	  (gnus-delete-images 'from-picon)
	(gnus-picon-transform-address "from" 'from-picon)))
    ))

;;;###autoload
(defun gnus-treat-mail-picon ()
  "Display picons in the Cc and To headers.
If picons are already displayed, remove them."
  (interactive)
  (let ((wash-picon-p buffer-read-only))
    (gnus-with-article-buffer
      (if (and wash-picon-p (memq 'mail-picon gnus-article-wash-types))
	  (gnus-delete-images 'mail-picon)
	(gnus-picon-transform-address "cc" 'mail-picon)
	(gnus-picon-transform-address "to" 'mail-picon)))
    ))

;;;###autoload
(defun gnus-treat-newsgroups-picon ()
  "Display picons in the Newsgroups and Followup-To headers.
If picons are already displayed, remove them."
  (interactive)
  (let ((wash-picon-p buffer-read-only))
    (gnus-with-article-buffer
      (if (and wash-picon-p (memq 'newsgroups-picon gnus-article-wash-types))
	  (gnus-delete-images 'newsgroups-picon)
	(gnus-picon-transform-newsgroups "newsgroups")
	(gnus-picon-transform-newsgroups "followup-to")))
    ))

(provide 'gnus-picon)

;;; arch-tag: fe9aede0-1b1b-463a-b4ab-807f98bcb31f
;;; gnus-picon.el ends here
