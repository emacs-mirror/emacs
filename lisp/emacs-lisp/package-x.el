;;; package-x.el --- Package extras

;; Copyright (C) 2007-2018 Free Software Foundation, Inc.

;; Author: Tom Tromey <tromey@redhat.com>
;; Created: 10 Mar 2007
;; Keywords: tools
;; Package: package

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

;; This file currently contains parts of the package system that many
;; won't need, such as package uploading.

;; To upload to an archive, first set `package-archive-upload-base' to
;; some desired directory.  For testing purposes, you can specify any
;; directory you want, but if you want the archive to be accessible to
;; others via http, this is typically a directory in the /var/www tree
;; (possibly one on a remote machine, accessed via Tramp).

;; Then call M-x package-upload-file, which prompts for a file to
;; upload. Alternatively, M-x package-upload-buffer uploads the
;; current buffer, if it's visiting a package file.

;; Once a package is uploaded, users can access it via the Package
;; Menu, by adding the archive to `package-archives'.

;;; Code:

(require 'package)
(defvar gnus-article-buffer)

(defcustom package-archive-upload-base "/path/to/archive"
  "The base location of the archive to which packages are uploaded.
This should be an absolute directory name.  If the archive is on
another machine, you may specify a remote name in the usual way,
e.g. \"/ssh:foo@example.com:/var/www/packages/\".
See Info node `(emacs)Remote Files'.

Unlike `package-archives', you can't specify a HTTP URL."
  :type 'directory
  :group 'package
  :version "24.1")

(defvar package-update-news-on-upload nil
  "Whether uploading a package should also update NEWS and RSS feeds.")

(defun package--encode (string)
  "Encode a string by replacing some characters with XML entities."
  ;; We need a special case for translating "&" to "&amp;".
  (let ((index))
    (while (setq index (string-match "[&]" string index))
      (setq string (replace-match "&amp;" t nil string))
      (setq index (1+ index))))
  (while (string-match "[<]" string)
    (setq string (replace-match "&lt;" t nil string)))
  (while (string-match "[>]" string)
    (setq string (replace-match "&gt;" t nil string)))
  (while (string-match "[']" string)
    (setq string (replace-match "&apos;" t nil string)))
  (while (string-match "[\"]" string)
    (setq string (replace-match "&quot;" t nil string)))
  string)

(defun package--make-rss-entry (title text archive-url)
  (let ((date-string (format-time-string "%a, %d %B %Y %T %z")))
    (concat "<item>\n"
	    "<title>" (package--encode title) "</title>\n"
	    ;; FIXME: should have a link in the web page.
	    "<link>" archive-url "news.html</link>\n"
	    "<description>" (package--encode text) "</description>\n"
	    "<pubDate>" date-string "</pubDate>\n"
	    "</item>\n")))

(defun package--make-html-entry (title text)
  (concat "<li> " (format-time-string "%B %e") " - "
	  title " - " (package--encode text)
	  " </li>\n"))

(defun package--update-file (file tag text)
  "Update the package archive file named FILE.
FILE should be relative to `package-archive-upload-base'.
TAG is a string that can be found within the file; TEXT is
inserted after its first occurrence in the file."
  (setq file (expand-file-name file package-archive-upload-base))
  (save-excursion
    (let ((old-buffer (find-buffer-visiting file)))
      (with-current-buffer (let ((find-file-visit-truename t))
			     (or old-buffer (find-file-noselect file)))
	(goto-char (point-min))
	(search-forward tag)
	(forward-line)
	(insert text)
	(let ((file-precious-flag t))
	  (save-buffer))
	(unless old-buffer
	  (kill-buffer (current-buffer)))))))

(defun package--archive-contents-from-url (archive-url)
  "Parse archive-contents file at ARCHIVE-URL.
Return the file contents, as a string, or nil if unsuccessful."
  (when archive-url
    (with-temp-buffer
      (ignore-errors
	(url-insert-file-contents (concat archive-url "archive-contents"))
	(package-read-from-string
	 (buffer-substring-no-properties (point-min) (point-max)))))))

(defun package--archive-contents-from-file ()
  "Parse the archive-contents at `package-archive-upload-base'"
  (let ((file (expand-file-name "archive-contents"
				package-archive-upload-base)))
    (if (not (file-exists-p file))
	;; No existing archive-contents means a new archive.
	(list package-archive-version)
      (let ((dont-kill (find-buffer-visiting file)))
	(with-current-buffer (let ((find-file-visit-truename t))
			       (find-file-noselect file))
	  (prog1
	      (package-read-from-string
	       (buffer-substring-no-properties (point-min) (point-max)))
	    (unless dont-kill
	      (kill-buffer (current-buffer)))))))))

(defun package-maint-add-news-item (title description archive-url)
  "Add a news item to the webpages associated with the package archive.
TITLE is the title of the news item.
DESCRIPTION is the text of the news item."
  (interactive "sTitle: \nsText: ")
  (package--update-file "elpa.rss"
			"<description>"
			(package--make-rss-entry title description archive-url))
  (package--update-file "news.html"
			"New entries go here"
			(package--make-html-entry title description)))

(defun package--update-news (package version description archive-url)
  "Update the ELPA web pages when a package is uploaded."
  (package-maint-add-news-item (concat package " version " version)
			       description
			       archive-url))

(declare-function lm-commentary "lisp-mnt" (&optional file))
(defvar tar-data-buffer)

(defun package-upload-buffer-internal (pkg-desc extension &optional archive-url)
  "Upload a package whose contents are in the current buffer.
PKG-DESC is the `package-desc'.
EXTENSION is the file extension, a string.  It can be either
\"el\" or \"tar\".

The upload destination is given by `package-archive-upload-base'.
If its value is invalid, prompt for a directory.

Optional arg ARCHIVE-URL is the URL of the destination archive.
If it is non-nil, compute the new \"archive-contents\" file
starting from the existing \"archive-contents\" at that URL.  In
addition, if `package-update-news-on-upload' is non-nil, call
`package--update-news' to add a news item at that URL.

If ARCHIVE-URL is nil, compute the new \"archive-contents\" file
from the \"archive-contents\" at `package-archive-upload-base',
if it exists."
  (let ((package-archive-upload-base package-archive-upload-base))
    ;; Check if `package-archive-upload-base' is valid.
    (when (or (not (stringp package-archive-upload-base))
	      (equal package-archive-upload-base
		     (car-safe
		      (get 'package-archive-upload-base 'standard-value))))
      (setq package-archive-upload-base
	    (read-directory-name
	     "Base directory for package archive: ")))
    (unless (file-directory-p package-archive-upload-base)
      (if (y-or-n-p (format "%s does not exist; create it? "
			    package-archive-upload-base))
	  (make-directory package-archive-upload-base t)
	(error "Aborted")))
    (save-excursion
      (save-restriction
	(let* ((file-type (package-desc-kind pkg-desc))
	       (pkg-name (package-desc-name pkg-desc))
	       (requires (package-desc-reqs pkg-desc))
	       (desc (if (eq (package-desc-summary pkg-desc)
                             package--default-summary)
			 (read-string "Description of package: ")
		       (package-desc-summary pkg-desc)))
	       (split-version (package-desc-version pkg-desc))
	       (commentary
                (pcase file-type
                  (`single (lm-commentary))
                  (`tar nil))) ;; FIXME: Get it from the README file.
               (extras (package-desc-extras pkg-desc))
	       (pkg-version (package-version-join split-version))
	       (pkg-buffer (current-buffer)))

          ;; `package-upload-file' will error if given a directory,
          ;; but we check it here as well just in case.
          (when (eq 'dir file-type)
            (user-error "Can't upload directory, tar it instead"))
	  ;; Get archive-contents from ARCHIVE-URL if it's non-nil, or
	  ;; from `package-archive-upload-base' otherwise.
	  (let ((contents (or (package--archive-contents-from-url archive-url)
			      (package--archive-contents-from-file)))
		(new-desc (package-make-ac-desc
                           split-version requires desc file-type extras)))
	    (if (> (car contents) package-archive-version)
		(error "Unrecognized archive version %d" (car contents)))
	    (let ((elt (assq pkg-name (cdr contents))))
	      (if elt
		  (if (version-list-<= split-version
				       (package--ac-desc-version (cdr elt)))
		      (error "New package has smaller version: %s" pkg-version)
		    (setcdr elt new-desc))
		(setq contents (cons (car contents)
				     (cons (cons pkg-name new-desc)
					   (cdr contents))))))

	    ;; Now CONTENTS is the updated archive contents.  Upload
	    ;; this and the package itself.  For now we assume ELPA is
	    ;; writable via file primitives.
	    (let ((print-level nil)
                  (print-quoted t)
		  (print-length nil))
	      (write-region (concat (pp-to-string contents) "\n")
			    nil
			    (expand-file-name "archive-contents"
					      package-archive-upload-base)))

	    ;; If there is a commentary section, write it.
	    (when commentary
	      (write-region commentary nil
	        	    (expand-file-name
	        	     (concat (symbol-name pkg-name) "-readme.txt")
	        	     package-archive-upload-base)))

	    (set-buffer (if (eq file-type 'tar) tar-data-buffer pkg-buffer))
	    (write-region (point-min) (point-max)
			  (expand-file-name
			   (format "%s-%s.%s" pkg-name pkg-version extension)
			   package-archive-upload-base)
			  nil nil nil 'excl)

	    ;; Write a news entry.
	    (and package-update-news-on-upload
		 archive-url
		 (package--update-news (format "%s.%s" pkg-name extension)
				       pkg-version desc archive-url))

	    ;; special-case "package": write a second copy so that the
	    ;; installer can easily find the latest version.
	    (if (eq pkg-name 'package)
		(write-region (point-min) (point-max)
			      (expand-file-name
			       (format "%s.%s" pkg-name extension)
			       package-archive-upload-base)
			      nil nil nil 'ask))))))))

(defun package-upload-buffer ()
  "Upload the current buffer as a single-file Emacs Lisp package.
If `package-archive-upload-base' does not specify a valid upload
destination, prompt for one."
  (interactive)
  (save-excursion
    (save-restriction
      ;; Find the package in this buffer.
      (let ((pkg-desc (package-buffer-info)))
	(package-upload-buffer-internal pkg-desc "el")))))

(defun package-upload-file (file)
  "Upload the Emacs Lisp package FILE to the package archive.
Interactively, prompt for FILE.  The package is considered a
single-file package if FILE ends in \".el\", and a multi-file
package if FILE ends in \".tar\".
If `package-archive-upload-base' does not specify a valid upload
destination, prompt for one."
  (interactive "fPackage file name: ")
  (with-temp-buffer
    (insert-file-contents file)
    (let ((pkg-desc
           (cond
            ((string-match "\\.tar\\'" file)
             (tar-mode) (package-tar-file-info))
            ((string-match "\\.el\\'" file) (package-buffer-info))
            (t (error "Unrecognized extension `%s'"
                      (file-name-extension file))))))
      (package-upload-buffer-internal pkg-desc (file-name-extension file)))))

(defun package-gnus-summary-upload ()
  "Upload a package contained in the current *Article* buffer.
This should be invoked from the gnus *Summary* buffer."
  (interactive)
  (with-current-buffer gnus-article-buffer
    (package-upload-buffer)))

(provide 'package-x)

;;; package-x.el ends here
