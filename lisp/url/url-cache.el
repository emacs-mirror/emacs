;;; url-cache.el --- Uniform Resource Locator retrieval tool
;; Author: $Author: fx $
;; Created: $Date: 2002/01/22 17:53:45 $
;; Version: $Revision: 1.4 $
;; Keywords: comm, data, processes, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry <wmperry@cs.indiana.edu>
;;; Copyright (c) 1996 - 1999 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'url-parse)

(defcustom url-cache-directory
  (expand-file-name "cache" url-configuration-directory)
  "*The directory where cache files should be stored."
  :type 'directory
  :group 'url-file)

;; Cache manager
(defun url-cache-file-writable-p (file)
  "Follows the documentation of `file-writable-p', unlike `file-writable-p'."
  (and (file-writable-p file)
       (if (file-exists-p file)
           (not (file-directory-p file))
         (file-directory-p (file-name-directory file)))))
                
(defun url-cache-prepare (file)
  "Makes it possible to cache data in FILE.
Creates any necessary parent directories, deleting any non-directory files
that would stop this.  Returns nil if parent directories can not be
created.  If FILE already exists as a non-directory, it changes
permissions of FILE or deletes FILE to make it possible to write a new
version of FILE.  Returns nil if this can not be done.  Returns nil if
FILE already exists as a directory.  Otherwise, returns t, indicating that
FILE can be created or overwritten."
  (cond
   ((url-cache-file-writable-p file)
    t)
   ((file-directory-p file)
    nil)
   (t
    (condition-case ()
	(or (make-directory (file-name-directory file) t) t)
      (error nil)))))

;;;###autoload
(defun url-store-in-cache (&optional buff)
  "Store buffer BUFF in the cache."
  (if (not (and buff (get-buffer buff)))
      nil
    (save-excursion
      (and buff (set-buffer buff))
      (let* ((fname (url-cache-create-filename (url-view-url t))))
	(if (url-cache-prepare fname)
	    (let ((coding-system-for-write 'binary))
	      (write-region (point-min) (point-max) fname nil 5)))))))
	
;;;###autoload
(defun url-is-cached (url)
  "Return non-nil if the URL is cached."
  (let* ((fname (url-cache-create-filename url))
	 (attribs (file-attributes fname)))
    (and fname				; got a filename
	 (file-exists-p fname)		; file exists
	 (not (eq (nth 0 attribs) t))	; Its not a directory
	 (nth 5 attribs))))		; Can get last mod-time

(defun url-cache-create-filename-human-readable (url)
  "Return a filename in the local cache for URL"
  (if url
      (let* ((url (if (vectorp url) (url-recreate-url url) url))
	     (urlobj (url-generic-parse-url url))
	     (protocol (url-type urlobj))
	     (hostname (url-host urlobj))
	     (host-components
	      (cons
	       (user-real-login-name)
	       (cons (or protocol "file")
		     (reverse (split-string (or hostname "localhost")
					    (eval-when-compile
					      (regexp-quote ".")))))))
	     (fname    (url-filename urlobj)))
	(if (and fname (/= (length fname) 0) (= (aref fname 0) ?/))
	    (setq fname (substring fname 1 nil)))
	(if fname
	    (let ((slash nil))
	      (setq fname
		    (mapconcat
		     (function
		      (lambda (x)
			(cond
			 ((and (= ?/ x) slash)
			  (setq slash nil)
			  "%2F")
			 ((= ?/ x)
			  (setq slash t)
			  "/")
			 (t
			  (setq slash nil)
			  (char-to-string x))))) fname ""))))

	(setq fname (and fname
			 (mapconcat
			  (function (lambda (x)
				      (if (= x ?~) "" (char-to-string x))))
			  fname ""))
	      fname (cond
		     ((null fname) nil)
		     ((or (string= "" fname) (string= "/" fname))
		      url-directory-index-file)
		     ((= (string-to-char fname) ?/)
		      (if (string= (substring fname -1 nil) "/")
			  (concat fname url-directory-index-file)
			(substring fname 1 nil)))
		     (t
		      (if (string= (substring fname -1 nil) "/")
			  (concat fname url-directory-index-file)
			fname))))
	(and fname
	     (expand-file-name fname
			       (expand-file-name
				(mapconcat 'identity host-components "/")
				url-cache-directory))))))

(defun url-cache-create-filename-using-md5 (url)
  "Create a cached filename using MD5.
 Very fast if you are in XEmacs, suitably fast otherwise."
  (require 'md5)
  (if url
      (let* ((url (if (vectorp url) (url-recreate-url url) url))
	     (checksum (md5 url))
	     (urlobj (url-generic-parse-url url))
	     (protocol (url-type urlobj))
	     (hostname (url-host urlobj))
	     (host-components
	      (cons
	       (user-real-login-name)
	       (cons (or protocol "file")
		     (nreverse
		      (delq nil
			    (split-string (or hostname "localhost")
					  (eval-when-compile
					    (regexp-quote "."))))))))
	     (fname    (url-filename urlobj)))
	(and fname
	     (expand-file-name checksum
			       (expand-file-name
				(mapconcat 'identity host-components "/")
				url-cache-directory))))))

(defcustom url-cache-creation-function 'url-cache-create-filename-using-md5
  "*What function to use to create a cached filename."
  :type '(choice (const :tag "MD5 of filename (low collision rate)"
			:value url-cache-create-filename-using-md5)
		 (const :tag "Human readable filenames (higher collision rate)"
			:value url-cache-create-filename-human-readable)
		 (function :tag "Other"))
  :group 'url-cache)

(defun url-cache-create-filename (url)
  (funcall url-cache-creation-function url))

;;;###autoload
(defun url-cache-extract (fnam)
  "Extract FNAM from the local disk cache"
  (erase-buffer)
  (insert-file-contents-literally fnam))

;;;###autoload
(defun url-cache-expired (url mod)
  "Return t iff a cached file has expired."
  (let* ((urlobj (if (vectorp url) url (url-generic-parse-url url)))
	 (type (url-type urlobj)))
    (cond
     (url-standalone-mode
      (not (file-exists-p (url-cache-create-filename url))))
     ((string= type "http")
      t)
     ((member type '("file" "ftp"))
      (if (or (equal mod '(0 0)) (not mod))
	  t
	(or (> (nth 0 mod) (nth 0 (current-time)))
	    (> (nth 1 mod) (nth 1 (current-time))))))
     (t nil))))

(provide 'url-cache)
