;;; url-file.el --- File retrieval code
;; Author: $Author: fx $
;; Created: $Date: 2002/04/22 09:14:24 $
;; Version: $Revision: 1.11 $
;; Keywords: comm, data, processes

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

(eval-when-compile (require 'cl))
(require 'mailcap)
(require 'url-vars)
(require 'url-parse)
(require 'url-dired)

(defconst url-file-default-port 21 "Default FTP port.")
(defconst url-file-asynchronous-p t "FTP transfers are asynchronous.")
(defalias 'url-file-expand-file-name 'url-default-expander)

(defun url-file-find-possibly-compressed-file (fname &rest args)
  "Find the exact file referenced by `fname'.
This tries the common compression extensions, because things like
ange-ftp and efs are not quite smart enough to realize when a server
can do automatic decompression for them, and won't find 'foo' if
'foo.gz' exists, even though the ftp server would happily serve it up
to them."
  (let ((scratch nil)
	(compressed-extensions '("" ".gz" ".z" ".Z" ".bz2"))
	(found nil))
    (while (and compressed-extensions (not found))
      (if (file-exists-p (setq scratch (concat fname (pop compressed-extensions))))
	  (setq found scratch)))
    found))

(defun url-file-host-is-local-p (host)
  "Return t iff HOST references our local machine."
  (let ((case-fold-search t))
    (or
     (null host)
     (string= "" host)
     (equal (downcase host) (downcase (system-name)))
     (and (string-match "^localhost$" host) t)
     (and (not (string-match (regexp-quote ".") host))
	  (equal (downcase host) (if (string-match (regexp-quote ".")
						   (system-name))
				     (substring (system-name) 0
						(match-beginning 0))
				   (system-name)))))))

(defun url-file-asynch-callback (x y name buff func args &optional efs)
  (if (not (featurep 'ange-ftp))
      ;; EFS passes us an extra argument
      (setq name buff
	    buff func
	    func args
	    args efs))
  (let ((size (nth 7 (file-attributes name))))
    (save-excursion
      (set-buffer buff)
      (goto-char (point-max))
      (if (/= -1 size)
	  (insert (format "Content-length: %d\n" size)))
      (insert "\n")
      (insert-file-contents-literally name)
      (if (not (url-file-host-is-local-p (url-host url-current-object)))
	  (condition-case ()
	      (delete-file name)
	    (error nil)))
      (apply func args))))

(defun url-file-build-filename (url)
  (if (not (vectorp url))
      (setq url (url-generic-parse-url url)))
  (let* ((user (url-user url))
	 (pass (url-password url))
	 (port (url-port url))
	 (host (url-host url))
	 (site (if (and port (/= port 21))
		   (if (featurep 'ange-ftp)
		       (format "%s %d" host port)
		     ;; This works in Emacs 21's ange-ftp too.
		     (format "%s#%d" host port))
		 host))
	 (file (url-unhex-string (url-filename url)))
	 (filename (if (or user (not (url-file-host-is-local-p host)))
		       (concat "/" (or user "anonymous") "@" site ":" file)
		     (if (and (memq system-type
				    '(emx ms-dos windows-nt ms-windows))
			      (string-match "^/[a-zA-Z]:/" file))
			 (substring file 1)
		       file)))
	 pos-index)

    (and user pass
	 (cond
	  ((featurep 'ange-ftp)
	   (ange-ftp-set-passwd host user pass))
	  ((or (featurep 'efs) (featurep 'efs-auto))
	   (efs-set-passwd host user pass))
	  (t
	   nil)))

    ;; This makes sure that directories have a trailing directory
    ;; separator on them so URL expansion works right.
    ;;
    ;; FIXME?  What happens if the remote system doesn't use our local
    ;; directory-sep-char as its separator?  Would it be safer to just
    ;; use '/' unconditionally and rely on the FTP server to
    ;; straighten it out for us?
    (if (and (file-directory-p filename)
	     (not (string-match (format "%c$" directory-sep-char) filename)))
	(url-set-filename url
			  (format "%s%c" filename directory-sep-char)))

    ;; If it is a directory, look for an index file first.
    (if (and (file-directory-p filename)
	     url-directory-index-file
	     (setq pos-index (expand-file-name url-directory-index-file filename))
	     (file-exists-p pos-index)
	     (file-readable-p pos-index))
	(setq filename pos-index))

    ;; Find the (possibly compressed) file
    (setq filename (url-file-find-possibly-compressed-file filename))
    filename))

;;;###autoload
(defun url-file (url callback cbargs)
  "Handle file: and ftp: URLs."
  (let* ((buffer nil)
	 (uncompressed-filename nil)
	 (content-type nil)
	 (content-encoding nil)
	 (coding-system-for-read 'binary))

    (setq filename (url-file-build-filename url))

    (if (not filename)
	(error "File does not exist: %s" (url-recreate-url url)))

    ;; Need to figure out the content-type from the real extension,
    ;; not the compressed one.
    (setq uncompressed-filename (if (string-match "\\.\\(gz\\|Z\\|z\\)$" filename)
				    (substring filename 0 (match-beginning 0))
				  filename))
    (setq content-type (mailcap-extension-to-mime
			(url-file-extension uncompressed-filename))
	  content-encoding (case (intern (url-file-extension filename))
			     ((\.z \.gz) "gzip")
			     (\.Z "compress")
			     (\.uue "x-uuencoded")
			     (\.hqx "x-hqx")
			     (\.bz2 "x-bzip2")
			     (otherwise nil)))

    (if (file-directory-p filename)
	;; A directory is done the same whether we are local or remote
	(url-find-file-dired filename)
      (save-excursion
	(setq buffer (generate-new-buffer " *url-file*"))
	(set-buffer buffer)
	(mm-disable-multibyte)
	(setq url-current-object url)
	(insert "Content-type: " (or content-type "application/octet-stream") "\n")
	(if content-encoding
	    (insert "Content-transfer-encoding: " content-encoding "\n"))
	(if (url-file-host-is-local-p (url-host url))
	      ;; Local files are handled slightly oddly
	    (if (featurep 'ange-ftp)
		(url-file-asynch-callback nil nil
					  filename
					  (current-buffer)
					  callback cbargs)
	      (url-file-asynch-callback nil nil nil
					filename
					(current-buffer)
					callback cbargs))
	  ;; FTP handling
	  (let* ((extension (url-file-extension filename))
		 (new (url-generate-unique-filename
		       (and (> (length extension) 0)
			    (concat "%s." extension)))))
	    (if (featurep 'ange-ftp)
		(ange-ftp-copy-file-internal filename (expand-file-name new) t
					     nil t
					     (list 'url-file-asynch-callback
						   new (current-buffer)
						   callback cbargs)
					     t)
	      (autoload 'efs-copy-file-internal "efs")
	      (efs-copy-file-internal filename (efs-ftp-path filename)
				      new (efs-ftp-path new)
				      t nil 0
				      (list 'url-file-asynch-callback
					    new (current-buffer)
					    callback cbargs)
				      0 nil))))))
    buffer))

(defmacro url-file-create-wrapper (method args)
  (` (defalias (quote (, (intern (format "url-ftp-%s" method))))
       (defun (, (intern (format "url-file-%s" method))) (, args)
	 (, (format "FTP/FILE URL wrapper around `%s' call." method))
	 (setq url (url-file-build-filename url))
	 (and url ((, method) (,@ (remove '&rest (remove '&optional args)))))))))

(url-file-create-wrapper file-exists-p (url))
(url-file-create-wrapper file-attributes (url))
(url-file-create-wrapper file-symlink-p (url))
(url-file-create-wrapper file-readable-p (url))
(url-file-create-wrapper file-writable-p (url))
(url-file-create-wrapper file-executable-p (url))
(if (featurep 'xemacs)
    (progn
      (url-file-create-wrapper directory-files (url &optional full match nosort files-only))
      (url-file-create-wrapper file-truename (url &optional default)))
  (url-file-create-wrapper directory-files (url &optional full match nosort))
  (url-file-create-wrapper file-truename (url &optional counter prev-dirs)))

(provide 'url-file)
