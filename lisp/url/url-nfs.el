;;; url-nfs.el --- NFS URL interface
;; Author: $Author: fx $
;; Created: $Date: 2001/05/22 16:10:50 $
;; Version: $Revision: 1.3 $
;; Keywords: comm, data, processes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996 by William M. Perry <wmperry@cs.indiana.edu>
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
(require 'url-parse)
(require 'url-file)

(defvar url-nfs-automounter-directory-spec
  "file:/net/%h%f"
  "*How to invoke the NFS automounter.  Certain % sequences are recognized.

%h -- the hostname of the NFS server
%n -- the port # of the NFS server
%u -- the username to use to authenticate
%p -- the password to use to authenticate
%f -- the filename on the remote server
%% -- a literal %

Each can be used any number of times.")

(defun url-nfs-unescape (format host port user pass file)
  (save-excursion
    (set-buffer (get-buffer-create " *nfs-parse*"))
    (erase-buffer)
    (insert format)
    (goto-char (point-min))
    (while (re-search-forward "%\\(.\\)" nil t)
       (let ((escape (aref (match-string 1) 0)))
	 (replace-match "" t t)
	 (case escape
	   (?% (insert "%"))
	   (?h (insert host))
	   (?n (insert (or port "")))
	   (?u (insert (or user "")))
	   (?p (insert (or pass "")))
	   (?f (insert (or file "/"))))))
    (buffer-string)))

(defun url-nfs-build-filename (url)
  (let* ((host (url-host url))
	 (port (string-to-int (url-port url)))
	 (pass (url-password url))
	 (user (url-user url))
	 (file (url-filename url)))
    (url-generic-parse-url
     (url-nfs-unescape url-nfs-automounter-directory-spec
		       host port user pass file))))

(defun url-nfs (url callback cbargs)
  (url-file (url-nfs-build-filename url) callback cbargs))

(defmacro url-nfs-create-wrapper (method args)
  (` (defun (, (intern (format "url-nfs-%s" method))) (, args)
       (, (format "NFS URL wrapper around `%s' call." method))
       (setq url (url-nfs-build-filename url))
       (and url ((, (intern (format "url-file-%s" method)))
		 (,@ (remove '&rest (remove '&optional args))))))))

(url-nfs-create-wrapper file-exists-p (url))
(url-nfs-create-wrapper file-attributes (url))
(url-nfs-create-wrapper file-symlink-p (url))
(url-nfs-create-wrapper file-readable-p (url))
(url-nfs-create-wrapper file-writable-p (url))
(url-nfs-create-wrapper file-executable-p (url))
(if (featurep 'xemacs)
    (progn
      (url-nfs-create-wrapper directory-files (url &optional full match nosort files-only))
      (url-nfs-create-wrapper file-truename (url &optional default)))
  (url-nfs-create-wrapper directory-files (url &optional full match nosort))
  (url-nfs-create-wrapper file-truename (url &optional counter prev-dirs)))

(provide 'url-nfs)
