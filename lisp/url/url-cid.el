;;; url-cid.el --- Content-ID URL loader
;; Author: $Author: fx $
;; Created: $Date: 2001/05/05 16:35:58 $
;; Version: $Revision: 1.3 $
;; Keywords: comm, data, processes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1998 - 1999 Free Software Foundation, Inc.
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

(require 'url-vars)
(require 'url-parse)

(require 'mm-decode)

(defun url-cid-gnus (cid)
  (let ((content-type nil)
 	(encoding nil)
 	(part nil)
 	(data nil))
    (setq part (mm-get-content-id cid))
    (if (not part)
	(message "Unknown CID encountered: %s" cid)
      (setq data (save-excursion
		   (set-buffer (mm-handle-buffer part))
		   (buffer-string))
	    content-type (mm-handle-type part)
	    encoding (symbol-name (mm-handle-encoding part)))
      (if (= 0 (length content-type)) (setq content-type "text/plain"))
      (if (= 0 (length encoding)) (setq encoding "8bit"))
      (if (listp content-type)
	  (setq content-type (car content-type)))
      (insert (format "Content-type: %d\r\n"  (length data))
	      "Content-type: " content-type "\r\n"
	      "Content-transfer-encoding: " encoding "\r\n"
	      "\r\n"
	      (or data "")))))

;;;###autoload
(defun url-cid (url)
  (cond
   ((fboundp 'mm-get-content-id)
    ;; Using Pterodactyl Gnus or later
    (save-excursion
      (set-buffer (generate-new-buffer " *url-cid*"))
      (url-cid-gnus (url-filename url))))
   (t
    (message "Unable to handle CID URL: %s" url))))
