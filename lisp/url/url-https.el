;;; url-https.el --- HTTP over SSL routines
;; Author: $Author: wmperry $
;; Created: $Date: 2001/11/22 14:32:13 $
;; Version: $Revision: 1.3 $
;; Keywords: comm, data, processes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1999 Free Software Foundation, Inc.
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

(require 'url-gw)
(require 'url-util)
(require 'url-parse)
(require 'url-cookie)
(require 'url-http)

(defconst url-https-default-port 443 "Default HTTPS port.")
(defconst url-https-asynchronous-p t "HTTPS retrievals are asynchronous.")
(defalias 'url-https-expand-file-name 'url-http-expand-file-name)

(defmacro url-https-create-secure-wrapper (method args)
  (` (defun (, (intern (format (if method "url-https-%s" "url-https") method))) (, args)
       (, (format "HTTPS wrapper around `%s' call." (or method "url-http")))
       (condition-case ()
	   (require 'ssl)
	 (error
	  (error "HTTPS support could not find `ssl' library.")))
       (let ((url-gateway-method 'ssl))
	 ((, (intern (format (if method "url-http-%s" "url-http") method))) (,@ (remove '&rest (remove '&optional args))))))))

(url-https-create-secure-wrapper nil (url callback cbargs))
(url-https-create-secure-wrapper file-exists-p (url))
(url-https-create-secure-wrapper file-readable-p (url))
(url-https-create-secure-wrapper file-attributes (url))

(provide 'url-https)
