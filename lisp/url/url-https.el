;;; url-https.el --- HTTP over SSL/TLS routines

;; Copyright (c) 1999, 2004 Free Software Foundation, Inc.

;; Keywords: comm, data, processes

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'url-gw)
(require 'url-util)
(require 'url-parse)
(require 'url-cookie)
(require 'url-http)
(require 'tls)

(defconst url-https-default-port 443 "Default HTTPS port.")
(defconst url-https-asynchronous-p t "HTTPS retrievals are asynchronous.")
(defalias 'url-https-expand-file-name 'url-http-expand-file-name)

(defmacro url-https-create-secure-wrapper (method args)
  `(defun ,(intern (format (if method "url-https-%s" "url-https") method)) ,args
    ,(format "HTTPS wrapper around `%s' call." (or method "url-http"))
    (let ((url-gateway-method (condition-case ()
				  (require 'ssl)
				(error 'tls))))
      (,(intern (format (if method "url-http-%s" "url-http") method))
       ,@(remove '&rest (remove '&optional args))))))

(url-https-create-secure-wrapper nil (url callback cbargs))
(url-https-create-secure-wrapper file-exists-p (url))
(url-https-create-secure-wrapper file-readable-p (url))
(url-https-create-secure-wrapper file-attributes (url &optional id-format))

(provide 'url-https)

;; arch-tag: c3645ac5-c248-4d12-ad41-7c4b6f7b6d19
;;; url-https.el ends here
