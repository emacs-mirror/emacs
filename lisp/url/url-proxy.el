;;; url-proxy.el --- Proxy server support
;; Author: $Author: fx $
;; Created: $Date: 2001/10/11 21:09:35 $
;; Version: $Revision: 1.5 $
;; Keywords: comm, data, processes, hypermedia

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

(require 'url-parse)
(autoload 'url-warn "url")

(defun url-default-find-proxy-for-url (urlobj host)
  (cond
   ((or (and (assoc "no_proxy" url-proxy-services)
	     (string-match
	      (cdr
	       (assoc "no_proxy" url-proxy-services))
	      host))
	(equal "www" (url-type urlobj)))
    "DIRECT")
   ((cdr (assoc (url-type urlobj) url-proxy-services))
    (concat "PROXY " (cdr (assoc (url-type urlobj) url-proxy-services))))
   ;;
   ;; Should check for socks
   ;;
   (t
    "DIRECT")))

(defvar url-proxy-locator 'url-default-find-proxy-for-url)

(defun url-find-proxy-for-url (url host)
  (let ((proxies (split-string (funcall url-proxy-locator url host) " *; *"))
	(proxy nil)
	(case-fold-search t))
    ;; Not sure how I should handle gracefully degrading from one proxy to
    ;; another, so for now just deal with the first one
    ;; (while proxies
    (if (listp proxies)
	(setq proxy (car proxies))
      (setq proxy proxies))
    (cond
     ((string-match "^direct" proxy) nil)
     ((string-match "^proxy +" proxy)
      (concat "http://" (substring proxy (match-end 0)) "/"))
     ((string-match "^socks +" proxy)
      (concat "socks://" (substring proxy (match-end 0))))
     (t
      (url-warn 'url (format "Unknown proxy directive: %s" proxy) 'critical)
      nil))))

(defun url-proxy (url callback &optional cbargs)
  ;; Retrieve URL from a proxy.
  ;; Expects `url-using-proxy' to be bound to the specific proxy to use."
  (setq url-using-proxy (url-generic-parse-url url-using-proxy))
  (let ((proxy-object (copy-sequence url)))
    (url-set-target proxy-object nil)
    (url-http url-using-proxy callback cbargs)))

(provide 'url-proxy)
