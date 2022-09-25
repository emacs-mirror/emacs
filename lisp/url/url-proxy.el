;;; url-proxy.el --- Proxy server support  -*- lexical-binding: t; -*-

;; Copyright (C) 1999, 2004-2022 Free Software Foundation, Inc.

;; Keywords: comm, data, processes, hypermedia

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

;;; Code:

(require 'url-parse)

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
    (setq proxy (if (listp proxies) (car proxies) proxies))
    (cond
     ((string-match "^DIRECT" proxy) nil)
     ((string-match "^PROXY +" proxy)
      (concat "http://" (substring proxy (match-end 0)) "/"))
     ((string-match "^SOCKS +" proxy)
      (concat "socks://" (substring proxy (match-end 0))))
     (t
      (display-warning 'url (format "Unknown proxy directive: %s" proxy) :error)
      nil))))

(autoload 'url-http "url-http")

(defun url-proxy (url callback &optional cbargs)
  ;; Retrieve URL from a proxy.
  ;; Expects `url-using-proxy' to be bound to the specific proxy to use."
  (setq url-using-proxy (url-generic-parse-url url-using-proxy))

  (cond
   ((string= (url-type url-using-proxy) "http")
    (url-http url callback cbargs))
   (t
    (error "Don't know how to use proxy `%s'" url-using-proxy))))

(provide 'url-proxy)

;;; url-proxy.el ends here
