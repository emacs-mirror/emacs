;;; url-domsuf.el --- Say what domain names can have cookies set.  -*- lexical-binding:t -*-

;; Copyright (C) 2012-2026 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>

;; Keywords: comm, data, processes, hypermedia

;; This file is part of GNU Emacs.
;;
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

;; The rules for what domains can have cookies set is defined here:
;; https://publicsuffix.org/list/

;;; Code:

(defvar url-domsuf-domains nil)

(defun url-domsuf--public-suffix-file ()
  "Look for and return a  file name for a recent \"public_suffix_list.dat\".
Emacs ships with a copy of this file, but some systems might have
a newer version available.  Look for it in some standard
locations, and if a newer file was found, then return that."
  (car (sort
        (seq-filter
         #'file-readable-p
         (list (expand-file-name "publicsuffix.txt.gz" data-directory)
               (expand-file-name "publicsuffix.txt" data-directory)
               ;; Debian and Fedora
               "/usr/share/publicsuffix/public_suffix_list.dat"
               ;; FreeBSD port
               "/usr/local/share/public_suffix_list/public_suffix_list.dat"))
        #'file-newer-than-file-p)))

(defun url-domsuf-parse-file ()
  (with-temp-buffer
    (with-auto-compression-mode
      (insert-file-contents (url-domsuf--public-suffix-file)))
    (let ((domains nil)
	  domain exception)
      (while (not (eobp))
	(when (not (looking-at "[/\n\t ]"))
	  ;; !pref.aichi.jp means that it's allowed.
	  (if (not (eq (following-char) ?!))
	      (setq exception nil)
	    (setq exception t)
	    (forward-char 1))
	  (setq domain (buffer-substring (point) (line-end-position)))
	  (cond
	   ((string-match "\\`\\*\\." domain)
	    (setq domain (substring domain 2))
	    (push (cons domain (1+ (length (split-string domain "[.]"))))
		  domains))
	   (exception
	    (push (cons domain t) domains))
	   (t
	    (push (cons domain nil) domains))))
	(forward-line 1))
      (setq url-domsuf-domains (nreverse domains)))))

(defun url-domsuf-cookie-allowed-p (domain)
  (unless url-domsuf-domains
    (url-domsuf-parse-file))
  (let* ((allowedp t)
	 (domain-bits (split-string domain "[.]"))
	 (length (length domain-bits))
	 (upper-domain (mapconcat 'identity (cdr domain-bits) "."))
	 entry modifier)
    (dolist (elem url-domsuf-domains)
      (setq entry (car elem)
	    modifier (cdr elem))
      (cond
       ;; "com"
       ((and (null modifier)
	     (string= domain entry))
	(setq allowedp nil))
       ;; "!city.yokohama.jp"
       ((and (eq modifier t)
	     (string= domain entry))
	(setq allowedp t))
       ;; "*.bd"
       ((and (numberp modifier)
	     (= length modifier)
	     (string= entry upper-domain))
	(setq allowedp nil))))
    allowedp))

(provide 'url-domsuf)

;;; url-domsuf.el ends here
