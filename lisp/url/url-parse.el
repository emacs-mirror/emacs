;;; url-parse.el --- Uniform Resource Locator parser

;; Copyright (C) 1996, 1997, 1998, 1999, 2004,
;;   2005, 2006, 2007 Free Software Foundation, Inc.

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

(require 'url-vars)

(autoload 'url-scheme-get-property "url-methods")

(defmacro url-type (urlobj)
  `(aref ,urlobj 0))

(defmacro url-user (urlobj)
  `(aref ,urlobj 1))

(defmacro url-password (urlobj)
  `(aref ,urlobj 2))

(defmacro url-host (urlobj)
  `(aref ,urlobj 3))

(defmacro url-port (urlobj)
  `(or (aref ,urlobj 4)
      (if (url-fullness ,urlobj)
	  (url-scheme-get-property (url-type ,urlobj) 'default-port))))

(defmacro url-filename (urlobj)
  `(aref ,urlobj 5))

(defmacro url-target (urlobj)
  `(aref ,urlobj 6))

(defmacro url-attributes (urlobj)
  `(aref ,urlobj 7))

(defmacro url-fullness (urlobj)
  `(aref ,urlobj 8))

(defmacro url-set-type (urlobj type)
  `(aset ,urlobj 0 ,type))

(defmacro url-set-user (urlobj user)
  `(aset ,urlobj 1 ,user))

(defmacro url-set-password (urlobj pass)
  `(aset ,urlobj 2 ,pass))

(defmacro url-set-host (urlobj host)
  `(aset ,urlobj 3 ,host))

(defmacro url-set-port (urlobj port)
  `(aset ,urlobj 4 ,port))

(defmacro url-set-filename (urlobj file)
  `(aset ,urlobj 5 ,file))

(defmacro url-set-target (urlobj targ)
  `(aset ,urlobj 6 ,targ))

(defmacro url-set-attributes (urlobj targ)
  `(aset ,urlobj 7 ,targ))

(defmacro url-set-full (urlobj val)
  `(aset ,urlobj 8 ,val))

;;;###autoload
(defun url-recreate-url (urlobj)
  "Recreate a URL string from the parsed URLOBJ."
  (concat (url-type urlobj) ":" (if (url-host urlobj) "//" "")
	  (if (url-user urlobj)
	      (concat (url-user urlobj)
		      (if (url-password urlobj)
			  (concat ":" (url-password urlobj)))
		      "@"))
	  (url-host urlobj)
	  (if (and (url-port urlobj)
		   (not (equal (url-port urlobj)
			       (url-scheme-get-property (url-type urlobj) 'default-port))))
	      (format ":%d" (url-port urlobj)))
	  (or (url-filename urlobj) "/")          
	  (url-recreate-url-attributes urlobj)
	  (if (url-target urlobj)
	      (concat "#" (url-target urlobj)))))

(defun url-recreate-url-attributes (urlobj)
  "Recreate the attributes of an URL string from the parsed URLOBJ."
  (when (url-attributes urlobj)
    (concat ";"
	    (mapconcat (lambda (x)
                         (if (cdr x)
                             (concat (car x) "=" (cdr x))
                           (car x)))
                       (url-attributes urlobj) ";"))))

;;;###autoload
(defun url-generic-parse-url (url)
  "Return a vector of the parts of URL.
Format is:
\[TYPE USER PASSWORD HOST PORT FILE TARGET ATTRIBUTES FULL\]"
  ;; See RFC 3986.
  (cond
   ((null url)
    (make-vector 9 nil))
   ((or (not (string-match url-nonrelative-link url))
	(= ?/ (string-to-char url)))
    ;; This isn't correct, as a relative URL can be a fragment link
    ;; (e.g. "#foo") and many other things (see section 4.2).
    ;; However, let's not fix something that isn't broken, especially
    ;; when close to a release.
    (let ((retval (make-vector 9 nil)))
      (url-set-filename retval url)
      (url-set-full retval nil)
      retval))
   (t
    (with-temp-buffer
      (set-syntax-table url-parse-syntax-table)
      (let ((save-pos nil)
	    (prot nil)
	    (user nil)
	    (pass nil)
	    (host nil)
	    (port nil)
	    (file nil)
	    (refs nil)
	    (attr nil)
	    (full nil)
	    (inhibit-read-only t))
	(erase-buffer)
	(insert url)
	(goto-char (point-min))
	(setq save-pos (point))

	;; 3.1. Scheme
	(if (not (looking-at "//"))
	    (progn
	      (skip-chars-forward "a-zA-Z+.\\-")
	      (downcase-region save-pos (point))
	      (setq prot (buffer-substring save-pos (point)))
	      (skip-chars-forward ":")
	      (setq save-pos (point))))

	;; 3.2. Authority
	(if (looking-at "//")
	    (progn
	      (setq full t)
	      (forward-char 2)
	      (setq save-pos (point))
	      (skip-chars-forward "^/")
	      (setq host (buffer-substring save-pos (point)))
	      (if (string-match "^\\([^@]+\\)@" host)
		  (setq user (match-string 1 host)
			host (substring host (match-end 0) nil)))
	      (if (and user (string-match "\\([^:]+\\):\\(.*\\)" user))
		  (setq pass (match-string 2 user)
			user (match-string 1 user)))
	      ;; This gives wrong results for IPv6 literal addresses.
	      (if (string-match ":\\([0-9+]+\\)" host)
		  (setq port (string-to-number (match-string 1 host))
			host (substring host 0 (match-beginning 0))))
	      (if (string-match ":$" host)
		  (setq host (substring host 0 (match-beginning 0))))
	      (setq host (downcase host)
		    save-pos (point))))

	(if (not port)
	    (setq port (url-scheme-get-property prot 'default-port)))

	;; 3.3. Path
	;; Gross hack to preserve ';' in data URLs
	(setq save-pos (point))

	;; 3.4. Query
	(if (string= "data" prot)
	    (goto-char (point-max))
	  ;; Now check for references
	  (skip-chars-forward "^#")
	  (if (eobp)
	      nil
	    (delete-region
	     (point)
	     (progn
	       (skip-chars-forward "#")
	       (setq refs (buffer-substring (point) (point-max)))
	       (point-max))))
	  (goto-char save-pos)
	  (skip-chars-forward "^;")
	  (if (not (eobp))
	      (setq attr (url-parse-args (buffer-substring (point) (point-max)) t)
		    attr (nreverse attr))))

	(setq file (buffer-substring save-pos (point)))
	(if (and host (string-match "%[0-9][0-9]" host))
	    (setq host (url-unhex-string host)))
	(vector prot user pass host port file refs attr full))))))

(provide 'url-parse)

;; arch-tag: f338325f-71ab-4bee-93cc-78fb9a03d403
;;; url-parse.el ends here
