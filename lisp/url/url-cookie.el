;;; url-cookie.el --- Netscape Cookie support
;; Author: $Author: wmperry $
;; Created: $Date: 2002/10/29 14:44:59 $
;; Version: $Revision: 1.7 $
;; Keywords: comm, data, processes, hypermedia

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

(require 'timezone)
(require 'url-util)
(require 'url-parse)
(eval-when-compile (require 'cl))

;; See http://home.netscape.com/newsref/std/cookie_spec.html for the
;; 'open standard' defining this crap.
;;
;; A cookie is stored internally as a vector of 7 slots
;; [ 'cookie name value expires path domain secure ]

(defsubst url-cookie-name    (cookie) (aref cookie 1))
(defsubst url-cookie-value   (cookie) (aref cookie 2))
(defsubst url-cookie-expires (cookie) (aref cookie 3))
(defsubst url-cookie-path    (cookie) (aref cookie 4))
(defsubst url-cookie-domain  (cookie) (aref cookie 5))
(defsubst url-cookie-secure  (cookie) (aref cookie 6))

(defsubst url-cookie-set-name    (cookie val) (aset cookie 1 val))
(defsubst url-cookie-set-value   (cookie val) (aset cookie 2 val))
(defsubst url-cookie-set-expires (cookie val) (aset cookie 3 val))
(defsubst url-cookie-set-path    (cookie val) (aset cookie 4 val))
(defsubst url-cookie-set-domain  (cookie val) (aset cookie 5 val))
(defsubst url-cookie-set-secure  (cookie val) (aset cookie 6 val))
(defsubst url-cookie-retrieve-arg (key args) (nth 1 (memq key args)))

(defsubst url-cookie-create (&rest args)
  (let ((retval (make-vector 7 nil)))
    (aset retval 0 'cookie)
    (url-cookie-set-name retval (url-cookie-retrieve-arg :name args))
    (url-cookie-set-value retval (url-cookie-retrieve-arg :value args))
    (url-cookie-set-expires retval (url-cookie-retrieve-arg :expires args))
    (url-cookie-set-path retval (url-cookie-retrieve-arg :path args))
    (url-cookie-set-domain retval (url-cookie-retrieve-arg :domain args))
    (url-cookie-set-secure retval (url-cookie-retrieve-arg :secure args))
    retval))

(defun url-cookie-p (obj)
  (and (vectorp obj) (= (length obj) 7) (eq (aref obj 0) 'cookie)))

(defgroup url-cookie nil
  "URL cookies"
  :prefix "url-"
  :prefix "url-cookie-"
  :group 'url)

(defvar url-cookie-storage nil         "Where cookies are stored.")
(defvar url-cookie-secure-storage nil  "Where secure cookies are stored.")
(defcustom url-cookie-file nil            "*Where cookies are stored on disk."
  :type '(choice (const :tag "Default" :value nil) file)
  :group 'url-file
  :group 'url-cookie)

(defcustom url-cookie-confirmation nil
  "*If non-nil, confirmation by the user is required to accept HTTP cookies."
  :type 'boolean
  :group 'url-cookie)

(defcustom url-cookie-multiple-line nil
  "*If nil, HTTP requests put all cookies for the server on one line.
Some web servers, such as http://www.hotmail.com/, only accept cookies
when they are on one line.  This is broken behaviour, but just try
telling Microsoft that.")

(defvar url-cookies-changed-since-last-save nil
  "Whether the cookies list has changed since the last save operation.")

;;;###autoload
(defun url-cookie-parse-file (&optional fname)
  (setq fname (or fname url-cookie-file))
  (condition-case ()
      (load fname nil t)
    (error (message "Could not load cookie file %s" fname))))

(defun url-cookie-clean-up (&optional secure)
  (let* (
	 (var (if secure 'url-cookie-secure-storage 'url-cookie-storage))
	 (val (symbol-value var))
	 (cur nil)
	 (new nil)
	 (cookies nil)
	 (cur-cookie nil)
	 (new-cookies nil)
	 )
    (while val
      (setq cur (car val)
	    val (cdr val)
	    new-cookies nil
	    cookies (cdr cur))
      (while cookies
	(setq cur-cookie (car cookies)
	      cookies (cdr cookies))
	(if (or (not (url-cookie-p cur-cookie))
		(url-cookie-expired-p cur-cookie)
		(null (url-cookie-expires cur-cookie)))
	    nil
	  (setq new-cookies (cons cur-cookie new-cookies))))
      (if (not new-cookies)
	  nil
	(setcdr cur new-cookies)
	(setq new (cons cur new))))
    (set var new)))

;;;###autoload
(defun url-cookie-write-file (&optional fname)
  (setq fname (or fname url-cookie-file))
  (cond
   ((not url-cookies-changed-since-last-save) nil)
   ((not (file-writable-p fname))
    (message "Cookies file %s (see variable `url-cookie-file') is unwritable." fname))
   (t
    (url-cookie-clean-up)
    (url-cookie-clean-up t)
    (save-excursion
      (set-buffer (get-buffer-create " *cookies*"))
      (erase-buffer)
      (fundamental-mode)
      (insert ";; Emacs-W3 HTTP cookies file\n"
	      ";; Automatically generated file!!! DO NOT EDIT!!!\n\n"
	      "(setq url-cookie-storage\n '")
      (pp url-cookie-storage (current-buffer))
      (insert ")\n(setq url-cookie-secure-storage\n '")
      (pp url-cookie-secure-storage (current-buffer))
      (insert ")\n")
      (write-file fname)
      (kill-buffer (current-buffer))))))

(defun url-cookie-store (name value &optional expires domain path secure)
  "Stores a netscape-style cookie"
  (let* ((storage (if secure url-cookie-secure-storage url-cookie-storage))
	 (tmp storage)
	 (cur nil)
	 (found-domain nil))

    ;; First, look for a matching domain
    (setq found-domain (assoc domain storage))

    (if found-domain
	;; Need to either stick the new cookie in existing domain storage
	;; or possibly replace an existing cookie if the names match.
	(progn
	  (setq storage (cdr found-domain)
		tmp nil)
	  (while storage
	    (setq cur (car storage)
		  storage (cdr storage))
	    (if (and (equal path (url-cookie-path cur))
		     (equal name (url-cookie-name cur)))
		(progn
		  (url-cookie-set-expires cur expires)
		  (url-cookie-set-value cur value)
		  (setq tmp t))))
	  (if (not tmp)
	      ;; New cookie
	      (setcdr found-domain (cons
				    (url-cookie-create :name name
						       :value value
						       :expires expires
						       :domain domain
						       :path path
						       :secure secure)
				    (cdr found-domain)))))
      ;; Need to add a new top-level domain
      (setq tmp (url-cookie-create :name name
				   :value value
				   :expires expires
				   :domain domain
				   :path path
				   :secure secure))
      (cond
       (storage
	(setcdr storage (cons (list domain tmp) (cdr storage))))
       (secure
	(setq url-cookie-secure-storage (list (list domain tmp))))
       (t
	(setq url-cookie-storage (list (list domain tmp))))))))

(defun url-cookie-expired-p (cookie)
  (let* (
	 (exp (url-cookie-expires cookie))
	 (cur-date (and exp (timezone-parse-date (current-time-string))))
	 (exp-date (and exp (timezone-parse-date exp)))
	 (cur-greg (and cur-date (timezone-absolute-from-gregorian
				  (string-to-int (aref cur-date 1))
				  (string-to-int (aref cur-date 2))
				  (string-to-int (aref cur-date 0)))))
	 (exp-greg (and exp (timezone-absolute-from-gregorian
			     (string-to-int (aref exp-date 1))
			     (string-to-int (aref exp-date 2))
			     (string-to-int (aref exp-date 0)))))
	 (diff-in-days (and exp (- cur-greg exp-greg)))
	 )
    (cond
     ((not exp)	nil)			; No expiry == expires at browser quit
     ((< diff-in-days 0) nil)		; Expires sometime after today
     ((> diff-in-days 0) t)		; Expired before today
     (t					; Expires sometime today, check times
      (let* ((cur-time (timezone-parse-time (aref cur-date 3)))
	     (exp-time (timezone-parse-time (aref exp-date 3)))
	     (cur-norm (+ (* 360 (string-to-int (aref cur-time 2)))
			  (*  60 (string-to-int (aref cur-time 1)))
			  (*   1 (string-to-int (aref cur-time 0)))))
	     (exp-norm (+ (* 360 (string-to-int (aref exp-time 2)))
			  (*  60 (string-to-int (aref exp-time 1)))
			  (*   1 (string-to-int (aref exp-time 0))))))
	(> (- cur-norm exp-norm) 1))))))

;;;###autoload
(defun url-cookie-retrieve (host path &optional secure)
  "Retrieves all the netscape-style cookies for a specified HOST and PATH"
  (let ((storage (if secure
		     (append url-cookie-secure-storage url-cookie-storage)
		   url-cookie-storage))
	(case-fold-search t)
	(cookies nil)
	(cur nil)
	(retval nil)
	(path-regexp nil))
    (while storage
      (setq cur (car storage)
	    storage (cdr storage)
	    cookies (cdr cur))
      (if (and (car cur)
	       (string-match (concat "^.*" (regexp-quote (car cur)) "$") host))
	  ;; The domains match - a possible hit!
	  (while cookies
	    (setq cur (car cookies)
		  cookies (cdr cookies)
		  path-regexp (concat "^" (regexp-quote
					   (url-cookie-path cur))))
	    (if (and (string-match path-regexp path)
		     (not (url-cookie-expired-p cur)))
		(setq retval (cons cur retval))))))
    retval))

;;;###autolaod
(defun url-cookie-generate-header-lines (host path secure)
  (let* ((cookies (url-cookie-retrieve host path secure))
	(retval nil)
	(cur nil)
	(chunk nil))
    ;; Have to sort this for sending most specific cookies first
    (setq cookies (and cookies
		       (sort cookies
			     (function
			      (lambda (x y)
				(> (length (url-cookie-path x))
				   (length (url-cookie-path y))))))))
    (while cookies
      (setq cur (car cookies)
	    cookies (cdr cookies)
	    chunk (format "%s=%s" (url-cookie-name cur) (url-cookie-value cur))
	    retval (if (and url-cookie-multiple-line
			    (< 80 (+ (length retval) (length chunk) 4)))
		       (concat retval "\r\nCookie: " chunk)
		     (if retval
			 (concat retval "; " chunk)
		       (concat "Cookie: " chunk)))))
    (if retval
	(concat retval "\r\n")
      "")))

(defvar url-cookie-two-dot-domains
  (concat "\\.\\("
   (mapconcat 'identity (list "com" "edu" "net" "org" "gov" "mil" "int")
	      "\\|")
   "\\)$")
  "A regular expression of top-level domains that only require two matching
'.'s in the domain name in order to set a cookie.")

(defcustom url-cookie-trusted-urls nil
  "*A list of regular expressions matching URLs to always accept cookies from."
  :type '(repeat regexp)
  :group 'url-cookie)

(defcustom url-cookie-untrusted-urls nil
  "*A list of regular expressions matching URLs to never accept cookies from."
  :type '(repeat regexp)
  :group 'url-cookie)

(defun url-cookie-host-can-set-p (host domain)
  (let ((numdots 0)
	(tmp domain)
	(last nil)
	(case-fold-search t)
	(mindots 3))
    (while (setq last (string-match "\\." domain last))
      (setq numdots (1+ numdots)
	    last (1+ last)))
    (if (string-match url-cookie-two-dot-domains domain)
	(setq mindots 2))
    (cond
     ((string= host domain)		; Apparently netscape lets you do this
      t)
     ((>= numdots mindots)		; We have enough dots in domain name
      ;; Need to check and make sure the host is actually _in_ the
      ;; domain it wants to set a cookie for though.
      (string-match (concat (regexp-quote domain) "$") host))
     (t
      nil))))

;;;###autoload
(defun url-cookie-handle-set-cookie (str)
  (setq url-cookies-changed-since-last-save t)
  (let* ((args (url-parse-args str t))
	 (case-fold-search t)
	 (secure (and (assoc-ignore-case "secure" args) t))
	 (domain (or (cdr-safe (assoc-ignore-case "domain" args))
		     (url-host url-current-object)))
	 (current-url (url-view-url t))
	 (trusted url-cookie-trusted-urls)
	 (untrusted url-cookie-untrusted-urls)
	 (expires (cdr-safe (assoc-ignore-case "expires" args)))
	 (path (or (cdr-safe (assoc-ignore-case "path" args))
		   (file-name-directory
		    (url-filename url-current-object))))
	 (rest nil))
    (while args
      (if (not (member (downcase (car (car args)))
		       '("secure" "domain" "expires" "path")))
	  (setq rest (cons (car args) rest)))
      (setq args (cdr args)))

    ;; Sometimes we get dates that the timezone package cannot handle very
    ;; gracefully - take care of this here, instead of in url-cookie-expired-p
    ;; to speed things up.
    (if (and expires
	     (string-match
	      (concat "^[^,]+, +\\(..\\)-\\(...\\)-\\(..\\) +"
		      "\\(..:..:..\\) +\\[*\\([^\]]+\\)\\]*$")
	      expires))
	(setq expires (concat (match-string 1 expires) " "
			      (match-string 2 expires) " "
			      (match-string 3 expires) " "
			      (match-string 4 expires) " ["
			      (match-string 5 expires) "]")))

    ;; This one is for older Emacs/XEmacs variants that don't
    ;; understand this format without tenths of a second in it.
    ;; Wednesday, 30-Dec-2037 16:00:00 GMT
    ;;       - vs -
    ;; Wednesday, 30-Dec-2037 16:00:00.00 GMT
    (if (and expires
	     (string-match
	      "\\([0-9]+\\)-\\([A-Za-z]+\\)-\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9]+:[0-9]+\\)\\(\\.[0-9]+\\)*[ \t]+\\([-+a-zA-Z0-9]+\\)"
	      expires))
	(setq expires (concat (match-string 1 expires) "-"	; day
			      (match-string 2 expires) "-"	; month
			      (match-string 3 expires) " "	; year
			      (match-string 4 expires) ".00 " ; hour:minutes:seconds
			      (match-string 6 expires)))) ":" ; timezone
    
    (while (consp trusted)
      (if (string-match (car trusted) current-url)
	  (setq trusted (- (match-end 0) (match-beginning 0)))
	(pop trusted)))
    (while (consp untrusted)
      (if (string-match (car untrusted) current-url)
	  (setq untrusted (- (match-end 0) (match-beginning 0)))
	(pop untrusted)))
    (if (and trusted untrusted)
	;; Choose the more specific match
	(if (> trusted untrusted)
	    (setq untrusted nil)
	  (setq trusted nil)))
    (cond
     (untrusted
      ;; The site was explicity marked as untrusted by the user
      nil)
     ((or (eq url-privacy-level 'paranoid)
	  (and (listp url-privacy-level) (memq 'cookies url-privacy-level)))
      ;; user never wants cookies
      nil)
     ((and url-cookie-confirmation
	   (not trusted)
	   (save-window-excursion
	     (with-output-to-temp-buffer "*Cookie Warning*"
	       (mapcar
		(function
		 (lambda (x)
		   (princ (format "%s - %s" (car x) (cdr x))))) rest))
	     (prog1
		 (not (funcall url-confirmation-func
			       (format "Allow %s to set these cookies? "
				       (url-host url-current-object))))
	       (if (get-buffer "*Cookie Warning*")
		   (kill-buffer "*Cookie Warning*")))))
      ;; user wants to be asked, and declined.
      nil)
     ((url-cookie-host-can-set-p (url-host url-current-object) domain)
      ;; Cookie is accepted by the user, and passes our security checks
      (let ((cur nil))
	(while rest
	  (setq cur (pop rest))
	  (url-cookie-store (car cur) (cdr cur)
			    expires domain path secure))))
     (t
      (message "%s tried to set a cookie for domain %s - rejected."
	       (url-host url-current-object) domain)))))

(defvar url-cookie-timer nil)

(defcustom url-cookie-save-interval 3600
  "*The number of seconds between automatic saves of cookies.
Default is 1 hour.  Note that if you change this variable outside of
the `customize' interface after `url-do-setup' has been run, you need
to run the `url-cookie-setup-save-timer' function manually."
  :set (function (lambda (var val)
		   (set-default var val)
		   (and (featurep 'url)
			(fboundp 'url-cookie-setup-save-timer)
			(url-cookie-setup-save-timer))))
  :type 'integer
  :group 'url)

;;;###autoload
(defun url-cookie-setup-save-timer ()
  "Reset the cookie saver timer."
  (interactive)
  (cond
   ((featurep 'itimer)
    (ignore-errors (delete-itimer url-cookie-timer))
    (setq url-cookie-timer nil)
    (if url-cookie-save-interval
	(setq url-cookie-timer
	      (start-itimer "url-cookie-saver" 'url-cookie-write-file
			    url-cookie-save-interval
			    url-cookie-save-interval))))
   ((fboundp 'run-at-time)
    (ignore-errors (cancel-timer url-cookie-timer))
    (setq url-cookie-timer nil)
    (if url-cookie-save-interval
	(setq url-cookie-timer
	      (run-at-time url-cookie-save-interval
			   url-cookie-save-interval
			   'url-cookie-write-file))))
   (t nil)))

(provide 'url-cookie)

