;;; url-cookie.el --- URL cookie support  -*- lexical-binding:t -*-

;; Copyright (C) 1996-1999, 2004-2018 Free Software Foundation, Inc.

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

;;; Code:

(require 'url-util)
(require 'url-parse)
(require 'url-domsuf)

(eval-when-compile (require 'cl-lib))

(defgroup url-cookie nil
  "URL cookies."
  :prefix "url-"
  :prefix "url-cookie-"
  :group 'url)

;; A cookie is stored internally as a vector of 7 slots
;; [ url-cookie NAME VALUE EXPIRES LOCALPART DOMAIN SECURE ]

(cl-defstruct (url-cookie
            (:constructor url-cookie-create)
            (:copier nil)
            (:type vector)
            :named)
  name value expires localpart domain secure)

(defvar url-cookie-storage nil         "Where cookies are stored.")
(defvar url-cookie-secure-storage nil  "Where secure cookies are stored.")
(defcustom url-cookie-file nil
  "File where cookies are stored on disk."
  :type '(choice (const :tag "Default" :value nil) file)
  :group 'url-file
  :group 'url-cookie)

(defcustom url-cookie-confirmation nil
  "If non-nil, confirmation by the user is required to accept HTTP cookies."
  :type 'boolean
  :group 'url-cookie)

(defcustom url-cookie-multiple-line nil
  "If nil, HTTP requests put all cookies for the server on one line.
Some web servers, such as http://www.hotmail.com/, only accept cookies
when they are on one line.  This is broken behavior, but just try
telling Microsoft that."
  :type 'boolean
  :group 'url-cookie)

(defvar url-cookies-changed-since-last-save nil
  "Whether the cookies list has changed since the last save operation.")

(defun url-cookie-parse-file (&optional fname)
  "Load FNAME, default `url-cookie-file'."
  ;; It's completely normal for the cookies file not to exist yet.
  (load (or fname url-cookie-file) t t))

(defun url-cookie-parse-file-netscape (filename &optional long-session)
  "Load cookies from FILENAME in Netscape/Mozilla format.
When LONG-SESSION is non-nil, session cookies (expiring at t=0
i.e. 1970-1-1) are loaded as expiring one year from now instead."
  (interactive "fLoad Netscape/Mozilla cookie file: ")
  (let ((n 0))
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (goto-char (point-min))
      (when (not (looking-at-p "# Netscape HTTP Cookie File\n"))
	(error (format "File %s doesn't look like a netscape cookie file" filename)))
      (while (not (eobp))
	(when (not (looking-at-p (rx bol (* space) "#")))
	  (let* ((line (buffer-substring (point) (save-excursion (end-of-line) (point))))
		 (fields (split-string line "\t")))
	    (cond
	     ;;((>= 1 (length line) 0)
	     ;; (message "skipping empty line"))
	     ((= (length fields) 7)
	      (let ((dom (nth 0 fields))
		    ;; (match (nth 1 fields))
		    (path (nth 2 fields))
		    (secure (string= (nth 3 fields) "TRUE"))
		    ;; session cookies (expire time = 0) are supposed
		    ;; to be removed when the browser is closed, but
		    ;; the main point of loading external cookie is to
		    ;; reuse a browser session, so to prevent the
		    ;; cookie from being detected as expired straight
		    ;; away, make it expire a year from now
		    (expires (format-time-string
			      "%d %b %Y %T [GMT]"
			      (seconds-to-time
			       (let ((s (string-to-number (nth 4 fields))))
				 (if (and (= s 0) long-session)
				     (seconds-to-time (+ (* 365 24 60 60) (float-time)))
				   s)))))
		    (key (nth 5 fields))
		    (val (nth 6 fields)))
		(cl-incf n)
		;;(message "adding <%s>=<%s> exp=<%s> dom=<%s> path=<%s> sec=%S" key val expires dom path secure)
		(url-cookie-store key val expires dom path secure)
		))
	     (t
	      (message "ignoring malformed cookie line <%s>" line)))))
	(forward-line))
      (when (< 0 n)
	(setq url-cookies-changed-since-last-save t))
      (message "added %d cookies from file %s" n filename))))

(defun url-cookie-clean-up (&optional secure)
  (let ((var (if secure 'url-cookie-secure-storage 'url-cookie-storage))
	new new-cookies)
    (dolist (cur (symbol-value var))
      (setq new-cookies nil)
      (dolist (cur-cookie (cdr cur))
	(or (not (url-cookie-p cur-cookie))
	    (url-cookie-expired-p cur-cookie)
	    (null (url-cookie-expires cur-cookie))
	    (setq new-cookies (cons cur-cookie new-cookies))))
      (when new-cookies
	(setcdr cur new-cookies)
	(setq new (cons cur new))))
    (set var new)))

(defun url-cookie-write-file (&optional fname)
  (when url-cookies-changed-since-last-save
    (or fname (setq fname (expand-file-name url-cookie-file)))
    (if (condition-case nil
            (progn
              (url-make-private-file fname)
              nil)
          (error t))
        (message "Error accessing cookie file `%s'" fname)
    (url-cookie-clean-up)
    (url-cookie-clean-up t)
    (with-temp-buffer
      (insert ";; Emacs-W3 HTTP cookies file\n"
	      ";; Automatically generated file!!! DO NOT EDIT!!!\n\n"
	      "(setq url-cookie-storage\n '")
      (let ((print-length nil) (print-level nil))
	(pp url-cookie-storage (current-buffer))
	(insert ")\n(setq url-cookie-secure-storage\n '")
	(pp url-cookie-secure-storage (current-buffer)))
      (insert ")\n")
      (insert "\n;; Local Variables:\n"
              ";; version-control: never\n"
              ";; no-byte-compile: t\n"
              ";; End:\n")
      (set (make-local-variable 'version-control) 'never)
      (write-file fname))
    (setq url-cookies-changed-since-last-save nil))))

(defun url-cookie-store (name value &optional expires domain localpart secure)
  "Store a cookie."
  (when (> (length name) 0)
    (let ((storage (if secure url-cookie-secure-storage url-cookie-storage))
          tmp found-domain)
      ;; First, look for a matching domain.
      (if (setq found-domain (assoc domain storage))
          ;; Need to either stick the new cookie in existing domain storage
          ;; or possibly replace an existing cookie if the names match.
          (unless (dolist (cur (setq storage (cdr found-domain)) tmp)
                    (and (equal localpart (url-cookie-localpart cur))
                         (equal name (url-cookie-name cur))
                         (progn
                           (setf (url-cookie-expires cur) expires)
                           (setf (url-cookie-value cur) value)
                           (setq tmp t))))
            ;; New cookie.
            (setcdr found-domain (cons
                                  (url-cookie-create :name name
                                                     :value value
                                                     :expires expires
                                                     :domain domain
                                                     :localpart localpart
                                                     :secure secure)
                                  (cdr found-domain))))
        ;; Need to add a new top-level domain.
        (setq tmp (url-cookie-create :name name
                                     :value value
                                     :expires expires
                                     :domain domain
                                     :localpart localpart
                                     :secure secure))
        (cond (storage
               (setcdr storage (cons (list domain tmp) (cdr storage))))
              (secure
               (setq url-cookie-secure-storage (list (list domain tmp))))
              (t
               (setq url-cookie-storage (list (list domain tmp)))))))))

(defun url-cookie-expired-p (cookie)
  "Return non-nil if COOKIE is expired."
  (let ((exp (url-cookie-expires cookie)))
    (and (> (length exp) 0)
	 (condition-case ()
	     (time-less-p (date-to-time exp) nil)
	   (error nil)))))

(defun url-cookie-retrieve (host &optional localpart secure)
  "Retrieve all cookies for a specified HOST and LOCALPART."
  (let ((storage (if secure
		     (append url-cookie-secure-storage url-cookie-storage)
		   url-cookie-storage))
	(case-fold-search t)
	cookies retval localpart-match)
    (dolist (cur storage)
      (setq cookies (cdr cur))
      (if (and (car cur)
	       (string-match
                (concat "^.*"
                        (regexp-quote
                         ;; Remove the dot from wildcard domains
                         ;; before matching.
			 (if (eq ?. (aref (car cur) 0))
                             (substring (car cur) 1)
                           (car cur)))
                        "$") host))
	  ;; The domains match - a possible hit!
	  (dolist (cur cookies)
	    (and (if (and (stringp
			   (setq localpart-match (url-cookie-localpart cur)))
			  (stringp localpart))
		     (string-match (concat "^" (regexp-quote localpart-match))
				   localpart)
		   (equal localpart localpart-match))
		 (not (url-cookie-expired-p cur))
		 (setq retval (cons cur retval))))))
    retval))

(defun url-cookie-generate-header-lines (host localpart secure)
  (let ((cookies (url-cookie-retrieve host localpart secure))
	retval chunk)
    ;; Have to sort this for sending most specific cookies first.
    (setq cookies (and cookies
		       (sort cookies
			     (lambda (x y)
			       (> (length (url-cookie-localpart x))
				  (length (url-cookie-localpart y)))))))
    (dolist (cur cookies)
      (setq chunk (format "%s=%s" (url-cookie-name cur) (url-cookie-value cur))
	    retval (if (and url-cookie-multiple-line
			    (< 80 (+ (length retval) (length chunk) 4)))
		       (concat retval "\r\nCookie: " chunk)
		     (if retval
			 (concat retval "; " chunk)
		       (concat "Cookie: " chunk)))))
    (if retval
	(concat retval "\r\n")
      "")))

(defcustom url-cookie-trusted-urls nil
  "A list of regular expressions matching URLs to always accept cookies from."
  :type '(repeat regexp)
  :group 'url-cookie)

(defcustom url-cookie-untrusted-urls nil
  "A list of regular expressions matching URLs to never accept cookies from."
  :type '(repeat regexp)
  :group 'url-cookie)

(defun url-cookie-host-can-set-p (host domain)
  (cond
   ((string= host domain)	; Apparently netscape lets you do this
    t)
   ((zerop (length domain))
    nil)
   (t
    ;; Remove the dot from wildcard domains before matching.
    (when (eq ?. (aref domain 0))
      (setq domain (substring domain 1)))
    (and (url-domsuf-cookie-allowed-p domain)
         (string-suffix-p domain host 'ignore-case)))))

(defun url-cookie-handle-set-cookie (str)
  (setq url-cookies-changed-since-last-save t)
  (let* ((args (nreverse (url-parse-args str t)))
	 (case-fold-search t)
	 (secure (and (assoc-string "secure" args t) t))
	 (domain (or (cdr-safe (assoc-string "domain" args t))
		     (url-host url-current-object)))
	 (current-url (url-view-url t))
	 (trusted url-cookie-trusted-urls)
	 (untrusted url-cookie-untrusted-urls)
	 (max-age (cdr-safe (assoc-string "max-age" args t)))
	 (localpart (or (cdr-safe (assoc-string "path" args t))
			(file-name-directory
			 (url-filename url-current-object))))
	 (expires nil))
    (if (and max-age (string-match "\\`-?[0-9]+\\'" max-age))
	(setq expires (format-time-string "%a %b %d %H:%M:%S %Y GMT"
					  (time-add nil (read max-age))
					  t))
      (setq expires (cdr-safe (assoc-string "expires" args t))))
    (while (consp trusted)
      (if (string-match (car trusted) current-url)
	  (setq trusted (- (match-end 0) (match-beginning 0)))
	(pop trusted)))
    (while (consp untrusted)
      (if (string-match (car untrusted) current-url)
	  (setq untrusted (- (match-end 0) (match-beginning 0)))
	(pop untrusted)))
    (and trusted untrusted
	 ;; Choose the more specific match.
	 (set (if (> trusted untrusted) 'untrusted 'trusted) nil))
    (cond
     (untrusted
      ;; The site was explicitly marked as untrusted by the user.
      nil)
     ((or (eq url-privacy-level 'paranoid)
	  (and (listp url-privacy-level) (memq 'cookies url-privacy-level)))
      ;; User never wants cookies.
      nil)
     ((and url-cookie-confirmation
	   (not trusted)
	   (save-window-excursion
	     (with-output-to-temp-buffer "*Cookie Warning*"
	       (princ (format "%s=\"%s\"\n" (caar args) (cdar args)))
	       (dolist (x (cdr args))
		 (princ (format "  %s=\"%s\"\n" (car x) (cdr x)))))
	     (prog1
		 (not (funcall url-confirmation-func
			       (format "Allow %s to set these cookies? "
				       (url-host url-current-object))))
	       (if (get-buffer "*Cookie Warning*")
		   (kill-buffer "*Cookie Warning*")))))
      ;; User wants to be asked, and declined.
      nil)
     ((url-cookie-host-can-set-p (url-host url-current-object) domain)
      ;; Cookie is accepted by the user, and passes our security checks.
      (url-cookie-store (caar args) (cdar args)
			expires domain localpart secure))
     (t
      (url-lazy-message "%s tried to set a cookie for domain %s - rejected."
			(url-host url-current-object) domain)))))

(defvar url-cookie-timer nil)

(defcustom url-cookie-save-interval 3600
  "The number of seconds between automatic saves of cookies.
Default is 1 hour.  Note that if you change this variable outside of
the `customize' interface after `url-do-setup' has been run, you need
to run the `url-cookie-setup-save-timer' function manually."
  :set #'(lambda (var val)
	   (set-default var val)
	   (if (bound-and-true-p url-setup-done)
	       (url-cookie-setup-save-timer)))
  :type 'integer
  :group 'url-cookie)

(defun url-cookie-setup-save-timer ()
  "Reset the cookie saver timer."
  (interactive)
  (ignore-errors (cancel-timer url-cookie-timer))
  (setq url-cookie-timer nil)
  (if url-cookie-save-interval
      (setq url-cookie-timer (run-at-time url-cookie-save-interval
					  url-cookie-save-interval
					  #'url-cookie-write-file))))

(defun url-cookie-delete-cookies (&optional regexp keep)
  "Delete all cookies from the cookie store where the domain matches REGEXP.
If REGEXP is nil, all cookies are deleted.  If KEEP is non-nil,
instead delete all cookies that do not match REGEXP."
  (dolist (variable '(url-cookie-secure-storage url-cookie-storage))
    (let ((cookies (symbol-value variable)))
      (dolist (elem cookies)
        (when (or (and (null keep)
                       (or (null regexp)
                           (string-match regexp (car elem))))
                  (and keep
                       regexp
                       (not (string-match regexp (car elem)))))
          (setq cookies (delq elem cookies))))
      (set variable cookies)))
  (setq url-cookies-changed-since-last-save t)
  (url-cookie-write-file))

;;; Mode for listing and editing cookies.

(defun url-cookie-list ()
  "Display a buffer listing the current URL cookies, if there are any.
Use \\<url-cookie-mode-map>\\[url-cookie-delete] to remove cookies."
  (interactive)
  (unless (or url-cookie-secure-storage
              url-cookie-storage)
    (error "No cookies are defined"))

  (pop-to-buffer "*url cookies*")
  (let ((inhibit-read-only t)
	(domains (sort
		  (copy-sequence
		   (append url-cookie-secure-storage
			   url-cookie-storage))
		  (lambda (e1 e2)
		    (string< (car e1) (car e2)))))
	(domain-length 0)
	start name format domain)
    (erase-buffer)
    (url-cookie-mode)
    (dolist (elem domains)
      (setq domain-length (max domain-length (length (car elem)))))
    (setq format (format "%%-%ds %%-20s %%s" domain-length)
	  header-line-format
	  (concat " " (format format "Domain" "Name" "Value")))
    (dolist (elem domains)
      (setq domain (car elem))
      (dolist (cookie (sort (copy-sequence (cdr elem))
			    (lambda (c1 c2)
			      (string< (url-cookie-name c1)
				       (url-cookie-name c2)))))
	(setq start (point)
	      name (url-cookie-name cookie))
	(when (> (length name) 20)
	  (setq name (substring name 0 20)))
	(insert (format format domain name
			(url-cookie-value cookie))
		"\n")
	(setq domain "")
	(put-text-property start (1+ start) 'url-cookie cookie)))
    (goto-char (point-min))))

(defun url-cookie-delete ()
  "Delete the cookie on the current line."
  (interactive)
  (let ((cookie (get-text-property (line-beginning-position) 'url-cookie))
	(inhibit-read-only t)
	variable)
    (unless cookie
      (error "No cookie on the current line"))
    (setq variable (if (url-cookie-secure cookie)
		       'url-cookie-secure-storage
		     'url-cookie-storage))
    (let* ((list (symbol-value variable))
	   (elem (assoc (url-cookie-domain cookie) list)))
      (setq elem (delq cookie elem))
      (when (zerop (length (cdr elem)))
	(setq list (delq elem list)))
      (set variable list))
    (setq url-cookies-changed-since-last-save t)
    (url-cookie-write-file)
    (delete-region (line-beginning-position)
		   (progn
		     (forward-line 1)
		     (point)))))

(defvar url-cookie-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [delete] 'url-cookie-delete)
    (define-key map [(control k)] 'url-cookie-delete)
    map))

(define-derived-mode url-cookie-mode special-mode "URL Cookie"
  "Mode for listing cookies.

\\{url-cookie-mode-map}"
  (buffer-disable-undo)
  (setq buffer-read-only t
	truncate-lines t))

(provide 'url-cookie)

;;; url-cookie.el ends here
