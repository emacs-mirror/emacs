;;; thingatpt.el --- get the `thing' at point  -*- lexical-binding:t -*-

;; Copyright (C) 1991-1998, 2000-2022 Free Software Foundation, Inc.

;; Author: Mike Williams <mikew@gopher.dosli.govt.nz>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: extensions, matching, mouse
;; Created: Thu Mar 28 13:48:23 1991

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

;; This file provides routines for getting the "thing" at the location of
;; point, whatever that "thing" happens to be.  The "thing" is defined by
;; its beginning and end positions in the buffer.
;;
;; The function bounds-of-thing-at-point finds the beginning and end
;; positions by moving first forward to the end of the "thing", and then
;; backwards to the beginning.  By default, it uses the corresponding
;; forward-"thing" operator (e.g. forward-word, forward-line).
;;
;; Special cases are allowed for using properties associated with the named
;; "thing":
;;
;;   forward-op		Function to call to skip forward over a "thing" (or
;;                      with a negative argument, backward).
;;
;;   beginning-op	Function to call to skip to the beginning of a "thing".
;;   end-op		Function to call to skip to the end of a "thing".
;;
;; For simple things, defined as sequences of specific kinds of characters,
;; use macro define-thing-chars.
;;
;; Reliance on existing operators means that many `things' can be accessed
;; without further code:  eg.
;;     (thing-at-point 'line)
;;     (thing-at-point 'page)

;;; Code:

(require 'cl-lib)
(provide 'thingatpt)

(defvar thing-at-point-provider-alist nil
  "Alist of providers for returning a \"thing\" at point.
This variable can be set globally, or appended to buffer-locally
by modes, to provide functions that will return a \"thing\" at
point.  The first provider for the \"thing\" that returns a
non-nil value wins.

For instance, a major mode could say:

\(setq-local thing-at-point-provider-alist
            (append thing-at-point-provider-alist
                    \\='((url . my-mode--url-at-point))))

to provide a way to get an `url' at point in that mode.  The
provider functions are called with no parameters at the point in
question.

\"things\" include `symbol', `list', `sexp', `defun', `filename',
`existing-filename', `url', `email', `uuid', `word', `sentence',
`whitespace', `line', and `page'.")

;; Basic movement

;;;###autoload
(defun forward-thing (thing &optional n)
  "Move forward to the end of the Nth next THING.
THING should be a symbol specifying a type of syntactic entity.
Possibilities include `symbol', `list', `sexp', `defun',
`filename', `url', `email', `uuid', `word', `sentence', `whitespace',
`line', and `page'."
  (let ((forward-op (or (get thing 'forward-op)
			(intern-soft (format "forward-%s" thing)))))
    (if (functionp forward-op)
	(funcall forward-op (or n 1))
      (error "Can't determine how to move over a %s" thing))))

;; General routines

;;;###autoload
(defun bounds-of-thing-at-point (thing)
  "Determine the start and end buffer locations for the THING at point.
THING should be a symbol specifying a type of syntactic entity.
Possibilities include `symbol', `list', `sexp', `defun',
`filename', `url', `email', `uuid', `word', `sentence', `whitespace',
`line', and `page'.

See the file `thingatpt.el' for documentation on how to define a
valid THING.

Return a cons cell (START . END) giving the start and end
positions of the thing found."
  (if (get thing 'bounds-of-thing-at-point)
      (funcall (get thing 'bounds-of-thing-at-point))
    (let ((orig (point)))
      (ignore-errors
	(save-excursion
	  ;; Try moving forward, then back.
	  (funcall ;; First move to end.
	   (or (get thing 'end-op)
	       (lambda () (forward-thing thing 1))))
	  (funcall ;; Then move to beg.
	   (or (get thing 'beginning-op)
	       (lambda () (forward-thing thing -1))))
	  (let ((beg (point)))
	    (if (<= beg orig)
		;; If that brings us all the way back to ORIG,
		;; it worked.  But END may not be the real end.
		;; So find the real end that corresponds to BEG.
		;; FIXME: in which cases can `real-end' differ from `end'?
		(let ((real-end
		       (progn
			 (funcall
			  (or (get thing 'end-op)
			      (lambda () (forward-thing thing 1))))
			 (point))))
		  (when (and (<= orig real-end) (< beg real-end))
		    (cons beg real-end)))
	      (goto-char orig)
	      ;; Try a second time, moving backward first and then forward,
	      ;; so that we can find a thing that ends at ORIG.
	      (funcall ;; First, move to beg.
	       (or (get thing 'beginning-op)
		   (lambda () (forward-thing thing -1))))
	      (funcall ;; Then move to end.
	       (or (get thing 'end-op)
		   (lambda () (forward-thing thing 1))))
	      (let ((end (point))
		    (real-beg
		     (progn
		       (funcall
			(or (get thing 'beginning-op)
			    (lambda () (forward-thing thing -1))))
		       (point))))
		(if (and (<= real-beg orig) (<= orig end) (< real-beg end))
		    (cons real-beg end))))))))))

;;;###autoload
(defun thing-at-point (thing &optional no-properties)
  "Return the THING at point.
THING should be a symbol specifying a type of syntactic entity.
Possibilities include `symbol', `list', `sexp', `defun',
`filename', `existing-filename', `url', `email', `uuid', `word',
`sentence', `whitespace', `line', `number', and `page'.

When the optional argument NO-PROPERTIES is non-nil,
strip text properties from the return value.

See the file `thingatpt.el' for documentation on how to define
a symbol as a valid THING."
  (let ((text
         (cond
          ((cl-loop for (pthing . function) in thing-at-point-provider-alist
                    when (eq pthing thing)
                    for result = (funcall function)
                    when result
                    return result))
          ((get thing 'thing-at-point)
           (funcall (get thing 'thing-at-point)))
          (t
           (let ((bounds (bounds-of-thing-at-point thing)))
             (when bounds
               (buffer-substring (car bounds) (cdr bounds))))))))
    (when (and text no-properties (sequencep text))
      (set-text-properties 0 (length text) nil text))
    text))

;;;###autoload
(defun bounds-of-thing-at-mouse (event thing)
  "Determine start and end locations for THING at mouse click given by EVENT.
Like `bounds-of-thing-at-point', but tries to use the position in EVENT
where the mouse button is clicked to find the thing nearby."
  (save-excursion
    (mouse-set-point event)
    (bounds-of-thing-at-point thing)))

;;;###autoload
(defun thing-at-mouse (event thing &optional no-properties)
  "Return the THING at mouse click specified by EVENT.
Like `thing-at-point', but tries to use the position in EVENT
where the mouse button is clicked to find the thing nearby."
  (save-excursion
    (mouse-set-point event)
    (thing-at-point thing no-properties)))

;; Go to beginning/end

(defun beginning-of-thing (thing)
  "Move point to the beginning of THING.
The bounds of THING are determined by `bounds-of-thing-at-point'."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (or bounds (error "No %s here" thing))
    (goto-char (car bounds))))

(defun end-of-thing (thing)
  "Move point to the end of THING.
The bounds of THING are determined by `bounds-of-thing-at-point'."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (or bounds (error "No %s here" thing))
    (goto-char (cdr bounds))))

;;  Special cases

;;  Lines

;; bolp will be false when you click on the last line in the buffer
;; and it has no final newline.

(put 'line 'beginning-op
     (lambda () (if (bolp) (forward-line -1) (beginning-of-line))))

;;  Strings

(put 'string 'bounds-of-thing-at-point 'thing-at-point-bounds-of-string-at-point)

(defun thing-at-point-bounds-of-string-at-point ()
  "Return the bounds of the string at point.
Prefer the enclosing string with fallback on sexp at point.
\[Internal function used by `bounds-of-thing-at-point'.]"
  (save-excursion
    (let ((ppss (syntax-ppss)))
      (if (nth 3 ppss)
          ;; Inside the string
          (ignore-errors
            (goto-char (nth 8 ppss))
            (cons (point) (progn (forward-sexp) (point))))
        ;; At the beginning of the string
        (if (eq (char-syntax (char-after)) ?\")
            (let ((bound (bounds-of-thing-at-point 'sexp)))
	      (and bound
	           (<= (car bound) (point)) (< (point) (cdr bound))
	           bound)))))))

(defun in-string-p ()
  "Return non-nil if point is in a string."
  (declare (obsolete "use (nth 3 (syntax-ppss)) instead." "25.1"))
  (let ((orig (point)))
    (save-excursion
      (beginning-of-defun)
      (nth 3 (parse-partial-sexp (point) orig)))))

;;  Sexps

(defun thing-at-point--end-of-sexp ()
  "Move point to the end of the current sexp."
  (let ((char-syntax (syntax-after (point))))
    (if (or (eq char-syntax ?\))
	    (and (eq char-syntax ?\") (nth 3 (syntax-ppss))))
	(forward-char 1)
      (condition-case _
          (forward-sexp 1)
        (scan-error nil)))))

(define-obsolete-function-alias 'end-of-sexp
  'thing-at-point--end-of-sexp "25.1"
  "This is an internal thingatpt function and should not be used.")

(put 'sexp 'end-op 'thing-at-point--end-of-sexp)

(defun thing-at-point--beginning-of-sexp ()
  "Move point to the beginning of the current sexp."
  (let ((char-syntax (char-syntax (char-before))))
    (if (or (eq char-syntax ?\()
	    (and (eq char-syntax ?\") (nth 3 (syntax-ppss))))
	(forward-char -1)
      (forward-sexp -1))))

(define-obsolete-function-alias 'beginning-of-sexp
  'thing-at-point--beginning-of-sexp "25.1"
  "This is an internal thingatpt function and should not be used.")

(put 'sexp 'beginning-op 'thing-at-point--beginning-of-sexp)

;; Symbols

(put 'symbol 'beginning-op 'thing-at-point--beginning-of-symbol)

(defun thing-at-point--beginning-of-symbol ()
  "Move point to the beginning of the current symbol."
  (and (re-search-backward "\\(\\sw\\|\\s_\\)+")
       (skip-syntax-backward "w_")))

;;  Lists

(put 'list 'bounds-of-thing-at-point 'thing-at-point-bounds-of-list-at-point)

(defun thing-at-point-bounds-of-list-at-point ()
  "Return the bounds of the list at point.
Prefer the enclosing list with fallback on sexp at point.
\[Internal function used by `bounds-of-thing-at-point'.]"
  (save-excursion
    (if (ignore-errors (up-list -1))
	(ignore-errors (cons (point) (progn (forward-sexp) (point))))
      (let ((bound (bounds-of-thing-at-point 'sexp)))
	(and bound
	     (<= (car bound) (point)) (< (point) (cdr bound))
	     bound)))))

;; Defuns

(put 'defun 'beginning-op 'beginning-of-defun)
(put 'defun 'end-op       'end-of-defun)
(put 'defun 'forward-op   'end-of-defun)

;; Things defined by sets of characters

(defmacro define-thing-chars (thing chars)
  "Define THING as a sequence of CHARS.
E.g.:
\(define-thing-chars twitter-screen-name \"[:alnum:]_\")"
  `(progn
     (put ',thing 'end-op
          (lambda ()
            (re-search-forward (concat "\\=[" ,chars "]*") nil t)))
     (put ',thing 'beginning-op
          (lambda ()
            (if (re-search-backward (concat "[^" ,chars "]") nil t)
	        (forward-char)
	      (goto-char (point-min)))))))

;;  Filenames

(defvar thing-at-point-file-name-chars "-@~/[:alnum:]_.${}#%,:"
  "Characters allowable in filenames.")

(define-thing-chars filename thing-at-point-file-name-chars)

;; Files

(defun thing-at-point-file-at-point (&optional _lax _bounds)
  "Return the name of the existing file at point."
  (when-let ((filename (thing-at-point 'filename)))
    (setq filename (expand-file-name filename))
    (and (file-exists-p filename)
         filename)))

(put 'existing-filename 'thing-at-point 'thing-at-point-file-at-point)

;;  URIs

(defvar thing-at-point-beginning-of-url-regexp nil
  "Regexp matching the beginning of a well-formed URI.
If nil, construct the regexp from `thing-at-point-uri-schemes'.")

(defvar thing-at-point-url-path-regexp
  "[^]\t\n \"'<>[^`{}]*[^]\t\n \"'<>[^`{}.,;]+"
  "Regexp matching the host and filename or e-mail part of a URL.")

(defvar thing-at-point-short-url-regexp
  (concat "[-A-Za-z0-9]+\\.[-A-Za-z0-9.]+" thing-at-point-url-path-regexp)
  "Regexp matching a URI without a scheme component.")

(defvar thing-at-point-uri-schemes
  ;; Officials from https://www.iana.org/assignments/uri-schemes.html
  '("aaa://" "about:" "acap://" "apt:" "bzr://" "bzr+ssh://"
    "attachment:/" "chrome://" "cid:" "content://" "crid://" "cvs://"
    "data:" "dav:" "dict://" "doi:" "dns:" "dtn:" "feed:" "file:/"
    "finger://" "fish://" "ftp://" "geo:" "git://" "go:" "gopher://"
    "h323:" "http://" "https://" "im:" "imap://" "info:" "ipp:"
    "irc://" "irc6://" "ircs://" "iris.beep:" "jar:" "ldap://"
    "ldaps://" "magnet:" "mailto:" "mid:"  "mtqp://" "mupdate://"
    "news:" "nfs://" "nntp://" "opaquelocktoken:" "pop://" "pres:"
    "resource://" "rmi://" "rsync://" "rtsp://" "rtspu://" "service:"
    "sftp://" "sip:" "sips:" "smb://" "sms:" "snmp://" "soap.beep://"
    "soap.beeps://" "ssh://" "svn://" "svn+ssh://" "tag:" "tel:"
    "telnet://" "tftp://" "tip://" "tn3270://" "udp://" "urn:"
    "uuid:" "vemmi://"  "webcal://" "xri://" "xmlrpc.beep://"
    "xmlrpc.beeps://" "z39.50r://" "z39.50s://" "xmpp:"
    ;; Compatibility
    "fax:" "man:" "mms://" "mmsh://" "modem:" "prospero:" "snews:"
    "wais://")
  "List of URI schemes recognized by `thing-at-point-url-at-point'.
Each string in this list should correspond to the start of a
URI's scheme component, up to and including the trailing // if
the scheme calls for that to be present.")

(defvar thing-at-point-markedup-url-regexp "<URL:\\([^<>\n]+\\)>"
  "Regexp matching a URL marked up per RFC1738.
This kind of markup was formerly recommended as a way to indicate
URIs, but as of RFC 3986 it is no longer recommended.
Subexpression 1 should contain the delimited URL.")

(defvar thing-at-point-newsgroup-regexp
  "\\`[[:lower:]]+\\.[-+[:lower:]_0-9.]+\\'"
  "Regexp matching a newsgroup name.")

(defvar thing-at-point-newsgroup-heads
  '("alt" "comp" "gnu" "misc" "news" "sci" "soc" "talk")
  "Used by `thing-at-point-newsgroup-p' if gnus is not running.")

(defvar thing-at-point-default-mail-uri-scheme "mailto"
  "Default scheme for ill-formed URIs that look like <foo@example.com>.
If nil, do not give such URIs a scheme.")

(put 'url 'bounds-of-thing-at-point 'thing-at-point-bounds-of-url-at-point)

(defun thing-at-point-bounds-of-url-at-point (&optional lax)
  "Return a cons cell containing the start and end of the URI at point.
Try to find a URI using `thing-at-point-markedup-url-regexp'.
If that fails, try with `thing-at-point-beginning-of-url-regexp'.
If that also fails, and optional argument LAX is non-nil, return
the bounds of a possible ill-formed URI (one lacking a scheme)."
  ;; Look for the old <URL:foo> markup.  If found, use it.
  (or (thing-at-point--bounds-of-markedup-url)
      ;; Otherwise, find the bounds within which a URI may exist.  The
      ;; method is similar to `ffap-string-at-point'.  Note that URIs
      ;; may contain parentheses but may not contain spaces (RFC3986).
      (let* ((allowed-chars "--:=&?$+@-Z_[:alpha:]~#,%;*()!'")
	     (skip-before "^[0-9a-zA-Z]")
	     (skip-after  ":;.,!?'")
	     (pt (point))
	     (beg (save-excursion
		    (skip-chars-backward allowed-chars)
		    (skip-chars-forward skip-before pt)
		    (point)))
	     (end (save-excursion
		    (skip-chars-forward allowed-chars)
		    (skip-chars-backward skip-after pt)
		    (point))))
	(or (thing-at-point--bounds-of-well-formed-url beg end pt)
	    (if lax (cons beg end))))))

(defun thing-at-point--bounds-of-markedup-url ()
  (when thing-at-point-markedup-url-regexp
    (let ((case-fold-search t)
	  (pt (point))
	  (beg (line-beginning-position))
	  (end (line-end-position))
	  found)
      (save-excursion
	(goto-char beg)
	(while (and (not found)
		    (<= (point) pt)
		    (< (point) end))
	  (and (re-search-forward thing-at-point-markedup-url-regexp
				  end 1)
	       (> (point) pt)
	       (setq found t))))
      (if found
	  (cons (match-beginning 1) (match-end 1))))))

(defun thing-at-point--bounds-of-well-formed-url (beg end pt)
  (save-excursion
    (goto-char beg)
    (let (url-beg paren-end regexp)
      (save-restriction
	(narrow-to-region beg end)
	;; The scheme component must either match at BEG, or have no
	;; other alphanumerical ASCII characters before it.
	(setq regexp (concat "\\(?:\\`\\|[^a-zA-Z0-9]\\)\\("
			     (or thing-at-point-beginning-of-url-regexp
				 (regexp-opt thing-at-point-uri-schemes))
			     "\\)"))
	(and (re-search-forward regexp end t)
	     ;; URI must have non-empty contents.
	     (< (point) end)
	     (setq url-beg (match-beginning 1))))
      (when url-beg
	;; If there is an open paren before the URI, truncate to the
	;; matching close paren.
	(and (> url-beg (point-min))
	     (eq (car-safe (syntax-after (1- url-beg))) 4)
	     (save-restriction
	       (narrow-to-region (1- url-beg) (min end (point-max)))
	       (setq paren-end (ignore-errors
                                 ;; Make the scan work inside comments.
                                 (let ((parse-sexp-ignore-comments nil))
                                   (scan-lists (1- url-beg) 1 0)))))
	     (not (blink-matching-check-mismatch (1- url-beg) paren-end))
	     (setq end (1- paren-end)))
	;; Ensure PT is actually within BOUNDARY. Check the following
	;; example with point on the beginning of the line:
	;;
	;; 3,1406710489,https://gnu.org,0,"0"
	(and (<= url-beg pt end) (cons url-beg end))))))

(put 'url 'thing-at-point 'thing-at-point-url-at-point)

(defun thing-at-point-url-at-point (&optional lax bounds)
  "Return the URL around or before point.
If no URL is found, return nil.

If optional argument LAX is non-nil, look for URLs that are not
well-formed, such as foo@bar or <nobody>.

If optional arguments BOUNDS are non-nil, it should be a cons
cell of the form (START . END), containing the beginning and end
positions of the URI.  Otherwise, these positions are detected
automatically from the text around point.

If the scheme component is absent, either because a URI delimited
with <url:...> lacks one, or because an ill-formed URI was found
with LAX or BEG and END, try to add a scheme in the returned URI.
The scheme is chosen heuristically: \"mailto:\" if the address
looks like an email address, \"ftp://\" if it starts with
\"ftp\", etc."
  (unless bounds
    (setq bounds (thing-at-point-bounds-of-url-at-point lax)))
  (when (and bounds (< (car bounds) (cdr bounds)))
    (let ((str (buffer-substring-no-properties (car bounds) (cdr bounds))))
      ;; If there is no scheme component, try to add one.
      (unless (string-match "\\`[a-zA-Z][-a-zA-Z0-9+.]*:" str)
	(or
	 ;; If the URI has the form <foo@bar>, treat it according to
	 ;; `thing-at-point-default-mail-uri-scheme'.  If there are
	 ;; no angle brackets, it must be mailto.
	 (when (string-match "\\`[^:</>@]+@[-.0-9=&?$+A-Z_a-z~#,%;*]" str)
	   (let ((scheme (if (and (eq (char-before (car bounds)) ?<)
				  (eq (char-after  (cdr bounds)) ?>))
			     thing-at-point-default-mail-uri-scheme
			   "mailto")))
	     (if scheme
		 (setq str (concat scheme ":" str)))))
	 ;; If the string is like <FOO>, where FOO is an existing user
	 ;; name on the system, treat that as an email address.
	 (and (string-match "\\`[[:alnum:]]+\\'" str)
	      (eq (char-before (car bounds)) ?<)
	      (eq (char-after  (cdr bounds)) ?>)
	      (not (string-search "~" (expand-file-name (concat "~" str))))
	      (setq str (concat "mailto:" str)))
	 ;; If it looks like news.example.com, treat it as news.
	 (if (thing-at-point-newsgroup-p str)
	     (setq str (concat "news:" str)))
	 ;; If it looks like ftp.example.com. treat it as ftp.
	 (if (string-match "\\`ftp\\." str)
	     (setq str (concat "ftp://" str)))
	 ;; If it looks like www.example.com. treat it as http.
	 (if (string-match "\\`www\\." str)
	     (setq str (concat "http://" str)))
	 ;; Otherwise, it just isn't a URI.
	 (setq str nil)))
      str)))

(defun thing-at-point-newsgroup-p (string)
  "Return STRING if it looks like a newsgroup name, else nil."
  (and
   (string-match thing-at-point-newsgroup-regexp string)
   (let ((htbs '(gnus-active-hashtb gnus-newsrc-hashtb gnus-killed-hashtb))
	 (heads thing-at-point-newsgroup-heads)
	 htb ret)
     (while htbs
       (setq htb (car htbs) htbs (cdr htbs))
       (ignore-errors
         (setq htb (symbol-value htb))
	 (when (cond ((obarrayp htb)
	              (intern-soft string htb))
                     ((listp htb)
                      (member string htb))
                     ((hash-table-p htb)
                      (gethash string htb)))
	   (setq ret string htbs nil))
	 ;; If we made it this far, gnus is running, so ignore "heads":
	 (setq heads nil)))
     (or ret (not heads)
	 (let ((head (string-match "\\`\\([[:lower:]]+\\)\\." string)))
	   (and head (setq head (substring string 0 (match-end 1)))
		(member head heads)
		(setq ret string))))
     ret)))

(put 'url 'end-op (lambda () (end-of-thing 'url)))

(put 'url 'beginning-op (lambda () (beginning-of-thing 'url)))

;; The normal thingatpt mechanism doesn't work for complex regexps.
;; This should work for almost any regexp wherever we are in the
;; match.  To do a perfect job for any arbitrary regexp would mean
;; testing every position before point.  Regexp searches won't find
;; matches that straddle the start position so we search forwards once
;; and then back repeatedly and then back up a char at a time.

(defun thing-at-point-looking-at (regexp &optional distance)
  "Return non-nil if point is in or just after a match for REGEXP.
Set the match data from the earliest such match ending at or after
point.

Optional argument DISTANCE limits search for REGEXP forward and
back from point."
  (save-excursion
    (let ((old-point (point))
	  (forward-bound (and distance (+ (point) distance)))
	  (backward-bound (and distance (- (point) distance)))
	  match prev-pos new-pos)
      (and (looking-at regexp)
	   (>= (match-end 0) old-point)
	   (setq match (point)))
      ;; Search back repeatedly from end of next match.
      ;; This may fail if next match ends before this match does.
      (re-search-forward regexp forward-bound 'limit)
      (setq prev-pos (point))
      (while (and (setq new-pos (re-search-backward regexp backward-bound t))
                  ;; Avoid inflooping with some regexps, such as "^",
                  ;; matching which never moves point.
                  (< new-pos prev-pos)
		  (or (> (match-beginning 0) old-point)
		      (and (looking-at regexp)	; Extend match-end past search start
			   (>= (match-end 0) old-point)
			   (setq match (point))))))
      (if (not match) nil
	(goto-char match)
	;; Back up a char at a time in case search skipped
	;; intermediate match straddling search start pos.
	(while (and (not (bobp))
		    (progn (backward-char 1) (looking-at regexp))
		    (>= (match-end 0) old-point)
		    (setq match (point))))
	(goto-char match)
	(looking-at regexp)))))

;;   Email addresses
(defvar thing-at-point-email-regexp
  "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?"
  "A regular expression probably matching an email address.
This does not match the real name portion, only the address, optionally
with angle brackets.")

;; Haven't set 'forward-op on 'email nor defined 'forward-email' because
;; not sure they're actually needed, and URL seems to skip them too.
;; Note that (end-of-thing 'email) and (beginning-of-thing 'email)
;; work automagically, though.

(put 'email 'bounds-of-thing-at-point
     (lambda ()
       (let ((thing (thing-at-point-looking-at
		     thing-at-point-email-regexp 500)))
         (if thing
             (let ((beginning (match-beginning 0))
                   (end (match-end 0)))
               (cons beginning end))))))

(put 'email 'thing-at-point
     (lambda ()
       (let ((boundary-pair (bounds-of-thing-at-point 'email)))
         (if boundary-pair
             (buffer-substring-no-properties
              (car boundary-pair) (cdr boundary-pair))))))

;;  Buffer and region

(put 'buffer 'end-op (lambda () (goto-char (point-max))))
(put 'buffer 'beginning-op (lambda () (goto-char (point-min))))
(put 'region 'bounds-of-thing-at-point
     (lambda ()
       (when (use-region-p)
         (cons (region-beginning) (region-end)))))

;; UUID

(defconst thing-at-point-uuid-regexp
  (rx bow
      (repeat 8 hex-digit) "-"
      (repeat 4 hex-digit) "-"
      (repeat 4 hex-digit) "-"
      (repeat 4 hex-digit) "-"
      (repeat 12 hex-digit)
      eow)
  "A regular expression matching a UUID.
See RFC 4122 for the description of the format.")

(put 'uuid 'bounds-of-thing-at-point
     (lambda ()
       (when (thing-at-point-looking-at thing-at-point-uuid-regexp 36)
         (cons (match-beginning 0) (match-end 0)))))

;;  Aliases

(defun word-at-point (&optional no-properties)
  "Return the word at point.  See `thing-at-point'."
  (thing-at-point 'word no-properties))

(defun sentence-at-point (&optional no-properties)
  "Return the sentence at point.  See `thing-at-point'."
  (thing-at-point 'sentence no-properties))

(defun thing-at-point--read-from-whole-string (str)
  "Read a Lisp expression from STR.
Signal an error if the entire string was not used."
  (let* ((read-data (read-from-string str))
	 (more-left
	  (condition-case nil
	      ;; The call to `ignore' suppresses a compiler warning.
	      (progn (ignore (read-from-string (substring str (cdr read-data))))
		     t)
	    (end-of-file nil))))
    (if more-left
	(error "Can't read whole string")
      (car read-data))))

(define-obsolete-function-alias 'read-from-whole-string
  'thing-at-point--read-from-whole-string "25.1"
  "This is an internal thingatpt function and should not be used.")

(defun form-at-point (&optional thing pred)
  (let* ((obj (thing-at-point (or thing 'sexp)))
         (sexp (if (stringp obj)
                   (ignore-errors
                     (thing-at-point--read-from-whole-string obj))
                 obj)))
    (if (or (not pred) (funcall pred sexp)) sexp)))

;;;###autoload
(defun sexp-at-point ()
  "Return the sexp at point, or nil if none is found."
  (form-at-point 'sexp))
;;;###autoload
(defun symbol-at-point ()
  "Return the symbol at point, or nil if none is found."
  (let ((thing (thing-at-point 'symbol)))
    (if thing (intern thing))))
;;;###autoload
(defun number-at-point ()
  "Return the number at point, or nil if none is found.
Decimal numbers like \"14\" or \"-14.5\", as well as hex numbers
like \"0xBEEF09\" or \"#xBEEF09\", are recognized."
  (cond
   ((thing-at-point-looking-at "\\(0x\\|#x\\)\\([a-fA-F0-9]+\\)" 500)
    (string-to-number
     (buffer-substring (match-beginning 2) (match-end 2))
     16))
   ((thing-at-point-looking-at "-?[0-9]+\\.?[0-9]*" 500)
    (string-to-number
     (buffer-substring (match-beginning 0) (match-end 0))))))

(put 'number 'thing-at-point 'number-at-point)
;;;###autoload
(defun list-at-point (&optional ignore-comment-or-string)
  "Return the Lisp list at point, or nil if none is found.
If IGNORE-COMMENT-OR-STRING is non-nil comments and strings are
treated as white space."
  (let ((ppss (and ignore-comment-or-string (syntax-ppss))))
    (save-excursion
      (goto-char (or (nth 8 ppss) (point)))
      (form-at-point 'list 'listp))))

;;; thingatpt.el ends here
