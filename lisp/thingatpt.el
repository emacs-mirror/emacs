;;; thingatpt.el --- get the `thing' at point

;; Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 2000,
;;   2001, 2002, 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

;; Author: Mike Williams <mikew@gopher.dosli.govt.nz>
;; Maintainer: FSF
;; Keywords: extensions, matching, mouse
;; Created: Thu Mar 28 13:48:23 1991

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This file provides routines for getting the "thing" at the location of
;; point, whatever that "thing" happens to be.  The "thing" is defined by
;; its beginning and end positions in the buffer.
;;
;; The function bounds-of-thing-at-point finds the beginning and end
;; positions by moving first forward to the end of the "thing", and then
;; backwards to the beginning.  By default, it uses the corresponding
;; forward-"thing" operator (eg. forward-word, forward-line).
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
;; Reliance on existing operators means that many `things' can be accessed
;; without further code:  eg.
;;     (thing-at-point 'line)
;;     (thing-at-point 'page)

;;; Code:

(provide 'thingatpt)

;; Basic movement

;;;###autoload
(defun forward-thing (thing &optional n)
  "Move forward to the end of the Nth next THING."
  (let ((forward-op (or (get thing 'forward-op)
			(intern-soft (format "forward-%s" thing)))))
    (if (functionp forward-op)
	(funcall forward-op (or n 1))
      (error "Can't determine how to move over a %s" thing))))

;; General routines

;;;###autoload
(defun bounds-of-thing-at-point (thing)
  "Determine the start and end buffer locations for the THING at point.
THING is a symbol which specifies the kind of syntactic entity you want.
Possibilities include `symbol', `list', `sexp', `defun', `filename', `url',
`word', `sentence', `whitespace', `line', `page' and others.

See the file `thingatpt.el' for documentation on how to define
a symbol as a valid THING.

The value is a cons cell (START . END) giving the start and end positions
of the textual entity that was found."
  (if (get thing 'bounds-of-thing-at-point)
      (funcall (get thing 'bounds-of-thing-at-point))
    (let ((orig (point)))
      (condition-case nil
	  (save-excursion
	    ;; Try moving forward, then back.
            (funcall ;; First move to end.
             (or (get thing 'end-op)
                 (lambda () (forward-thing thing 1))))
            (funcall ;; Then move to beg.
             (or (get thing 'beginning-op)
                 (lambda () (forward-thing thing -1))))
	    (let ((beg (point)))
	      (if (not (and beg (> beg orig)))
		  ;; If that brings us all the way back to ORIG,
		  ;; it worked.  But END may not be the real end.
		  ;; So find the real end that corresponds to BEG.
		  (let ((real-end
			 (progn
			   (funcall
			    (or (get thing 'end-op)
                                (lambda () (forward-thing thing 1))))
			   (point))))
		    (if (and beg real-end (<= beg orig) (<= orig real-end))
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
		  (if (and real-beg end (<= real-beg orig) (<= orig end))
		      (cons real-beg end))))))
	(error nil)))))

;;;###autoload
(defun thing-at-point (thing)
  "Return the THING at point.
THING is a symbol which specifies the kind of syntactic entity you want.
Possibilities include `symbol', `list', `sexp', `defun', `filename', `url',
`word', `sentence', `whitespace', `line', `page' and others.

See the file `thingatpt.el' for documentation on how to define
a symbol as a valid THING."
  (if (get thing 'thing-at-point)
      (funcall (get thing 'thing-at-point))
    (let ((bounds (bounds-of-thing-at-point thing)))
      (if bounds
	  (buffer-substring (car bounds) (cdr bounds))))))

;; Go to beginning/end

(defun beginning-of-thing (thing)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (or bounds (error "No %s here" thing))
    (goto-char (car bounds))))

(defun end-of-thing (thing)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (or bounds (error "No %s here" thing))
    (goto-char (cdr bounds))))

;;  Special cases

;;  Lines

;; bolp will be false when you click on the last line in the buffer
;; and it has no final newline.

(put 'line 'beginning-op
     (lambda () (if (bolp) (forward-line -1) (beginning-of-line))))

;;  Sexps

(defun in-string-p ()
  (let ((orig (point)))
    (save-excursion
      (beginning-of-defun)
      (nth 3 (parse-partial-sexp (point) orig)))))

(defun end-of-sexp ()
  (let ((char-syntax (char-syntax (char-after (point)))))
    (if (or (eq char-syntax ?\))
	    (and (eq char-syntax ?\") (in-string-p)))
	(forward-char 1)
      (forward-sexp 1))))

(put 'sexp 'end-op 'end-of-sexp)

(defun beginning-of-sexp ()
  (let ((char-syntax (char-syntax (char-before (point)))))
    (if (or (eq char-syntax ?\()
	    (and (eq char-syntax ?\") (in-string-p)))
	(forward-char -1)
      (forward-sexp -1))))

(put 'sexp 'beginning-op 'beginning-of-sexp)

;;  Lists

(put 'list 'end-op (lambda () (up-list 1)))
(put 'list 'beginning-op 'backward-sexp)

;;  Filenames and URLs  www.com/foo%32bar

(defvar thing-at-point-file-name-chars "-~/[:alnum:]_.${}#%,:"
  "Characters allowable in filenames.")

(put 'filename 'end-op
     (lambda ()
       (re-search-forward (concat "\\=[" thing-at-point-file-name-chars "]*")
			  nil t)))
(put 'filename 'beginning-op
     (lambda ()
       (if (re-search-backward (concat "[^" thing-at-point-file-name-chars "]")
			       nil t)
	   (forward-char)
	 (goto-char (point-min)))))

(defvar thing-at-point-url-path-regexp
  "[^]\t\n \"'()<>[^`{}]*[^]\t\n \"'()<>[^`{}.,;]+"
  "A regular expression probably matching the host and filename or e-mail part of a URL.")

(defvar thing-at-point-short-url-regexp
  (concat "[-A-Za-z0-9.]+" thing-at-point-url-path-regexp)
  "A regular expression probably matching a URL without an access scheme.
Hostname matching is stricter in this case than for
``thing-at-point-url-regexp''.")

(defvar thing-at-point-uri-schemes
  ;; Officials from http://www.iana.org/assignments/uri-schemes.html
  '("ftp://" "http://" "gopher://" "mailto:" "news:" "nntp:"
    "telnet://" "wais://" "file:/" "prospero:" "z39.50s:" "z39.50r:"
    "cid:" "mid:" "vemmi:" "service:" "imap:" "nfs:" "acap:" "rtsp:"
    "tip:" "pop:" "data:" "dav:" "opaquelocktoken:" "sip:" "tel:" "fax:"
    "modem:" "ldap:" "https://" "soap.beep:" "soap.beeps:" "urn:" "go:"
    "afs:" "tn3270:" "mailserver:"
    "crid:" "dict:" "dns:" "dtn:" "h323:" "im:" "info:" "ipp:"
    "iris.beep:" "mtqp:" "mupdate:" "pres:" "sips:" "snmp:" "tag:"
    "tftp:" "xmlrpc.beep:" "xmlrpc.beeps:" "xmpp:"
  ;; Compatibility
    "snews:" "irc:" "mms://" "mmsh://")
  "Uniform Resource Identifier (URI) Schemes.")

(defvar thing-at-point-url-regexp
  (concat "\\<\\(" (mapconcat 'identity thing-at-point-uri-schemes "\\|") "\\)"
          thing-at-point-url-path-regexp)
  "A regular expression probably matching a complete URL.")

(defvar thing-at-point-markedup-url-regexp
  "<URL:[^>]+>"
  "A regular expression matching a URL marked up per RFC1738.
This may contain whitespace (including newlines) .")

(put 'url 'bounds-of-thing-at-point 'thing-at-point-bounds-of-url-at-point)
(defun thing-at-point-bounds-of-url-at-point ()
  (let ((strip (thing-at-point-looking-at
			 thing-at-point-markedup-url-regexp))) ;; (url "") short
    (if (or strip
	    (thing-at-point-looking-at thing-at-point-url-regexp)
	    ;; Access scheme omitted?
	    ;; (setq short (thing-at-point-looking-at
	    ;;     	 thing-at-point-short-url-regexp))
            )
	(let ((beginning (match-beginning 0))
	      (end (match-end 0)))
	  (when strip
            (setq beginning (+ beginning 5))
            (setq end (- end 1)))
	  (cons beginning end)))))

(put 'url 'thing-at-point 'thing-at-point-url-at-point)
(defun thing-at-point-url-at-point ()
  "Return the URL around or before point.

Search backwards for the start of a URL ending at or after point.  If
no URL found, return nil.  The access scheme will be prepended if
absent: \"mailto:\" if the string contains \"@\", \"ftp://\" if it
starts with \"ftp\" and not \"ftp:/\", or \"http://\" by default."

  (let ((url "") short strip)
    (if (or (setq strip (thing-at-point-looking-at
			 thing-at-point-markedup-url-regexp))
	    (thing-at-point-looking-at thing-at-point-url-regexp)
	    ;; Access scheme omitted?
	    (setq short (thing-at-point-looking-at
			 thing-at-point-short-url-regexp)))
	(progn
	  (setq url (buffer-substring-no-properties (match-beginning 0)
						    (match-end 0)))
	  (and strip (setq url (substring url 5 -1))) ; Drop "<URL:" & ">"
	  ;; strip whitespace
	  (while (string-match "[ \t\n\r]+" url)
	    (setq url (replace-match "" t t url)))
	  (and short (setq url (concat (cond ((string-match "^[a-zA-Z]+:" url)
					       ;; already has a URL scheme.
					       "")
					     ((string-match "@" url)
                                              "mailto:")
					     ;; e.g. ftp.swiss... or ftp-swiss...
                                             ((string-match "^ftp" url)
                                              "ftp://")
                                             (t "http://"))
                                       url)))
	  (if (string-equal "" url)
	      nil
	    url)))))

;; The normal thingatpt mechanism doesn't work for complex regexps.
;; This should work for almost any regexp wherever we are in the
;; match.  To do a perfect job for any arbitrary regexp would mean
;; testing every position before point.  Regexp searches won't find
;; matches that straddle the start position so we search forwards once
;; and then back repeatedly and then back up a char at a time.

(defun thing-at-point-looking-at (regexp)
  "Return non-nil if point is in or just after a match for REGEXP.
Set the match data from the earliest such match ending at or after
point."
  (save-excursion
    (let ((old-point (point)) match)
      (and (looking-at regexp)
	   (>= (match-end 0) old-point)
	   (setq match (point)))
      ;; Search back repeatedly from end of next match.
      ;; This may fail if next match ends before this match does.
      (re-search-forward regexp nil 'limit)
      (while (and (re-search-backward regexp nil t)
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

(put 'url 'end-op
     (lambda ()
       (let ((bounds (thing-at-point-bounds-of-url-at-point)))
         (if bounds
             (goto-char (cdr bounds))
           (error "No URL here")))))
(put 'url 'beginning-op
     (lambda ()
       (let ((bounds (thing-at-point-bounds-of-url-at-point)))
         (if bounds
             (goto-char (car bounds))
           (error "No URL here")))))

;;  Whitespace

(defun forward-whitespace (arg)
  (interactive "p")
  (if (natnump arg)
      (re-search-forward "[ \t]+\\|\n" nil 'move arg)
    (while (< arg 0)
      (if (re-search-backward "[ \t]+\\|\n" nil 'move)
	  (or (eq (char-after (match-beginning 0)) 10)
	      (skip-chars-backward " \t")))
      (setq arg (1+ arg)))))

;;  Buffer

(put 'buffer 'end-op (lambda () (goto-char (point-max))))
(put 'buffer 'beginning-op (lambda () (goto-char (point-min))))

;;  Symbols

(defun forward-symbol (arg)
  (interactive "p")
  (if (natnump arg)
      (re-search-forward "\\(\\sw\\|\\s_\\)+" nil 'move arg)
    (while (< arg 0)
      (if (re-search-backward "\\(\\sw\\|\\s_\\)+" nil 'move)
	  (skip-syntax-backward "w_"))
      (setq arg (1+ arg)))))

;;  Syntax blocks

(defun forward-same-syntax (&optional arg)
  (interactive "p")
  (while (< arg 0)
    (skip-syntax-backward
     (char-to-string (char-syntax (char-after (1- (point))))))
    (setq arg (1+ arg)))
  (while (> arg 0)
    (skip-syntax-forward (char-to-string (char-syntax (char-after (point)))))
    (setq arg (1- arg))))

;;  Aliases

(defun word-at-point () (thing-at-point 'word))
(defun sentence-at-point () (thing-at-point 'sentence))

(defun read-from-whole-string (str)
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

(defun form-at-point (&optional thing pred)
  (let ((sexp (condition-case nil
		  (read-from-whole-string (thing-at-point (or thing 'sexp)))
		(error nil))))
    (if (or (not pred) (funcall pred sexp)) sexp)))

;;;###autoload
(defun sexp-at-point ()   (form-at-point 'sexp))
;;;###autoload
(defun symbol-at-point ()
  (let ((thing (thing-at-point 'symbol)))
    (if thing (intern thing))))
;;;###autoload
(defun number-at-point () (form-at-point 'sexp 'numberp))
;;;###autoload
(defun list-at-point ()   (form-at-point 'list 'listp))

;; arch-tag: bb65a163-dae2-4055-aedc-fe11f497f698
;;; thingatpt.el ends here
