;;; nnheader.el --- header access macros for Gnus and its backends  -*- lexical-binding: t; -*-

;; Copyright (C) 1987-1990, 1993-1998, 2000-2022 Free Software
;; Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;;	Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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

(eval-when-compile (require 'cl-lib))

(defvar gnus-decode-encoded-word-function)
(defvar gnus-decode-encoded-address-function)
(defvar gnus-alter-header-function)

(defvar nnmail-extra-headers)
(defvar gnus-newsgroup-name)
(defvar jka-compr-compression-info-list)

;; Requiring `gnus-util' at compile time creates a circular
;; dependency between nnheader.el and gnus-util.el.
;;(eval-when-compile (require 'gnus-util))

(require 'mail-utils)
(require 'mm-util)
(require 'gnus-util)
(autoload 'gnus-remove-odd-characters "gnus-sum")
(autoload 'gnus-range-add "gnus-range")
(autoload 'gnus-remove-from-range "gnus-range")
;; FIXME none of these are used explicitly in this file.
(autoload 'gnus-sorted-intersection "gnus-range")
(autoload 'gnus-intersection "gnus-range")
(autoload 'gnus-sorted-complement "gnus-range")
(autoload 'gnus-sorted-difference "gnus-range")

(defcustom gnus-verbose-backends 7
  "Integer that says how verbose the Gnus backends should be.
The higher the number, the more messages the Gnus backends will flash
to say what it's doing.  At zero, the Gnus backends will be totally
mute; at five, they will display most important messages; and at ten,
they will keep on jabbering all the time."
  :group 'gnus-start
  :type 'integer)

(defcustom gnus-nov-is-evil nil
  "If non-nil, Gnus backends will never output headers in the NOV format."
  :group 'gnus-server
  :type 'boolean)

(defcustom nnheader-max-head-length 8192
  "Max length of the head of articles.

Value is an integer, nil, or t.  nil means read in chunks of a file
indefinitely until a complete head is found; t means always read the
entire file immediately, disregarding `nnheader-head-chop-length'.

Integer values will in effect be rounded up to the nearest multiple of
`nnheader-head-chop-length'."
  :group 'gnus-article-various		; FIXME?
  :type '(choice integer (const :tag "Read chunks" nil)
		 (const :tag "Read entire file" t)))

(defcustom nnheader-head-chop-length 2048
  "Length of each read operation when trying to fetch HEAD headers."
  :group 'gnus-article-various		; FIXME?
  :type 'integer)

(defvar nnheader-read-timeout
  (if (memq system-type '(windows-nt cygwin))
      ;; http://thread.gmane.org/v9655t3pjo.fsf@marauder.physik.uni-ulm.de
      ;;
      ;; IIRC, values lower than 1.0 didn't/don't work on Windows/DOS.
      ;;
      ;; There should probably be a runtime test to determine the timing
      ;; resolution, or a primitive to report it.  I don't know off-hand
      ;; what's possible.  Perhaps better, maybe the Windows/DOS primitive
      ;; could round up non-zero timeouts to a minimum of 1.0?
      1.0
    ;; 2008-05-19 change by Larsi:
    ;; Change the default timeout from 0.1 seconds to 0.01 seconds.  This will
    ;; make nntp and pop3 article retrieval faster in some cases, but might
    ;; make CPU usage larger.  If this has any bad side effects, we might
    ;; revert this change.
    0.01)
  ;; When changing this variable, consider changing `pop3-read-timeout' as
  ;; well.
  "How long nntp should wait between checking for the end of output.
Shorter values mean quicker response, but are more CPU intensive.")

(defcustom nnheader-file-name-translation-alist
  (if (memq system-type '(windows-nt cygwin))
      (mapcar (lambda (c) (cons c ?_)) '(?: ?* ?\" ?< ?> ??)))
  "Alist that says how to translate characters in file names.
For instance, if \":\" is invalid as a file character in file names
on your system, you could say something like:

\(setq nnheader-file-name-translation-alist \\='((?: . ?_)))"
  :group 'gnus-article-various		; FIXME?
  :type '(alist :key-type character :value-type character))

(defcustom nnheader-directory-separator-character
  (string-to-char (substring (file-name-as-directory ".") -1))
  "A character used as a directory separator."
  :group 'gnus-article-various		; FIXME?
  :type 'character)

(autoload 'nnmail-message-id "nnmail")
(autoload 'mail-position-on-field "sendmail")

;;; Header access macros.

;; These macros may look very much like the ones in GNUS 4.1.  They
;; are, in a way, but you should note that the indices they use have
;; been changed from the internal GNUS format to the NOV format.  The
;; makes it possible to read headers from XOVER much faster.
;;
;; The format of a header is now:
;; [number subject from date id references chars lines xref extra]
;;
;; (That next-to-last entry is defined as "misc" in the NOV format,
;; but Gnus uses it for xrefs.)

(defalias 'mail-header-p #'vectorp)     ;For lack of tag, it's all we have.
(cl-defstruct (mail-header
               (:type vector)
               (:constructor nil)
               (:constructor make-full-mail-header
                (&optional number subject from date id
			   references chars lines xref
			   extra)))
  number
  subject
  from
  date
  id
  references
  chars
  lines
  xref
  extra)

(defalias 'mail-header-message-id #'mail-header-id)

(defsubst make-mail-header (&optional init)
  "Create a new mail header structure initialized with INIT."
  (make-full-mail-header init init init init init
                         init init init init init))

;; fake message-ids: generation and detection

(defvar nnheader-fake-message-id 1)

(defsubst nnheader-generate-fake-message-id (&optional number)
  (if (numberp number)
      (format "fake+none+%s+%d" gnus-newsgroup-name number)
    (format "fake+none+%s+%s"
	    gnus-newsgroup-name
	    (int-to-string (cl-incf nnheader-fake-message-id)))))

(defsubst nnheader-fake-message-id-p (id)
  (save-match-data		       ; regular message-id's are <.*>
    (string-match "\\`fake\\+none\\+.*\\+[0-9]+\\'" id)))

;; Parsing headers and NOV lines.

(defsubst nnheader-remove-cr-followed-by-lf ()
  (goto-char (point-max))
  (while (search-backward "\r\n" nil t)
    (delete-char 1)))

(defsubst nnheader-header-value ()
  (skip-chars-forward " \t")
  (buffer-substring (point) (point-at-eol)))

(autoload 'ietf-drums-unfold-fws "ietf-drums")


(defsubst nnheader-head-make-header (number)
  "Return a full mail header with article NUMBER.
Do this using data of type `head' in the current buffer."
  (let ((p (point-min))
	(cur (current-buffer))
	in-reply-to chars lines end ref)
    ;; This implementation of this function, with nine
    ;; search-forwards instead of the one re-search-forward and a
    ;; case (which basically was the old function) is actually
    ;; about twice as fast, even though it looks messier.  You
    ;; can't have everything, I guess.  Speed and elegance don't
    ;; always go hand in hand.
    (make-full-mail-header
     ;; Number.
     number
     ;; Subject.
     (progn
       (goto-char p)
       (if (search-forward "\nsubject:" nil t)
	   (funcall gnus-decode-encoded-word-function
		    (nnheader-header-value))
	 "(none)"))
     ;; From.
     (progn
       (goto-char p)
       (if (search-forward "\nfrom:" nil t)
	   (funcall gnus-decode-encoded-address-function
		    (nnheader-header-value))
	 "(nobody)"))
     ;; Date.
     (progn
       (goto-char p)
       (if (search-forward "\ndate:" nil t)
	   (nnheader-header-value) ""))
     ;; Message-ID.
     (progn
       (goto-char p)
       (if (re-search-forward
	    "^message-id: *\\(<[^\n\t> ]+>\\)" nil t)
	   ;; We do it this way to make sure the Message-ID
	   ;; is (somewhat) syntactically valid.
	   (buffer-substring (match-beginning 1)
			     (match-end 1))
	 ;; If there was no message-id, we just fake one to make
	 ;; subsequent routines simpler.
	 (nnheader-generate-fake-message-id number)))
     ;; References.
     (progn
       (goto-char p)
       (if (search-forward "\nreferences:" nil t)
	   (progn
	     (setq end (point))
	     (prog1
		 (nnheader-header-value)
	       (setq ref
		     (buffer-substring
		      (progn
			(end-of-line)
			(search-backward ">" end t)
			(1+ (point)))
		      (progn
			(search-backward "<" end t)
			(point))))))
	 ;; Get the references from the in-reply-to header if there
	 ;; were no references and the in-reply-to header looks
	 ;; promising.
	 (if (and (search-forward "\nin-reply-to:" nil t)
		  (setq in-reply-to (nnheader-header-value))
		  (string-match "<[^>]+>" in-reply-to))
	     (let (ref2)
	       (setq ref (substring in-reply-to (match-beginning 0)
				    (match-end 0)))
	       (while (string-match "<[^>]+>" in-reply-to (match-end 0))
		 (setq ref2 (substring in-reply-to (match-beginning 0)
				       (match-end 0)))
		 (when (> (length ref2) (length ref))
		   (setq ref ref2)))
	       ref)
	   nil)))
     ;; Chars.
     (progn
       (goto-char p)
       (if (search-forward "\nchars: " nil t)
	   (if (numberp (setq chars (ignore-errors (read cur))))
	       chars -1)
	 -1))
     ;; Lines.
     (progn
       (goto-char p)
       (if (search-forward "\nlines: " nil t)
	   (if (numberp (setq lines (ignore-errors (read cur))))
	       lines -1)
	 -1))
     ;; Xref.
     (progn
       (goto-char p)
       (and (search-forward "\nxref:" nil t)
	    (nnheader-header-value)))
     ;; Extra.
     (when nnmail-extra-headers
       (let ((extra nnmail-extra-headers)
	     out)
	 (while extra
	   (goto-char p)
	   (when (search-forward
		  (concat "\n" (symbol-name (car extra)) ":") nil t)
	     (push (cons (car extra) (nnheader-header-value))
		   out))
	   (pop extra))
	 out)))))

(defun nnheader-parse-head (&optional naked temp)
  "Parse data of type `header' in the current buffer and return a mail header.
Modify the buffer contents in the process.  The buffer is assumed
to begin each header with an \"Article retrieved\" line with an
article number; if NAKED is non-nil this line is assumed absent,
and the buffer should contain a single header's worth of data.
If TEMP is non-nil the data is first copied to a temporary buffer
leaving the original buffer untouched."
  (let ((cur (current-buffer))
	(num 0)
	(beg (point-min))
	(end (point-max))
	buf)
    (when (or naked
	      ;; Search to the beginning of the next header.  Error
	      ;; messages do not begin with 2 or 3.
	      (when (re-search-forward "^[23][0-9]+ " nil t)
		(setq num (read cur)
		      beg (point)
		      end (if (search-forward "\n.\n" nil t)
                              (goto-char (- (point) 2))
			    (point)))))
      ;; When TEMP copy the data to a temporary buffer.
      (if temp
	  (progn
	    (set-buffer (setq buf (generate-new-buffer " *nnheader-temp*")))
	    (insert-buffer-substring cur beg end))
        ;; Otherwise just narrow to the data.
	(narrow-to-region beg end))
      (let ((case-fold-search t)
	    (buffer-read-only nil)
	    header)
	(nnheader-remove-cr-followed-by-lf)
	(ietf-drums-unfold-fws)
        (subst-char-in-region (point-min) (point-max) ?\t ?\s t)
        (subst-char-in-region (point-min) (point-max) ?\r ?\s t)
	(goto-char (point-min))
	(insert "\n")
	(setq header (nnheader-head-make-header num))
	(goto-char (point-min))
	(delete-char 1)
	(if temp
	    (kill-buffer buf)
	  (goto-char (point-max))
	  (widen))
	(when gnus-alter-header-function
	  (funcall gnus-alter-header-function header))
	header))))

(defmacro nnheader-nov-skip-field ()
  '(search-forward "\t" eol 'move))

(defmacro nnheader-nov-field ()
  '(buffer-substring (point) (if (nnheader-nov-skip-field) (1- (point)) eol)))

(defmacro nnheader-nov-read-integer ()
  '(prog1
       (if (eq (char-after) ?\t)
	   0
	 (let ((num (condition-case nil
			(read (current-buffer))
		      (error nil))))
	   (if (numberp num) num 0)))
     (or (eobp) (forward-char 1))))

(defmacro nnheader-nov-parse-extra ()
  '(let (out string)
     (while (not (memq (char-after) '(?\n nil)))
       (setq string (nnheader-nov-field))
       (when (string-match "^\\([^ :]+\\): " string)
	 (push (cons (intern (match-string 1 string))
		     (substring string (match-end 0)))
	       out)))
     out))

(eval-and-compile
  (defvar nnheader-uniquify-message-id nil))

(defmacro nnheader-nov-read-message-id (&optional number)
  `(let ((id (nnheader-nov-field)))
     (if (string-match "^<[^>]+>$" id)
	 ,(if nnheader-uniquify-message-id
	      '(if (string-match "__[^@]+@" id)
		   (concat (substring id 0 (match-beginning 0))
			   (substring id (1- (match-end 0))))
		 id)
	    'id)
       (nnheader-generate-fake-message-id ,number))))

(defalias 'nnheader-nov-make-header 'nnheader-parse-nov)
(autoload 'gnus-extract-message-id-from-in-reply-to "gnus-sum")

(defun nnheader-parse-nov (&optional number)
  (let ((eol (point-at-eol))
	references in-reply-to x header)
      (setq header
	    (make-full-mail-header
	     (or number (nnheader-nov-read-integer))      ; number
	     (condition-case ()	; subject
		 (gnus-remove-odd-characters
		  (funcall gnus-decode-encoded-word-function
			   (setq x (nnheader-nov-field))))
	       (error x))
	     (condition-case ()	; from
		 (gnus-remove-odd-characters
		  (funcall gnus-decode-encoded-address-function
			   (setq x (nnheader-nov-field))))
	       (error x))
	     (nnheader-nov-field)	; date
	     (nnheader-nov-read-message-id number)	; id
	     (setq references (nnheader-nov-field))	; refs
	     (nnheader-nov-read-integer) ; chars
	     (nnheader-nov-read-integer) ; lines
	     (unless (eobp)
	       (if (looking-at "Xref: ")
		   (goto-char (match-end 0)))
	       (nnheader-nov-field)) ; Xref
	     (nnheader-nov-parse-extra))) ; extra

      (when (and (string= references "")
		 (setq in-reply-to (mail-header-extra header))
		 (setq in-reply-to (cdr (assq 'In-Reply-To in-reply-to))))
	(setf (mail-header-references header)
              (gnus-extract-message-id-from-in-reply-to in-reply-to)))
      header))


(defun nnheader-insert-nov (header)
  (princ (mail-header-number header) (current-buffer))
  (let ((p (point)))
    (insert
     "\t"
     (or (mail-header-subject header) "(none)") "\t"
     (or (mail-header-from header) "(nobody)") "\t"
     (or (mail-header-date header) "") "\t"
     (or (mail-header-id header)
	 (nnmail-message-id))
     "\t"
     (or (mail-header-references header) "") "\t")
    (princ (or (mail-header-chars header) 0) (current-buffer))
    (insert "\t")
    (princ (or (mail-header-lines header) 0) (current-buffer))
    (insert "\t")
    (when (mail-header-xref header)
      (insert "Xref: " (mail-header-xref header)))
    (when (or (mail-header-xref header)
	      (mail-header-extra header))
      (insert "\t"))
    (when (mail-header-extra header)
      (let ((extra (mail-header-extra header)))
	(while extra
	  (insert (symbol-name (caar extra))
		  ": " (if (stringp (cdar extra)) (cdar extra) "") "\t")
	  (pop extra))))
    (insert "\n")
    (backward-char 1)
    (while (search-backward "\n" p t)
      (delete-char 1))
    (forward-line 1)))

(defun nnheader-write-overview-file (file headers)
  "Write HEADERS to FILE."
  (with-temp-file file
    (mapcar #'nnheader-insert-nov headers)))

(defun nnheader-insert-header (header)
  (insert
   "Subject: " (or (mail-header-subject header) "(none)") "\n"
   "From: " (or (mail-header-from header) "(nobody)") "\n"
   "Date: " (or (mail-header-date header) "") "\n"
   "Message-ID: " (or (mail-header-id header) (nnmail-message-id)) "\n"
   "References: " (or (mail-header-references header) "") "\n"
   "Lines: ")
  (princ (or (mail-header-lines header) 0) (current-buffer))
  (insert "\n\n"))

(defun nnheader-insert-article-line (article)
  (goto-char (point-min))
  (insert "220 ")
  (princ article (current-buffer))
  (insert " Article retrieved.\n")
  (search-forward "\n\n" nil 'move)
  (delete-region (point) (point-max))
  (forward-char -1)
  (insert "."))

(defun nnheader-nov-delete-outside-range (beg end)
  "Delete all NOV lines that lie outside the BEG to END range."
  ;; First we find the first wanted line.
  (nnheader-find-nov-line beg)
  (delete-region (point-min) (point))
  ;; Then we find the last wanted line.
  (when (nnheader-find-nov-line end)
    (forward-line 1))
  (delete-region (point) (point-max)))

(defun nnheader-find-nov-line (article)
  "Put point at the NOV line that start with ARTICLE.
If ARTICLE doesn't exist, put point where that line
would have been.  The function will return non-nil if
the line could be found."
  ;; This function basically does a binary search.
  (let ((max (point-max))
	(min (goto-char (point-min)))
	(cur (current-buffer))
	(prev (point-min))
	num found)
    (while (not found)
      (goto-char (+ min (/ (- max min) 2)))
      (beginning-of-line)
      (if (or (= (point) prev)
	      (eobp))
	  (setq found t)
	(setq prev (point))
	(while (and (not (numberp (setq num (read cur))))
		    (not (eobp)))
	  (gnus-delete-line))
	(cond ((> num article)
	       (setq max (point)))
	      ((< num article)
	       (setq min (point)))
	      (t
	       (setq found 'yes)))))
    ;; We may be at the first line.
    (when (and (not num)
	       (not (eobp)))
      (setq num (read cur)))
    ;; Now we may have found the article we're looking for, or we
    ;; may be somewhere near it.
    (when (and (not (eq found 'yes))
	       (not (eq num article)))
      (setq found (point))
      (while (and (< (point) max)
		  (or (not (numberp num))
		      (< num article)))
	(forward-line 1)
	(setq found (point))
	(unless (eobp)
	  (setq num (read cur))))
      (unless (eq num article)
	(goto-char found)))
    (beginning-of-line)
    (eq num article)))

;; Various cruft the backends and Gnus need to communicate.

(defvar nntp-server-buffer nil)
(defvar nntp-process-response nil)
(defvar nnheader-file-coding-system 'undecided
  "Coding system used in file backends of Gnus.")
(defvar nnheader-callback-function nil)

(autoload 'gnus-get-buffer-create "gnus")

(defun nnheader-init-server-buffer ()
  "Initialize the Gnus-backend communication buffer."
  (unless (gnus-buffer-live-p nntp-server-buffer)
    (setq nntp-server-buffer (gnus-get-buffer-create " *nntpd*")))
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (mm-enable-multibyte)
    (kill-all-local-variables)
    (setq case-fold-search t)		;Should ignore case.
    (setq-local nntp-process-response nil)
    t))

;;; Various functions the backends use.

(defun nnheader-file-error (file)
  "Return a string that says what is wrong with FILE."
  (format
   (cond
    ((not (file-exists-p file))
     "%s does not exist")
    ((file-directory-p file)
     "%s is a directory")
    ((not (file-readable-p file))
     "%s is not readable"))
   file))

(defun nnheader-insert-head (file)
  "Insert the head of the article."
  (when (file-exists-p file)
    (if (eq nnheader-max-head-length t)
	;; Just read the entire file.
	(nnheader-insert-file-contents file)
      ;; Read blocks of the size specified by `nnheader-head-chop-length'
      ;; until we find a separator.
      (let ((beg 0)
	    (start (point))
	    ;; Use `binary' to prevent the contents from being decoded,
	    ;; or it will change the number of characters that
	    ;; `insert-file-contents' returns.
	    (coding-system-for-read 'binary))
	(while (and (eq nnheader-head-chop-length
			(nth 1 (mm-insert-file-contents
				file nil beg
				(cl-incf beg nnheader-head-chop-length))))
		    ;; CRLF or CR might be used for the line-break code.
		    (prog1 (not (re-search-forward "\n\r?\n\\|\r\r" nil t))
		      (goto-char (point-max)))
		    (or (null nnheader-max-head-length)
			(< beg nnheader-max-head-length))))
	;; Finally decode the contents.
	(when (mm-coding-system-p nnheader-file-coding-system)
	  (decode-coding-region start (point-max)
				nnheader-file-coding-system))))
    t))

(defun nnheader-article-p ()
  "Say whether the current buffer looks like an article."
  (goto-char (point-min))
  (if (not (search-forward "\n\n" nil t))
      nil
    (narrow-to-region (point-min) (1- (point)))
    (goto-char (point-min))
    (while (looking-at "[a-zA-Z][^ \t]+:.*\n\\([ \t].*\n\\)*\\|From .*\n")
      (goto-char (match-end 0)))
    (prog1
	(eobp)
      (widen))))

(defun nnheader-insert-references (references message-id)
  "Insert a References header based on REFERENCES and MESSAGE-ID."
  (if (and (not references) (not message-id))
      ;; This is invalid, but not all articles have Message-IDs.
      ()
    (mail-position-on-field "References")
    (let ((begin (point-at-bol))
	  (fill-column 78)
	  (fill-prefix "\t"))
      (when references
	(insert references))
      (when (and references message-id)
	(insert " "))
      (when message-id
	(insert message-id))
      ;; Fold long References lines to conform to RFC1036 (sort of).
      ;; The region must end with a newline to fill the region
      ;; without inserting extra newline.
      (fill-region-as-paragraph begin (1+ (point))))))

(declare-function message-remove-header "message"
		  (header &optional is-regexp first reverse))

(defun nnheader-replace-header (header new-value)
  "Remove HEADER and insert the NEW-VALUE."
  (require 'message)
  (save-excursion
    (save-restriction
      (nnheader-narrow-to-headers)
      (prog1
	  (message-remove-header header)
	(goto-char (point-max))
	(insert header ": " new-value "\n")))))

(defun nnheader-narrow-to-headers ()
  "Narrow to the head of an article."
  (widen)
  (narrow-to-region
   (goto-char (point-min))
   (if (search-forward "\n\n" nil t)
       (1- (point))
     (point-max)))
  (goto-char (point-min)))

(defun nnheader-get-lines-and-char ()
  "Return the number of lines and chars in the article body."
  (goto-char (point-min))
  (if (not (re-search-forward "\n\r?\n" nil t))
      (list 0 0)
    (list (count-lines (point) (point-max))
	  (- (point-max) (point)))))

(defun nnheader-remove-body ()
  "Remove the body from an article in this current buffer."
  (goto-char (point-min))
  (when (re-search-forward "\n\r?\n" nil t)
    (delete-region (point) (point-max))))

(defun nnheader-set-temp-buffer (name &optional noerase)
  "Set-buffer to an empty (possibly new) buffer called NAME with undo disabled."
  (set-buffer (gnus-get-buffer-create name))
  (buffer-disable-undo)
  (unless noerase
    (erase-buffer))
  (current-buffer))

(defvar nnheader-numerical-files
  (if (boundp 'jka-compr-compression-info-list)
      (concat "\\([0-9]+\\)\\("
	      (mapconcat (lambda (i) (aref i 0))
			 jka-compr-compression-info-list "\\|")
	      "\\)?")
    "[0-9]+$")
  "Regexp that match numerical files.")

(defvar nnheader-numerical-short-files (concat "^" nnheader-numerical-files)
  "Regexp that matches numerical file names.")

(defvar nnheader-numerical-full-files (concat "/" nnheader-numerical-files)
  "Regexp that matches numerical full file names.")

(defsubst nnheader-file-to-number (file)
  "Take a FILE name and return the article number."
  (if (string= nnheader-numerical-short-files "^[0-9]+$")
      (string-to-number file)
    (string-match nnheader-numerical-short-files file)
    (string-to-number (match-string 0 file))))

(defvar nnheader-directory-files-is-safe (not (eq system-type 'windows-nt))
  "If non-nil, Gnus believes `directory-files' is safe.
It has been reported numerous times that `directory-files' fails with
an alarming frequency on NFS mounted file systems.  If it is nil,
`nnheader-directory-files-safe' is used.")

(defun nnheader-directory-files-safe (&rest args)
  "Execute `directory-files' twice and returns the longer result."
  (let ((first (apply #'directory-files args))
	(second (apply #'directory-files args)))
    (if (> (length first) (length second))
	first
      second)))

(defun nnheader-directory-articles (dir)
  "Return a list of all article files in directory DIR."
  (mapcar #'nnheader-file-to-number
	  (if nnheader-directory-files-is-safe
	      (directory-files
	       dir nil nnheader-numerical-short-files t)
	    (nnheader-directory-files-safe
	     dir nil nnheader-numerical-short-files t))))

(defun nnheader-article-to-file-alist (dir)
  "Return an alist of article/file pairs in DIR."
  (mapcar (lambda (file) (cons (nnheader-file-to-number file) file))
	  (if nnheader-directory-files-is-safe
	      (directory-files
	       dir nil nnheader-numerical-short-files t)
	    (nnheader-directory-files-safe
	     dir nil nnheader-numerical-short-files t))))

(defun nnheader-fold-continuation-lines ()
  "Fold continuation lines in the current buffer."
  (nnheader-replace-regexp "\\(\r?\n[ \t]+\\)+" " "))

(defun nnheader-translate-file-chars (file &optional full)
  "Translate FILE into something that can be a file name.
If FULL, translate everything."
  (if (null nnheader-file-name-translation-alist)
      ;; No translation is necessary.
      file
    (let* ((i 0)
	   trans leaf path len)
      (if full
	  ;; Do complete translation.
	  (setq leaf (copy-sequence file)
		path ""
		i (if (and (< 1 (length leaf)) (eq ?: (aref leaf 1)))
		      2 0))
	;; We translate -- but only the file name.  We leave the directory
	;; alone.
	(setq leaf (file-name-nondirectory file)
	      path (file-name-directory file)))
      (setq len (length leaf))
      (while (< i len)
	(when (setq trans (cdr (assq (aref leaf i)
				     nnheader-file-name-translation-alist)))
	  (aset leaf i trans))
	(cl-incf i))
      (concat path leaf))))

(defun nnheader-report (backend &rest args)
  "Report an error from the BACKEND.
The first string in ARGS can be a format string."
  (set (intern (format "%s-status-string" backend))
       (if (< (length args) 2)
	   (car args)
	 (apply #'format args)))
  nil)

(defun nnheader-get-report-string (backend)
  "Get the most recent report from BACKEND."
  (condition-case ()
      (format "%s" (symbol-value (intern (format "%s-status-string"
						 backend))))
    (error "")))

(defun nnheader-get-report (backend)
  "Get the most recent report from BACKEND."
  (nnheader-message 5 (nnheader-get-report-string backend)))

(defun nnheader-insert (format &rest args)
  "Clear the communication buffer and insert FORMAT and ARGS into the buffer.
If FORMAT isn't a format string, it and all ARGS will be inserted
without formatting."
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (if (string-search "%" format)
	(insert (apply #'format format args))
      (apply #'insert format args))
    t))

(defsubst nnheader-replace-chars-in-string (string from to)
  (subst-char-in-string from to string))

(defun nnheader-replace-duplicate-chars-in-string (string from to)
  "Replace characters in STRING from FROM to TO."
  (let ((string (substring string 0))	;Copy string.
	(len (length string))
	(idx 0) prev i)
    ;; Replace all occurrences of FROM with TO.
    (while (< idx len)
      (setq i (aref string idx))
      (when (and (eq prev from) (= i from))
	(aset string (1- idx) to)
	(aset string idx to))
      (setq prev i)
      (setq idx (1+ idx)))
    string))

(defun nnheader-file-to-group (file &optional top)
  "Return a group name based on FILE and TOP."
  (nnheader-replace-chars-in-string
   (if (not top)
       file
     (condition-case ()
	 (substring (expand-file-name file)
		    (length
		     (expand-file-name
		      (file-name-as-directory top))))
       (error "")))
   nnheader-directory-separator-character ?.))

(defun nnheader-message (level &rest args)
  "Message if the Gnus backends are talkative."
  (apply (cond
	  ((and (numberp gnus-verbose-backends)
	        (> level gnus-verbose-backends))
	   #'format)
	  (gnus-add-timestamp-to-message #'gnus-message-with-timestamp)
	  (t #'message))
	 args))

(defun nnheader-be-verbose (level)
  "Return whether the backends should be verbose on LEVEL."
  (or (not (numberp gnus-verbose-backends))
      (<= level gnus-verbose-backends)))

(defcustom nnheader-pathname-coding-system 'iso-8859-1
  "Coding system for file name."
  :group 'gnus-article-various		; FIXME?
  :type 'coding-system)

(defun nnheader-group-pathname (group dir &optional file)
  "Make file name for GROUP."
  (concat
   (let ((dir (file-name-as-directory (expand-file-name dir))))
     ;; If this directory exists, we use it directly.
     (file-name-as-directory
      (if (file-directory-p (concat dir group))
	  (expand-file-name group dir)
	;; If not, we translate dots into slashes.
	(expand-file-name (encode-coding-string
			   (nnheader-replace-chars-in-string group ?. ?/)
			   nnheader-pathname-coding-system)
			  dir))))
   (cond ((null file) "")
	 ((numberp file) (int-to-string file))
	 (t file))))

(defun nnheader-concat (dir &rest files)
  "Concat DIR as directory to FILES."
  (apply #'concat (file-name-as-directory dir) files))

(defun nnheader-ms-strip-cr ()
  "Strip ^M from the end of all lines."
  (save-excursion
    (nnheader-remove-cr-followed-by-lf)))

(defun nnheader-file-size (file)
  "Return the file size of FILE or 0."
  (or (file-attribute-size (file-attributes file)) 0))

(defun nnheader-find-etc-directory (package &optional file first)
  "Go through `load-path' and find the \"../etc/PACKAGE\" directory.
This function will look in the parent directory of each `load-path'
entry, and look for the \"etc\" directory there.
If FILE, find the \".../etc/PACKAGE\" file instead.
If FIRST is non-nil, return the directory or the file found at the
first.  Otherwise, find the newest one, though it may take a time."
  (let ((path load-path)
	dir results)
    ;; We try to find the dir by looking at the load path,
    ;; stripping away the last component and adding "etc/".
    (while path
      (if (and (car path)
	       (file-exists-p
		(setq dir (concat
			   (file-name-directory
			    (directory-file-name (car path)))
			   "etc/" package
			   (if file "" "/"))))
	       (or file (file-directory-p dir)))
	  (progn
	    (or (member dir results)
		(push dir results))
	    (setq path (if first nil (cdr path))))
	(setq path (cdr path))))
    (if (or first (not (cdr results)))
	(car results)
      (car (sort results #'file-newer-than-file-p)))))

(defvar ange-ftp-path-format)
(defvar efs-path-regexp)
(defun nnheader-re-read-dir (path)
  "Re-read directory PATH if PATH is on a remote system."
  (if (and (fboundp 'efs-re-read-dir) (boundp 'efs-path-regexp))
      (when (string-match efs-path-regexp path)
	(efs-re-read-dir path))
    (when (and (fboundp 'ange-ftp-re-read-dir) (boundp 'ange-ftp-path-format))
      (when (string-match (car ange-ftp-path-format) path)
	(ange-ftp-re-read-dir path)))))

(defun nnheader-insert-file-contents (filename &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but only reads in the file.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hook, etc.
  This function ensures that none of these modifications will take place."
  (let ((coding-system-for-read nnheader-file-coding-system))
    (mm-insert-file-contents filename visit beg end replace)))

(defun nnheader-insert-nov-file (file first)
  (let ((size (file-attribute-size (file-attributes file)))
	(cutoff (* 32 1024)))
    (when size
      (if (< size cutoff)
          ;; If the file is small, we just load it.
          (nnheader-insert-file-contents file)
        ;; We start on the assumption that FIRST is pretty recent.  If
        ;; not, we just insert the rest of the file as well.
        (let (current)
          (nnheader-insert-file-contents file nil (- size cutoff) size)
          (goto-char (point-min))
          (delete-region (point) (or (search-forward "\n" nil 'move) (point)))
          (setq current (ignore-errors (read (current-buffer))))
          (if (and (numberp current)
                   (< current first))
              t
            (delete-region (point-min) (point-max))
            (nnheader-insert-file-contents file)))))))

(defun nnheader-find-file-noselect (&rest args)
  "Open a file with some variables bound.
See `find-file-noselect' for the arguments."
  (cl-letf* ((format-alist nil)
             (auto-mode-alist (mm-auto-mode-alist))
             ((default-value 'major-mode) 'fundamental-mode)
             (enable-local-variables nil)
             (after-insert-file-functions nil)
             (enable-local-eval nil)
             (coding-system-for-read nnheader-file-coding-system)
             (version-control 'never)
	     (find-file-hook nil))
    (apply #'find-file-noselect args)))

(defun nnheader-directory-regular-files (dir)
  "Return a list of all regular files in DIR."
  (let ((files (directory-files dir t))
	out)
    (while files
      (when (file-regular-p (car files))
	(push (car files) out))
      (pop files))
    (nreverse out)))

(defun nnheader-directory-files (&rest args)
  "Same as `directory-files', but prune \".\" and \"..\"."
  (let ((files (apply #'directory-files args))
	out)
    (while files
      (unless (member (file-name-nondirectory (car files)) '("." ".."))
	(push (car files) out))
      (pop files))
    (nreverse out)))

(defmacro nnheader-skeleton-replace (from &optional to regexp)
  `(let ((new (generate-new-buffer " *nnheader replace*"))
	 (cur (current-buffer))
	 (start (point-min)))
     (set-buffer cur)
     (goto-char (point-min))
     (while (,(if regexp 're-search-forward 'search-forward)
	     ,from nil t)
       (insert-buffer-substring
	cur start (prog1 (match-beginning 0) (set-buffer new)))
       (goto-char (point-max))
       ,(when to `(insert ,to))
       (set-buffer cur)
       (setq start (point)))
     (insert-buffer-substring
      cur start (prog1 (point-max) (set-buffer new)))
     (copy-to-buffer cur (point-min) (point-max))
     (kill-buffer (current-buffer))
     (set-buffer cur)))

(defun nnheader-replace-string (from to)
  "Do a fast replacement of FROM to TO from point to `point-max'."
  (nnheader-skeleton-replace from to))

(defun nnheader-replace-regexp (from to)
  "Do a fast regexp replacement of FROM to TO from point to `point-max'."
  (nnheader-skeleton-replace from to t))

(defun nnheader-strip-cr ()
  "Strip all \r's from the current buffer."
  (nnheader-skeleton-replace "\r"))

(define-obsolete-function-alias 'nnheader-cancel-timer 'cancel-timer "27.1")
(define-obsolete-function-alias 'nnheader-cancel-function-timers
  'cancel-function-timers "27.1")

;; When changing this function, consider changing `pop3-accept-process-output'
;; as well.
(defun nnheader-accept-process-output (process)
  (accept-process-output process nnheader-read-timeout))

(defun nnheader-update-marks-actions (backend-marks actions)
  (dolist (action actions)
    (let ((range (nth 0 action))
	  (what  (nth 1 action))
	  (marks (nth 2 action)))
      (dolist (mark marks)
	(setq backend-marks
	      (gnus-update-alist-soft
	       mark
	       (cond
		((eq what 'add)
		 (gnus-range-add (cdr (assoc mark backend-marks)) range))
		((eq what 'del)
		 (gnus-remove-from-range
		  (cdr (assoc mark backend-marks)) range))
		((eq what 'set)
		 range))
	       backend-marks)))))
  backend-marks)

(defmacro nnheader-insert-buffer-substring (buffer &optional start end)
  "Copy string from unibyte buffer to multibyte current buffer."
  `(insert (with-current-buffer ,buffer
	     ,(if (or start end)
		  `(buffer-substring (or ,start (point-min))
				     (or ,end (point-max)))
		'(buffer-string)))))

(defvar nnheader-last-message-time '(0 0))
(defun nnheader-message-maybe (&rest args)
  (let ((now (current-time)))
    (when (time-less-p 1 (time-subtract now nnheader-last-message-time))
      (setq nnheader-last-message-time now)
      (apply #'nnheader-message args))))

(make-obsolete-variable 'nnheader-load-hook
                        "use `with-eval-after-load' instead." "28.1")
(run-hooks 'nnheader-load-hook)

(provide 'nnheader)

;;; nnheader.el ends here
