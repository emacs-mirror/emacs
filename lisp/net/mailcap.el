;;; mailcap.el --- MIME media types configuration -*- lexical-binding: t -*-

;; Copyright (C) 1998-2022 Free Software Foundation, Inc.

;; Author: William M. Perry <wmperry@aventail.com>
;;	Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news, mail, multimedia

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

;; Provides configuration of MIME media types from directly from Lisp
;; and via the usual mailcap mechanism (RFC 1524).  Deals with
;; mime.types similarly.

;;; Code:

(require 'cl-lib)
(autoload 'mail-header-parse-content-type "mail-parse")

(defgroup mailcap nil
  "Definition of viewers for MIME types."
  :version "21.1"
  :group 'mime)

(defcustom mailcap-prefer-mailcap-viewers t
  "If non-nil, prefer viewers specified in ~/.mailcap.
If nil, the most specific viewer will be chosen, even if there is
a general override in ~/.mailcap.  For instance, if /etc/mailcap
has an entry for \"image/gif\", that one will be chosen even if
you have an entry for \"image/*\" in your ~/.mailcap file."
  :type 'boolean)

(defvar mailcap-parse-args-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "\"" table)
    (modify-syntax-entry ?{ "(" table)
    (modify-syntax-entry ?} ")" table)
    table)
  "A syntax table for parsing SGML attributes.")

(defvar mailcap-print-command
  (mapconcat 'identity
	     (cons (if (boundp 'lpr-command)
		       lpr-command
		     "lpr")
		   (when (boundp 'lpr-switches)
		     (if (stringp lpr-switches)
			 (list lpr-switches)
		       lpr-switches)))
	     " ")
  "Shell command (including switches) used to print PostScript files.")

(defun mailcap--get-user-mime-data (sym)
  (let ((val (default-value sym))
	res)
    (dolist (entry val)
      (push (list (cdr (assq 'viewer entry))
                  (cdr (assq 'type entry))
                  (cdr (assq 'test entry)))
            res))
    (nreverse res)))

(defun mailcap--set-user-mime-data (sym val)
  (let (res)
    (pcase-dolist (`(,viewer ,type ,test) val)
      (push `((viewer . ,viewer)
              (type . ,type)
              ,@(when test `((test . ,test))))
            res))
    (set-default sym (nreverse res))))

(defcustom mailcap-user-mime-data nil
  "A list of viewers preferred for different MIME types.
The elements of the list are alists of the following structure

  ((viewer . VIEWER)
   (type   . MIME-TYPE)
   (test   . TEST))

where VIEWER is either a Lisp command, e.g., a major mode, or a
string containing a shell command for viewing files of the
defined MIME-TYPE.  In case of a shell command, %s will be
replaced with the file.

MIME-TYPE is a regular expression being matched against the
actual MIME type.  It is implicitly surrounded with ^ and $.

TEST is a Lisp form which is evaluated in order to test if the
entry should be chosen.  The `test' entry is optional.

When selecting a viewer for a given MIME type, the first viewer
in this list with a matching MIME-TYPE and successful TEST is
selected.  Only if none matches, the standard `mailcap-mime-data'
is consulted."
  :version "26.1"
  :type '(repeat
	  (list
	   (choice (function :tag "Function or mode")
		   (string :tag "Shell command"))
	   (regexp :tag "MIME Type")
	   (sexp :tag "Test (optional)")))
  :get #'mailcap--get-user-mime-data
  :set #'mailcap--set-user-mime-data
  :group 'mailcap)

;; Postpone using defcustom for this as it's so big and we essentially
;; have to have two copies of the data around then.  Perhaps just
;; customize the Lisp viewers and rely on the normal configuration
;; files for the rest?  -- fx
(defvar mailcap-mime-data
  `(("application"
     ("vnd\\.ms-excel"
      (viewer . "gnumeric %s")
      (test   . (getenv "DISPLAY"))
      (type . "application/vnd.ms-excel"))
     ("octet-stream"
      (viewer . mailcap-save-binary-file)
      (non-viewer . t)
      (type . "application/octet-stream"))
     ("dvi"
      (viewer . "xdvi -safer %s")
      (test   . (eq window-system 'x))
      ("needsx11")
      (type   . "application/dvi")
      ("print" . "dvips -qRP %s"))
     ("dvi"
      (viewer . "dvitty %s")
      (test   . (not (getenv "DISPLAY")))
      (type   . "application/dvi")
      ("print" . "dvips -qRP %s"))
     ("emacs-lisp"
      (viewer . mailcap-maybe-eval)
      (type   . "application/emacs-lisp"))
     ("x-emacs-lisp"
      (viewer . mailcap-maybe-eval)
      (type   . "application/x-emacs-lisp"))
     ("x-tar"
      (viewer . mailcap-save-binary-file)
      (non-viewer . t)
      (type   . "application/x-tar"))
     ("x-latex"
      (viewer . tex-mode)
      (type   . "application/x-latex"))
     ("x-tex"
      (viewer . tex-mode)
      (type   . "application/x-tex"))
     ("latex"
      (viewer . tex-mode)
      (type   . "application/latex"))
     ("tex"
      (viewer . tex-mode)
      (type   . "application/tex"))
     ("texinfo"
      (viewer . texinfo-mode)
      (type   . "application/tex"))
     ("zip"
      (viewer . mailcap-save-binary-file)
      (non-viewer . t)
      (type   . "application/zip")
      ("copiousoutput"))
     ("pdf"
      (viewer . doc-view-mode)
      (type . "application/pdf")
      (test . window-system))
     ("pdf"
      (viewer . pdf-view-mode)
      (type . "application/pdf")
      (test . window-system))
     ("pdf"
      (viewer . "gv -safer %s")
      (type . "application/pdf")
      (test . window-system)
      ("print" . ,(concat "pdf2ps %s - | " mailcap-print-command)))
     ("pdf"
      (viewer . "gpdf %s")
      (type . "application/pdf")
      ("print" . ,(concat "pdftops %s - | " mailcap-print-command))
      (test . (eq window-system 'x)))
     ("pdf"
      (viewer . "xpdf %s")
      (type . "application/pdf")
      ("print" . ,(concat "pdftops %s - | " mailcap-print-command))
      (test . (eq window-system 'x)))
     ("pdf"
      (viewer . ,(concat "pdftotext %s -"))
      (type   . "application/pdf")
      ("print" . ,(concat "pdftops %s - | " mailcap-print-command))
      ("copiousoutput"))
     ("postscript"
      (viewer . "gv -safer %s")
      (type . "application/postscript")
      (test   . window-system)
      ("print" . ,(concat mailcap-print-command " %s"))
      ("needsx11"))
     ("postscript"
      (viewer . "ghostview -dSAFER %s")
      (type . "application/postscript")
      (test   . (eq window-system 'x))
      ("print" . ,(concat mailcap-print-command " %s"))
      ("needsx11"))
     ("postscript"
      (viewer . "ps2ascii %s")
      (type . "application/postscript")
      (test . (not (getenv "DISPLAY")))
      ("print" . ,(concat mailcap-print-command " %s"))
      ("copiousoutput"))
     ("sieve"
      (viewer . sieve-mode)
      (type   . "application/sieve"))
     ("pgp-keys"
      (viewer . "gpg --import --interactive --verbose")
      (type   . "application/pgp-keys")
      ("needsterminal")))
    ("audio"
     ("x-mpeg"
      (viewer . "maplay %s")
      (type   . "audio/x-mpeg"))
     (".*"
      (viewer . "showaudio")
      (type   . "audio/*")))
    ("message"
     ("rfc-*822"
      (viewer . mm-view-message)
      (test   . (and (featurep 'gnus)
		     (gnus-alive-p)))
      (type   . "message/rfc822"))
     ("rfc-*822"
      (viewer . vm-mode)
      (type   . "message/rfc822"))
     ("rfc-*822"
      (viewer . view-mode)
      (type   . "message/rfc822")))
    ("image"
     ("x-xwd"
      (viewer  . "xwud -in %s")
      (type    . "image/x-xwd")
      ("compose" . "xwd -frame > %s")
      (test    . (eq window-system 'x))
      ("needsx11"))
     ("x11-dump"
      (viewer . "xwud -in %s")
      (type . "image/x-xwd")
      ("compose" . "xwd -frame > %s")
      (test   . (eq window-system 'x))
      ("needsx11"))
     ("windowdump"
      (viewer . "xwud -in %s")
      (type . "image/x-xwd")
      ("compose" . "xwd -frame > %s")
      (test   . (eq window-system 'x))
      ("needsx11"))
     (".*"
      (viewer . "display %s")
      (type . "image/*")
      (test   . (eq window-system 'x))
      ("needsx11")))
    ("text"
     ("plain"
      (viewer  . view-mode)
      (type    . "text/plain"))
     ("plain"
      (viewer  . fundamental-mode)
      (type    . "text/plain"))
     ("enriched"
      (viewer . enriched-decode)
      (type   . "text/enriched"))
     ("dns"
      (viewer . dns-mode)
      (type   . "text/dns")))
    ("video"
     ("mpeg"
      (viewer . "mpeg_play %s")
      (type   . "video/mpeg")
      (test   . (eq window-system 'x))
      ("needsx11")))
    ("x-world"
     ("x-vrml"
      (viewer  . "webspace -remote %s -URL %u")
      (type    . "x-world/x-vrml")
      ("description"
       "VRML document")))
    ("archive"
     ("tar"
      (viewer . tar-mode)
      (type . "archive/tar"))))
  "The mailcap structure is an assoc list of assoc lists.
1st assoc list is keyed on the major content-type
2nd assoc list is keyed on the minor content-type (which can be a regexp)

Which looks like:
-----------------
 ((\"application\"
   (\"postscript\" . <info>))
  (\"text\"
   (\"plain\" . <info>)))

Where <info> is another assoc list of the various information
related to the mailcap RFC 1524.  This is keyed on the lowercase
attribute name (viewer, test, etc).  This looks like:
 ((viewer . VIEWERINFO)
  (test   . TESTINFO)
  (xxxx   . \"STRING\")
  FLAG)

Where VIEWERINFO specifies how the content-type is viewed.  Can be
a string, in which case it is run through a shell, with appropriate
parameters, or a symbol, in which case the symbol is `funcall'ed if
and only if it exists as a function, with the buffer as an argument.

TESTINFO is a test for the viewer's applicability, or nil.  If nil, it
means the viewer is always valid.  If it is a Lisp function, it is
called with a list of items from any extra fields from the
Content-Type header as argument to return a boolean value for the
validity.  Otherwise, if it is a non-function Lisp symbol or list
whose car is a symbol, it is `eval'uated to yield the validity.  If it
is a string or list of strings, it represents a shell command to run
to return a true or false shell value for the validity.

The last matching entry in this structure takes precedence over
preceding entries.")
(put 'mailcap-mime-data 'risky-local-variable t)

(defvar mailcap--computed-mime-data nil
  "Computed version of the mailcap data incorporating all sources.
Same format as `mailcap-mime-data'.")

(defcustom mailcap-download-directory nil
  "Directory to which `mailcap-save-binary-file' downloads files by default.
nil means your home directory."
  :type '(choice (const :tag "Home directory" nil)
		 directory)
  :group 'mailcap)

(defvar mailcap-poor-system-types
  '(ms-dos windows-nt)
  "Systems that don't have a Unix-like directory hierarchy.")

;;;
;;; Utility functions
;;;

(defun mailcap-save-binary-file ()
  (goto-char (point-min))
  (unwind-protect
      (let ((file (read-file-name
		   "Filename to save as: "
		   (or mailcap-download-directory "~/")))
	    (require-final-newline nil))
	(write-region (point-min) (point-max) file))
    (kill-buffer (current-buffer))))

(defvar mailcap-maybe-eval-warning
  "*** WARNING ***

This MIME part contains untrusted and possibly harmful content.
If you evaluate the Emacs Lisp code contained in it, a lot of nasty
things can happen.  Please examine the code very carefully before you
instruct Emacs to evaluate it.  You can browse the buffer containing
the code using \\[scroll-other-window].

If you are unsure what to do, please answer \"no\"."
  "Text of warning message displayed by `mailcap-maybe-eval'.
Make sure that this text consists only of few text lines.  Otherwise,
Gnus might fail to display all of it.")

(defun mailcap-maybe-eval ()
  "Maybe evaluate a buffer of Emacs Lisp code."
  (let ((lisp-buffer (current-buffer)))
    (goto-char (point-min))
    (when
	(save-window-excursion
	  (delete-other-windows)
          (let ((buffer (generate-new-buffer "*Warning*")))
	    (unwind-protect
		(with-current-buffer buffer
		  (insert (substitute-command-keys
			   mailcap-maybe-eval-warning))
		  (goto-char (point-min))
		  (display-buffer buffer)
		  (yes-or-no-p "This is potentially dangerous emacs-lisp code, evaluate it? "))
	      (kill-buffer buffer))))
      (eval-buffer (current-buffer)))
    (when (buffer-live-p lisp-buffer)
      (with-current-buffer lisp-buffer
	(emacs-lisp-mode)))))


;;;
;;; The mailcap parser
;;;

(defun mailcap-replace-regexp (regexp to-string)
  ;; Quiet replace-regexp.
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match to-string t nil)))

(defvar mailcap-parsed-p nil)

(defun mailcap-parse-mailcaps (&optional path force)
  "Parse out all the mailcaps specified in a path string PATH.
Components of PATH are separated by the `path-separator' character
appropriate for this system.  If FORCE, re-parse even if already
parsed.  If PATH is omitted, use the value of environment variable
MAILCAPS if set; otherwise (on Unix) use the path from RFC 1524, plus
/usr/local/etc/mailcap."
  (interactive (list nil t))
  (when (or (not mailcap-parsed-p)
	    force)
    ;; Clear out all old data.
    (setq mailcap--computed-mime-data nil)
    ;; Add the Emacs-distributed defaults (which will be used as
    ;; fallbacks).  Do it this way instead of just copying the list,
    ;; since entries are destructively modified.
    (cl-loop for (major . minors) in mailcap-mime-data
             do (cl-loop for (minor . entry) in minors
                         do (mailcap-add-mailcap-entry major minor entry)))
    (cond
     (path nil)
     ((getenv "MAILCAPS")
      (setq path (getenv "MAILCAPS")))
     ((memq system-type mailcap-poor-system-types)
      (setq path '(("~/.mailcap" user)
                   ("~/mail.cap" user)
                   ("~/etc/mail.cap" user))))
     (t
      (setq path
	    ;; This is per RFC 1524, specifically with /usr before
	    ;; /usr/local.
	    '(("~/.mailcap" user)
              ("/etc/mailcap" system)
              ("/usr/etc/mailcap" system)
	      ("/usr/local/etc/mailcap" system)))))
    ;; The ~/.mailcap entries will end up first in the resulting data.
    (dolist (spec (reverse
                   (if (stringp path)
                       (split-string path path-separator t)
                     path)))
      (let ((source (and (consp spec) (cadr spec)))
            (file-name (if (stringp spec)
                           spec
                         (car spec))))
        (when (and (file-readable-p file-name)
                   (file-regular-p file-name))
          (mailcap-parse-mailcap file-name source))))
    (setq mailcap-parsed-p t)))

(defun mailcap-parse-mailcap (fname &optional source)
  "Parse out the mailcap file specified by FNAME.
If SOURCE, mark the entry with this as the source."
  (let (major				; The major mime type (image/audio/etc)
	minor				; The minor mime type (gif, basic, etc)
	save-pos			; Misc saved positions used in parsing
	viewer				; How to view this mime type
	info				; Misc info about this mime type
	)
    (with-temp-buffer
      (insert-file-contents fname)
      (set-syntax-table mailcap-parse-args-syntax-table)
      (mailcap-replace-regexp "#.*" "")	; Remove all comments
      (mailcap-replace-regexp "\\\\[ \t]*\n" " ") ; And collapse spaces
      (mailcap-replace-regexp "\n+" "\n") ; And blank lines
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (while (not (bobp))
	(skip-chars-backward " \t\n")
	(beginning-of-line)
	(setq save-pos (point)
	      info nil)
	(skip-chars-forward "^/; \t\n")
	(downcase-region save-pos (point))
	(setq major (buffer-substring save-pos (point)))
	(skip-chars-forward " \t")
	(setq minor "")
	(when (eq (char-after) ?/)
	  (forward-char)
	  (skip-chars-forward " \t")
	  (setq save-pos (point))
	  (skip-chars-forward "^; \t\n")
	  (downcase-region save-pos (point))
	  (setq minor
		(cond
		 ((eq ?* (or (char-after save-pos) 0)) ".*")
		 ((= (point) save-pos) ".*")
		 (t (regexp-quote (buffer-substring save-pos (point)))))))
	(skip-chars-forward " \t")
	;;; Got the major/minor chunks, now for the viewers/etc
	;;; The first item _must_ be a viewer, according to the
	;;; RFC for mailcap files (#1524)
	(setq viewer "")
	(when (eq (char-after) ?\;)
	  (forward-char)
	  (skip-chars-forward " \t")
	  (setq save-pos (point))
	  (skip-chars-forward "^;\n")
	  ;; skip \;
	  (while (eq (char-before) ?\\)
	    (backward-delete-char 1)
	    (forward-char)
	    (skip-chars-forward "^;\n"))
	  (if (eq (or (char-after save-pos) 0) ?')
	      (setq viewer (progn
			     (narrow-to-region (1+ save-pos) (point))
			     (goto-char (point-min))
			     (prog1
				 (read (current-buffer))
			       (goto-char (point-max))
			       (widen))))
	    (setq viewer (buffer-substring save-pos (point)))))
	(setq save-pos (point))
	(end-of-line)
	(unless (equal viewer "")
	  (setq info (nconc (list (cons 'viewer viewer)
				  (cons 'type (concat major "/"
						      (if (string= minor ".*")
							  "*" minor))))
			    (mailcap-parse-mailcap-extras save-pos (point))))
	  (mailcap-mailcap-entry-passes-test info)
          ;; Record where the data came from.
          (when source
            (setq info (nconc info (list (cons 'source source)))))
	  (mailcap-add-mailcap-entry major minor info))
	(beginning-of-line)))))

(defun mailcap-parse-mailcap-extras (st nd)
  "Grab all the extra stuff from a mailcap entry."
  (let (
	name				; From name=
	value				; its value
	results				; Assoc list of results
	name-pos			; Start of XXXX= position
	val-pos				; Start of value position
	done				; Found end of \'d ;s?
	)
    (save-restriction
      (narrow-to-region st nd)
      (goto-char (point-min))
      (skip-chars-forward " \n\t;")
      (while (not (eobp))
	(setq done nil)
	(setq name-pos (point))
	(skip-chars-forward "^ \n\t=;")
	(downcase-region name-pos (point))
	(setq name (buffer-substring name-pos (point)))
	(skip-chars-forward " \t\n")
	(if (not (eq (char-after (point)) ?=)) ; There is no value
	    (setq value t)
	  (skip-chars-forward " \t\n=")
	  (setq val-pos (point))
	  (if (memq (char-after val-pos) '(?\" ?'))
	      (progn
		(setq val-pos (1+ val-pos))
		(condition-case nil
		    (progn
		      (forward-sexp 1)
		      (backward-char 1))
		  (error (goto-char (point-max)))))
	    (while (not done)
	      (skip-chars-forward "^;")
	      (if (eq (char-after (1- (point))) ?\\ )
		  (progn
		    (subst-char-in-region (1- (point)) (point) ?\\ ? )
		    (skip-chars-forward ";"))
		(setq done t))))
	  (setq	value (buffer-substring val-pos (point))))
	;; `test' as symbol, others like "copiousoutput" and "needsx11" as
	;; strings
	(push (cons (if (string-equal name "test") 'test name) value) results)
	(skip-chars-forward " \";\n\t"))
      results)))

(defun mailcap-mailcap-entry-passes-test (info)
  "Replace the test clause of INFO itself with a boolean for some cases.
This function supports only `test -n $DISPLAY' and `test -z $DISPLAY',
replaces them with t or nil.  As for others or if INFO has an interactive
spec (needsterm, needsterminal, or needsx11) but DISPLAY is not set,
the test clause will be unchanged."
  (let ((test (assq 'test info))	; The test clause
	status)
    (setq status (and test (split-string (cdr test) " ")))
    (if (and (or (assoc "needsterm" info)
		 (assoc "needsterminal" info)
		 (assoc "needsx11" info))
	     (not (getenv "DISPLAY")))
	(setq status nil)
      (cond
       ((and (equal (nth 0 status) "test")
	     (equal (nth 1 status) "-n")
	     (or (equal (nth 2 status) "$DISPLAY")
		 (equal (nth 2 status) "\"$DISPLAY\"")))
	(setq status (if (getenv "DISPLAY") t nil)))
       ((and (equal (nth 0 status) "test")
	     (equal (nth 1 status) "-z")
	     (or (equal (nth 2 status) "$DISPLAY")
		 (equal (nth 2 status) "\"$DISPLAY\"")))
	(setq status (if (getenv "DISPLAY") nil t)))
       (test nil)
       (t nil)))
    (and test (listp test) (setcdr test status))))

;;;
;;; The action routines.
;;;

(defun mailcap-possible-viewers (major minor)
  "Return a list of possible viewers from MAJOR for minor type MINOR."
  (let ((result nil))
    (pcase-dolist (`(,type . ,attrs) major)
      (when (or (equal type minor)
                (and minor
                     (string-match (concat "^" type "$") minor)))
	(push attrs result)))
    (nreverse result)))

(defun mailcap-unescape-mime-test (test type-info)
  (let (save-pos save-chr subst)
    (cond
     ((symbolp test) test)
     ((and (listp test) (symbolp (car test))) test)
     ((or (stringp test)
	  (and (listp test) (stringp (car test))
	       (setq test (mapconcat 'identity test " "))))
      (with-temp-buffer
	(insert test)
	(goto-char (point-min))
	(while (not (eobp))
	  (skip-chars-forward "^%")
	  (if (/= (- (point)
		     (progn (skip-chars-backward "\\\\")
			    (point)))
		  0)			; It is an escaped %
	      (progn
		(delete-char 1)
		(skip-chars-forward "%."))
	    (setq save-pos (point))
	    (skip-chars-forward "%")
	    (setq save-chr (char-after (point)))
	    ;; Escapes:
	    ;; %s: name of a file for the body data
	    ;; %t: content-type
	    ;; %{<parameter name}: value of parameter in mailcap entry
	    ;; %n: number of sub-parts for multipart content-type
	    ;; %F: a set of content-type/filename pairs for multiparts
	    (cond
	     ((null save-chr) nil)
	     ((= save-chr ?t)
	      (delete-region save-pos (progn (forward-char 1) (point)))
	      (insert (or (cdr (assq 'type type-info)) "\"\"")))
	     ((memq save-chr '(?M ?n ?F))
	      (delete-region save-pos (progn (forward-char 1) (point)))
	      (insert "\"\""))
	     ((= save-chr ?{)
	      (forward-char 1)
	      (skip-chars-forward "^}")
	      (downcase-region (+ 2 save-pos) (point))
	      (setq subst (buffer-substring (+ 2 save-pos) (point)))
	      (delete-region save-pos (1+ (point)))
	      (insert (or (cdr (assoc subst type-info)) "\"\"")))
	     (t nil))))
	(buffer-string)))
     (t (error "Bad value to mailcap-unescape-mime-test: %s" test)))))

(defvar mailcap-viewer-test-cache nil)

(defun mailcap-viewer-passes-test (viewer-info type-info)
  "Return non-nil if viewer specified by VIEWER-INFO passes its test clause.
Also return non-nil if it has no test clause.  TYPE-INFO is an argument
to supply to the test."
  (let* ((test-info (assq 'test viewer-info))
	 (test (cdr test-info))
	 (otest test)
	 (viewer (cdr (assq 'viewer viewer-info)))
	 (default-directory (expand-file-name "~/"))
	 status cache result)
    (cond ((not (or (stringp viewer) (fboundp viewer)))
	   nil)				; Non-existent Lisp function
	  ((setq cache (assoc test mailcap-viewer-test-cache))
	   (cadr cache))
	  ((not test-info) t)		; No test clause
	  (t
	   (setq
	    result
	    (cond
	     ((not test) nil)		; Already failed test
	     ((eq test t) t)		; Already passed test
	     ((functionp test)		; Lisp function as test
	      (funcall test type-info))
	     ((and (symbolp test)	; Lisp variable as test
		   (boundp test))
	      (symbol-value test))
	     ((and (listp test)		; List to be eval'd
		   (symbolp (car test)))
	      (eval test))
	     (t
	      (setq test (mailcap-unescape-mime-test test type-info)
		    test (list shell-file-name nil nil nil
			       shell-command-switch test)
		    status (apply 'call-process test))
	      (eq 0 status))))
	   (push (list otest result) mailcap-viewer-test-cache)
	   result))))

(defun mailcap-add-mailcap-entry (major minor info &optional storage)
  (let* ((storage (or storage 'mailcap--computed-mime-data))
         (old-major (assoc major (symbol-value storage))))
    (if (null old-major)		; New major area
        (set storage
             (cons (cons major (list (cons minor info)))
                   (symbol-value storage)))
      (let ((cur-minor (assoc minor old-major)))
	(cond
	 ((or (null cur-minor)		; New minor area, or
	      (assq 'test info))	; Has a test, insert at beginning
	  (setcdr old-major
                  (cons (cons minor info) (cdr old-major))))
	 ((and (not (assq 'test info))	; No test info, replace completely
	       (not (assq 'test cur-minor))
	       (equal (assq 'viewer info)  ; Keep alternative viewer
		      (assq 'viewer cur-minor)))
	  (setcdr cur-minor info))
	 (t
	  (setcdr old-major
                  (setcdr old-major
                          (cons (cons minor info) (cdr old-major))))))))))

(defun mailcap-add (type viewer &optional test)
  "Add VIEWER as a handler for TYPE.
If TEST is not given, it defaults to t."
  (let ((tl (split-string type "/")))
    (when (or (not (car tl))
	      (not (cadr tl)))
      (error "%s is not a valid MIME type" type))
    (let ((entry
           `((viewer . ,viewer)
             (test . ,(if test test t))
             (type . ,type))))
      ;; Store it.
      (mailcap-add-mailcap-entry (car tl) (cadr tl) entry
                                 'mailcap-user-mime-data)
      ;; Make it available for usage.
      (mailcap-add-mailcap-entry (car tl) (cadr tl) entry))))

;;;
;;; The main whabbo
;;;

(defun mailcap-viewer-lessp (x y)
  "Return t if viewer X is more desirable than viewer Y."
  (let ((x-wild (string-match "[*?]" (or (cdr-safe (assq 'type x)) "")))
	(y-wild (string-match "[*?]" (or (cdr-safe (assq 'type y)) "")))
	(x-lisp (not (stringp (or (cdr-safe (assq 'viewer x)) ""))))
	(y-lisp (not (stringp (or (cdr-safe (assq 'viewer y)) "")))))
    (cond
     ((and x-wild (not y-wild))
      nil)
     ((and (not x-wild) y-wild)
      t)
     ((and (not y-lisp) x-lisp)
      t)
     (t nil))))

(defun mailcap-select-preferred-viewer (type-info)
  "Return an applicable viewer entry from `mailcap-user-mime-data'."
  (let ((info (mapcar (lambda (a) (cons (symbol-name (car a))
                                   (cdr a)))
                      (cdr type-info)))
        viewer)
    (dolist (entry mailcap-user-mime-data)
      (when (and (null viewer)
                 (string-match (concat "^" (cdr (assq 'type entry)) "$")
                               (car type-info))
                 (mailcap-viewer-passes-test entry info))
        (setq viewer entry)))
    viewer))

(defun mailcap-mime-info (string &optional request no-decode)
  "Get the MIME viewer command for STRING, return nil if none found.
Expects a complete content-type header line as its argument.

Second argument REQUEST specifies what information to return.  If it is
nil or the empty string, the viewer (second field of the mailcap
entry) will be returned.  If it is a string, then the mailcap field
corresponding to that string will be returned (print, description,
whatever).  If a number, then all the information for this specific
viewer is returned.  If `all', then all possible viewers for
this type is returned.

If NO-DECODE is non-nil, don't decode STRING."
  ;; NO-DECODE avoids calling `mail-header-parse-content-type' from
  ;; `mail-parse.el'
  (let (
	major              ; Major encoding (text, etc)
	minor              ; Minor encoding (html, etc)
	info               ; Other info
	major-info         ; (assoc major mailcap--computed-mime-data)
	viewers            ; Possible viewers
	passed             ; Viewers that passed the test
	viewer             ; The one and only viewer
	ctl)
    (save-excursion
      (setq ctl
	    (if no-decode
		(list (or string "text/plain"))
	      (mail-header-parse-content-type (or string "text/plain"))))
      ;; Check if there's a user-defined viewer from `mailcap-user-mime-data'.
      (setq viewer (mailcap-select-preferred-viewer ctl))
      (if viewer
          (setq passed (list viewer))
        ;; None found, so heuristically select some applicable viewer
        ;; from `mailcap--computed-mime-data'.
        (mailcap-parse-mailcaps nil t)
        (setq major (split-string (car ctl) "/"))
        (setq minor (cadr major)
              major (car major))
        (when (setq major-info (cdr (assoc major mailcap--computed-mime-data)))
          (when (setq viewers (mailcap-possible-viewers major-info minor))
            (setq info (mapcar (lambda (a)
                                 (cons (symbol-name (car a)) (cdr a)))
                               (cdr ctl)))
            (dolist (entry viewers)
              (when (mailcap-viewer-passes-test entry info)
                (push entry passed)))
            (setq passed (sort (nreverse passed) 'mailcap-viewer-lessp))
            ;; When we want to prefer entries from the user's
            ;; ~/.mailcap file, then we filter out the system entries
            ;; and see whether we have anything left.
            (when mailcap-prefer-mailcap-viewers
              (when-let ((user-entries
                          (seq-filter (lambda (elem)
                                        (eq (cdr (assq 'source elem)) 'user))
                                      passed)))
                (setq passed user-entries)))
            (setq viewer (car passed))))
        (when (and (stringp (cdr (assq 'viewer viewer)))
                   passed)
          (setq viewer (car passed))))
      (cond
       ((and (null viewer) (not (equal major "default")) request)
        (mailcap-mime-info "default" request no-decode))
       ((or (null request) (equal request ""))
        (mailcap-unescape-mime-test (cdr (assq 'viewer viewer)) info))
       ((stringp request)
        (mailcap-unescape-mime-test
         (cdr-safe (assoc request viewer)) info))
       ((eq request 'all)
        passed)
       (t
        ;; MUST make a copy *sigh*, else we modify mailcap--computed-mime-data
        (setq viewer (copy-sequence viewer))
        (let ((view (assq 'viewer viewer))
              (test (assq 'test viewer)))
          (if view (setcdr view (mailcap-unescape-mime-test (cdr view) info)))
          (if test (setcdr test (mailcap-unescape-mime-test (cdr test) info))))
        viewer)))))

;;;
;;; Experimental MIME-types parsing
;;;

(defvar mailcap-mime-extensions
  '((""       . "text/plain")
    (".1"     . "text/plain")  ;; Manual pages
    (".3"     . "text/plain")
    (".8"     . "text/plain")
    (".abs"   . "audio/x-mpeg")
    (".aif"   . "audio/aiff")
    (".aifc"  . "audio/aiff")
    (".aiff"  . "audio/aiff")
    (".ano"   . "application/x-annotator")
    (".au"    . "audio/ulaw")
    (".avi"   . "video/x-msvideo")
    (".bcpio" . "application/x-bcpio")
    (".bin"   . "application/octet-stream")
    (".cdf"   . "application/x-netcdr")
    (".cpio"  . "application/x-cpio")
    (".csh"   . "application/x-csh")
    (".css"   . "text/css")
    (".dvi"   . "application/x-dvi")
    (".diff"  . "text/x-patch")
    (".dpatch". "text/x-patch")
    (".el"    . "application/emacs-lisp")
    (".eps"   . "application/postscript")
    (".etx"   . "text/x-setext")
    (".exe"   . "application/octet-stream")
    (".fax"   . "image/x-fax")
    (".gif"   . "image/gif")
    (".hdf"   . "application/x-hdf")
    (".hqx"   . "application/mac-binhex40")
    (".htm"   . "text/html")
    (".html"  . "text/html")
    (".icon"  . "image/x-icon")
    (".ief"   . "image/ief")
    (".jpg"   . "image/jpeg")
    (".macp"  . "image/x-macpaint")
    (".man"   . "application/x-troff-man")
    (".me"    . "application/x-troff-me")
    (".mif"   . "application/mif")
    (".mov"   . "video/quicktime")
    (".movie" . "video/x-sgi-movie")
    (".mp2"   . "audio/x-mpeg")
    (".mp3"   . "audio/x-mpeg")
    (".mp2a"  . "audio/x-mpeg2")
    (".mpa"   . "audio/x-mpeg")
    (".mpa2"  . "audio/x-mpeg2")
    (".mpe"   . "video/mpeg")
    (".mpeg"  . "video/mpeg")
    (".mpega" . "audio/x-mpeg")
    (".mpegv" . "video/mpeg")
    (".mpg"   . "video/mpeg")
    (".mpv"   . "video/mpeg")
    (".ms"    . "application/x-troff-ms")
    (".nc"    . "application/x-netcdf")
    (".nc"    . "application/x-netcdf")
    (".oda"   . "application/oda")
    (".patch" . "text/x-patch")
    (".pbm"   . "image/x-portable-bitmap")
    (".pdf"   . "application/pdf")
    (".pgm"   . "image/portable-graymap")
    (".pict"  . "image/pict")
    (".png"   . "image/png")
    (".pnm"   . "image/x-portable-anymap")
    (".pod"   . "text/plain")
    (".ppm"   . "image/portable-pixmap")
    (".ps"    . "application/postscript")
    (".qt"    . "video/quicktime")
    (".ras"   . "image/x-raster")
    (".rgb"   . "image/x-rgb")
    (".rtf"   . "application/rtf")
    (".rtx"   . "text/richtext")
    (".sh"    . "application/x-sh")
    (".sit"   . "application/x-stuffit")
    (".siv"   . "application/sieve")
    (".snd"   . "audio/basic")
    (".soa"   . "text/dns")
    (".src"   . "application/x-wais-source")
    (".tar"   . "archive/tar")
    (".tcl"   . "application/x-tcl")
    (".tex"   . "application/x-tex")
    (".texi"  . "application/texinfo")
    (".tga"   . "image/x-targa")
    (".tif"   . "image/tiff")
    (".tiff"  . "image/tiff")
    (".tr"    . "application/x-troff")
    (".troff" . "application/x-troff")
    (".tsv"   . "text/tab-separated-values")
    (".txt"   . "text/plain")
    (".vbs"   . "video/mpeg")
    (".vox"   . "audio/basic")
    (".vrml"  . "x-world/x-vrml")
    (".wav"   . "audio/x-wav")
    (".xls"   . "application/vnd.ms-excel")
    (".wrl"   . "x-world/x-vrml")
    (".xbm"   . "image/xbm")
    (".xpm"   . "image/xpm")
    (".xwd"   . "image/windowdump")
    (".zip"   . "application/zip")
    (".ai"    . "application/postscript")
    (".jpe"   . "image/jpeg")
    (".jpeg"  . "image/jpeg")
    (".org"   . "text/x-org"))
  "An alist of file extensions and corresponding MIME content-types.
This exists for you to customize the information in Lisp.  It is
merged with values from mailcap files by `mailcap-parse-mimetypes'.")

(defvar mailcap-mimetypes-parsed-p nil)

(defun mailcap-parse-mimetypes (&optional path force)
  "Parse out all the mimetypes specified in a Unix-style path string PATH.
Components of PATH are separated by the `path-separator' character
appropriate for this system.  If PATH is omitted, use the value of
environment variable MIMETYPES if set; otherwise use a default path.
If FORCE, re-parse even if already parsed."
  (interactive (list nil t))
  (when (or (not mailcap-mimetypes-parsed-p)
	    force)
    (cond
     (path nil)
     ((getenv "MIMETYPES") (setq path (getenv "MIMETYPES")))
     ((memq system-type mailcap-poor-system-types)
      (setq path '("~/mime.typ" "~/etc/mime.typ")))
     (t (setq path
	      ;; mime.types seems to be the normal name, definitely so
	      ;; on current GNUish systems.  The search order follows
	      ;; that for mailcap.
	      '("~/.mime.types"
		"/etc/mime.types"
		"/usr/etc/mime.types"
		"/usr/local/etc/mime.types"
		"/usr/local/www/conf/mime.types"
		"~/.mime-types"
		"/etc/mime-types"
		"/usr/etc/mime-types"
		"/usr/local/etc/mime-types"
		"/usr/local/www/conf/mime-types"))))
    (dolist (fname (reverse (if (stringp path)
                                (split-string path path-separator t)
                              path)))
      (when (file-readable-p fname)
        (mailcap-parse-mimetype-file fname)))
    (setq mailcap-mimetypes-parsed-p t)))

(defun mailcap-parse-mimetype-file (fname)
  "Parse out a mime-types file FNAME."
  (let (type				; The MIME type for this line
	extns				; The extensions for this line
	save-pos			; Misc. saved buffer positions
        save-extn)
    (with-temp-buffer
      (insert-file-contents fname)
      (mailcap-replace-regexp "#.*" "")
      (mailcap-replace-regexp "\n+" "\n")
      (mailcap-replace-regexp "[ \t]+$" "")
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
	(skip-chars-forward " \t\n")
	(setq save-pos (point))
	(skip-chars-forward "^ \t\n")
	(downcase-region save-pos (point))
	(setq type (buffer-substring save-pos (point)))
	(while (not (eolp))
	  (skip-chars-forward " \t")
	  (setq save-pos (point))
	  (skip-chars-forward "^ \t\n")
          (setq save-extn (buffer-substring save-pos (point)))
	  (push (cons (if (= (string-to-char save-extn) ?.)
		          save-extn (concat "." save-extn))
                      type)
                extns))
        (setq mailcap-mime-extensions (append extns mailcap-mime-extensions)
              extns nil)))))

(defun mailcap-extension-to-mime (extn)
  "Return the MIME content type of the file extensions EXTN."
  (mailcap-parse-mimetypes)
  (if (and (stringp extn)
	   (not (eq (string-to-char extn) ?.)))
      (setq extn (concat "." extn)))
  (cdr (assoc (downcase extn) mailcap-mime-extensions)))

(defun mailcap-file-name-to-mime-type (file-name)
  "Return the MIME content type based on the FILE-NAME's extension.
For instance, \"foo.png\" will result in \"image/png\"."
  (mailcap-extension-to-mime
   (if (string-match "\\(\\.[^.]+\\)\\'" file-name)
       (match-string 1 file-name)
     "")))

(defun mailcap-mime-types ()
  "Return a list of MIME media types."
  (mailcap-parse-mimetypes)
  (delete-dups
   (nconc
    (mapcar 'cdr mailcap-mime-extensions)
    (let (res type)
      (dolist (data mailcap--computed-mime-data)
        (dolist (info (cdr data))
          (setq type (cdr (assq 'type (cdr info))))
          (unless (string-search "*" type)
            (push type res))))
      (nreverse res)))))

;;;
;;; Useful supplementary functions
;;;

(defun mailcap-file-default-commands (files)
  "Return a list of default commands for FILES."
  (mailcap-parse-mailcaps)
  (mailcap-parse-mimetypes)
  (let* ((all-mime-type
	  ;; All unique MIME types from file extensions
	  (delete-dups
	   (mapcar (lambda (file)
		     (mailcap-extension-to-mime
		      (file-name-extension file t)))
		   files)))
	 (all-mime-info
	  ;; All MIME info lists
	  (delete-dups
	   (mapcar (lambda (mime-type)
		     (mailcap-mime-info mime-type 'all))
		   all-mime-type)))
	 (common-mime-info
	  ;; Intersection of mime-infos from different mime-types;
	  ;; or just the first MIME info for a single MIME type
	  (if (cdr all-mime-info)
              (let (res)
                (dolist (mi1 (car all-mime-info))
                  (dolist (mi2 (cdr all-mime-info))
                    (when (member mi1 mi2)
                      (push mi1 res))))
                (nreverse res))
	    (car all-mime-info))))
    ;; Command strings from `viewer' field of the MIME info
    (delete-dups
     (let (res command)
       (dolist (mime-info common-mime-info)
         (setq command (cdr (assq 'viewer mime-info)))
         (when (stringp command)
           (push
            (replace-regexp-in-string
             ;; Replace mailcap's `%s' placeholder
             ;; with dired's `?' placeholder
             "%s" "?"
             (replace-regexp-in-string
              ;; Remove the final filename placeholder
              "[ \t\n]*\\('\\)?%s\\1?[ \t\n]*\\'" ""
              command nil t)
             nil t)
            res)))
       (nreverse res)))))

(defun mailcap--async-shell (command file)
  "Asynchronously call MIME viewer shell COMMAND.
Replace %s in COMMAND with FILE, as per `mailcap-mime-data'.
Delete FILE once COMMAND exits."
  (let ((buf (get-buffer-create " *mailcap shell*")))
    (async-shell-command (format command file) buf)
    (add-function :after (process-sentinel (get-buffer-process buf))
                  (lambda (proc _msg)
                    (when (memq (process-status proc) '(exit signal))
                      (delete-file file))))))

(defun mailcap-view-mime (type)
  "View the data in the current buffer that has MIME type TYPE.
The variable `mailcap--computed-mime-data' determines the method
to use.  If the method is a shell command string, erase the
current buffer after passing its contents to the shell command."
  (let ((method (mailcap-mime-info type)))
    (if (stringp method)
        (let* ((ext (concat "." (cadr (split-string type "/"))))
               (file (make-temp-file "emacs-mailcap" nil ext))
               (coding-system-for-write 'binary))
          (write-region nil nil file nil 'silent)
          (delete-region (point-min) (point-max))
          (mailcap--async-shell method file))
      (funcall method))))

(defun mailcap-view-file (file)
  "View FILE according to rules given by the mailcap system.
This normally involves executing some external program to display
the file.

See \"~/.mailcap\", `mailcap-mime-data' and related files and variables."
  (interactive "fOpen file with mailcap: ")
  (setq file (expand-file-name file))
  (mailcap-parse-mailcaps)
  (let ((command (mailcap-mime-info
                  (mailcap-extension-to-mime (file-name-extension file)))))
    (unless command
      (error "No viewer for %s" (file-name-extension file)))
    ;; Remove quotes around the file name - we'll use shell-quote-argument.
    (while (string-match "['\"]%s['\"]" command)
      (setq command (replace-match "%s" t t command)))
    (setq command (replace-regexp-in-string
		   "%s"
		   (shell-quote-argument (convert-standard-filename file))
		   command
		   nil t))
    ;; Handlers such as "gio open" and kde-open5 start viewer in background
    ;; and exit immediately.  Avoid `start-process' since it assumes
    ;; :connection-type `pty' and kills children processes with SIGHUP
    ;; when temporary terminal session is finished (Bug#44824).
    ;; An alternative is `process-connection-type' let-bound to nil for
    ;; `start-process-shell-command' call (with no chance to report failure).
    (make-process
     :name "mailcap-view-file"
     :connection-type 'pipe
     :buffer nil ; "*Messages*" may be suitable for debugging
     :sentinel (lambda (proc event)
                 (when (and (memq (process-status proc) '(exit signal))
                            (/= (process-exit-status proc) 0))
                   (message
                    "Command %s: %s."
                    (mapconcat #'identity (process-command proc) " ")
                    (substring event 0 -1))))
     :command (list shell-file-name shell-command-switch command))))

(provide 'mailcap)

;;; mailcap.el ends here
