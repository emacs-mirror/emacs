;;; mml.el --- A package for parsing and validating MML documents
;; Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Maintainer: bugs@gnus.org
;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'mm-util)
(require 'mm-bodies)
(require 'mm-encode)
(require 'mm-decode)
(eval-when-compile (require 'cl))

(eval-and-compile
  (autoload 'message-make-message-id "message")
  (autoload 'gnus-setup-posting-charset "gnus-msg")
  (autoload 'message-fetch-field "message")
  (autoload 'message-posting-charset "message"))

(defvar mml-generate-multipart-alist nil
  "*Alist of multipart generation functions.
Each entry has the form (NAME . FUNCTION), where
NAME is a string containing the name of the part (without the 
leading \"/multipart/\"),
FUNCTION is a Lisp function which is called to generate the part.

The Lisp function has to supply the appropriate MIME headers and the
contents of this part.")

(defvar mml-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\\ "/" table)
    (modify-syntax-entry ?< "(" table)
    (modify-syntax-entry ?> ")" table)
    (modify-syntax-entry ?@ "w" table)
    (modify-syntax-entry ?/ "w" table)
    (modify-syntax-entry ?= " " table)
    (modify-syntax-entry ?* " " table)
    (modify-syntax-entry ?\; " " table)
    (modify-syntax-entry ?\' " " table)
    table))

(defvar mml-boundary-function 'mml-make-boundary
  "A function called to suggest a boundary.
The function may be called several times, and should try to make a new
suggestion each time.  The function is called with one parameter,
which is a number that says how many times the function has been
called for this message.")

(defvar mml-confirmation-set nil
  "A list of symbols, each of which disables some warning.
`unknown-encoding': always send messages contain characters with
unknown encoding; `use-ascii': always use ASCII for those characters
with unknown encoding; `multipart': always send messages with more than
one charsets.")

(defvar mml-generate-mime-preprocess-function nil
  "A function called before generating a mime part.
The function is called with one parameter, which is the part to be 
generated.")

(defvar mml-generate-mime-postprocess-function nil
  "A function called after generating a mime part.
The function is called with one parameter, which is the generated part.")

(defvar mml-generate-default-type "text/plain")

(defvar mml-buffer-list nil)

(defun mml-generate-new-buffer (name) 
  (let ((buf (generate-new-buffer name)))
    (push buf mml-buffer-list)
    buf))

(defun mml-destroy-buffers ()
  (let (kill-buffer-hook)
    (mapcar 'kill-buffer mml-buffer-list)
    (setq mml-buffer-list nil)))

(defun mml-parse ()
  "Parse the current buffer as an MML document."
  (goto-char (point-min))
  (let ((table (syntax-table)))
    (unwind-protect
	(progn
	  (set-syntax-table mml-syntax-table)
	  (mml-parse-1))
      (set-syntax-table table))))

(defun mml-parse-1 ()
  "Parse the current buffer as an MML document."
  (let (struct tag point contents charsets warn use-ascii no-markup-p raw)
    (while (and (not (eobp))
		(not (looking-at "<#/multipart")))
      (cond
       ((looking-at "<#multipart")
	(push (nconc (mml-read-tag) (mml-parse-1)) struct))
       ((looking-at "<#external")
	(push (nconc (mml-read-tag) (list (cons 'contents (mml-read-part))))
	      struct))
       (t
	(if (or (looking-at "<#part") (looking-at "<#mml"))
	    (setq tag (mml-read-tag)
		  no-markup-p nil
		  warn nil)
	  (setq tag (list 'part '(type . "text/plain"))
		no-markup-p t
		warn t))
	(setq raw (cdr (assq 'raw tag))
	      point (point)
	      contents (mml-read-part (eq 'mml (car tag)))
	      charsets (if raw nil 
			 (mm-find-mime-charset-region point (point))))
	(when (and (not raw) (memq nil charsets))
	  (if (or (memq 'unknown-encoding mml-confirmation-set)
		  (y-or-n-p
		   "Message contains characters with unknown encoding.  Really send?"))
	      (if (setq use-ascii 
			(or (memq 'use-ascii mml-confirmation-set)
			    (y-or-n-p "Use ASCII as charset?")))
		  (setq charsets (delq nil charsets))
		(setq warn nil))
	    (error "Edit your message to remove those characters")))
	(if (or raw
		(eq 'mml (car tag))
		(< (length charsets) 2))
	    (if (or (not no-markup-p)
		    (string-match "[^ \t\r\n]" contents))
		;; Don't create blank parts.
		(push (nconc tag (list (cons 'contents contents)))
		      struct))
	  (let ((nstruct (mml-parse-singlepart-with-multiple-charsets
			  tag point (point) use-ascii)))
	    (when (and warn
		       (not (memq 'multipart mml-confirmation-set))
		       (not
			(y-or-n-p
			 (format
			  "Warning: Your message contains more than %d parts.  Really send? "
			  (length nstruct)))))
	      (error "Edit your message to use only one charset"))
	    (setq struct (nconc nstruct struct)))))))
    (unless (eobp)
      (forward-line 1))
    (nreverse struct)))

(defun mml-parse-singlepart-with-multiple-charsets 
  (orig-tag beg end &optional use-ascii)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((current (or (mm-mime-charset (mm-charset-after))
			 (and use-ascii 'us-ascii)))
	    charset struct space newline paragraph)
	(while (not (eobp))
	  (setq charset (mm-mime-charset (mm-charset-after)))
	  (cond
	   ;; The charset remains the same.
	   ((eq charset 'us-ascii))
	   ((or (and use-ascii (not charset))
		(eq charset current))
	    (setq space nil
		  newline nil
		  paragraph nil))
	   ;; The initial charset was ascii.
	   ((eq current 'us-ascii)
	    (setq current charset
		  space nil
		  newline nil
		  paragraph nil))
	   ;; We have a change in charsets.
	   (t
	    (push (append
		   orig-tag
		   (list (cons 'contents
			       (buffer-substring-no-properties
				beg (or paragraph newline space (point))))))
		  struct)
	    (setq beg (or paragraph newline space (point))
		  current charset
		  space nil
		  newline nil
		  paragraph nil)))
	  ;; Compute places where it might be nice to break the part.
	  (cond
	   ((memq (following-char) '(?  ?\t))
	    (setq space (1+ (point))))
	   ((and (eq (following-char) ?\n)
		 (not (bobp))
		 (eq (char-after (1- (point))) ?\n))
	    (setq paragraph (point)))
	   ((eq (following-char) ?\n)
	    (setq newline (1+ (point)))))
	  (forward-char 1))
	;; Do the final part.
	(unless (= beg (point))
	  (push (append orig-tag
			(list (cons 'contents
				    (buffer-substring-no-properties
				     beg (point)))))
		struct))
	struct))))

(defun mml-read-tag ()
  "Read a tag and return the contents."
  (let (contents name elem val)
    (forward-char 2)
    (setq name (buffer-substring-no-properties
		(point) (progn (forward-sexp 1) (point))))
    (skip-chars-forward " \t\n")
    (while (not (looking-at ">"))
      (setq elem (buffer-substring-no-properties
		  (point) (progn (forward-sexp 1) (point))))
      (skip-chars-forward "= \t\n")
      (setq val (buffer-substring-no-properties
		 (point) (progn (forward-sexp 1) (point))))
      (when (string-match "^\"\\(.*\\)\"$" val)
	(setq val (match-string 1 val)))
      (push (cons (intern elem) val) contents)
      (skip-chars-forward " \t\n"))
    (forward-char 1)
    (skip-chars-forward " \t\n")
    (cons (intern name) (nreverse contents))))

(defun mml-read-part (&optional mml)
  "Return the buffer up till the next part, multipart or closing part or multipart.
If MML is non-nil, return the buffer up till the correspondent mml tag."
  (let ((beg (point)) (count 1))
    ;; If the tag ended at the end of the line, we go to the next line.
    (when (looking-at "[ \t]*\n")
      (forward-line 1))
    (if mml
	(progn
	  (while (and (> count 0) (not (eobp)))
	    (if (re-search-forward "<#\\(/\\)?mml." nil t)
		(setq count (+ count (if (match-beginning 1) -1 1)))
	      (goto-char (point-max))))
	  (buffer-substring-no-properties beg (if (> count 0) 
						  (point)
						(match-beginning 0))))
      (if (re-search-forward
	   "<#\\(/\\)?\\(multipart\\|part\\|external\\|mml\\)." nil t)
	  (prog1
	      (buffer-substring-no-properties beg (match-beginning 0))
	    (if (or (not (match-beginning 1))
		    (equal (match-string 2) "multipart"))
		(goto-char (match-beginning 0))
	      (when (looking-at "[ \t]*\n")
		(forward-line 1))))
	(buffer-substring-no-properties beg (goto-char (point-max)))))))

(defvar mml-boundary nil)
(defvar mml-base-boundary "-=-=")
(defvar mml-multipart-number 0)

(defun mml-generate-mime ()
  "Generate a MIME message based on the current MML document."
  (let ((cont (mml-parse))
	(mml-multipart-number mml-multipart-number))
    (if (not cont)
	nil
      (with-temp-buffer
	(if (and (consp (car cont))
		 (= (length cont) 1))
	    (mml-generate-mime-1 (car cont))
	  (mml-generate-mime-1 (nconc (list 'multipart '(type . "mixed"))
				      cont)))
	(buffer-string)))))

(defun mml-generate-mime-1 (cont)
  (save-restriction
    (narrow-to-region (point) (point))
    (if mml-generate-mime-preprocess-function
	(funcall mml-generate-mime-preprocess-function cont))
    (cond
     ((or (eq (car cont) 'part) (eq (car cont) 'mml))
      (let ((raw (cdr (assq 'raw cont)))
	    coded encoding charset filename type)
	(setq type (or (cdr (assq 'type cont)) "text/plain"))
	(if (and (not raw)
		 (member (car (split-string type "/")) '("text" "message")))
	    (with-temp-buffer
	      (cond
	       ((cdr (assq 'buffer cont))
		(insert-buffer-substring (cdr (assq 'buffer cont))))
	       ((and (setq filename (cdr (assq 'filename cont)))
		     (not (equal (cdr (assq 'nofile cont)) "yes")))
		(mm-insert-file-contents filename))
	       ((eq 'mml (car cont))
		(insert (cdr (assq 'contents cont))))
	       (t
		(save-restriction
		  (narrow-to-region (point) (point))
		  (insert (cdr (assq 'contents cont)))
		  ;; Remove quotes from quoted tags.
		  (goto-char (point-min))
		  (while (re-search-forward
			  "<#!+/?\\(part\\|multipart\\|external\\|mml\\)" nil t)
		    (delete-region (+ (match-beginning 0) 2)
				   (+ (match-beginning 0) 3))))))
	      (cond 
	       ((eq (car cont) 'mml)
		(let ((mml-boundary (funcall mml-boundary-function
					     (incf mml-multipart-number)))
		      (mml-generate-default-type "text/plain"))
		  (mml-to-mime))
		(let ((mm-7bit-chars (concat mm-7bit-chars "\x1b")))
		  ;; ignore 0x1b, it is part of iso-2022-jp
		  (setq encoding (mm-body-7-or-8))))
	       ((string= (car (split-string type "/")) "message")
		(let ((mm-7bit-chars (concat mm-7bit-chars "\x1b")))
		  ;; ignore 0x1b, it is part of iso-2022-jp
		  (setq encoding (mm-body-7-or-8))))
	       (t 
		(setq charset (mm-encode-body))
		(setq encoding (mm-body-encoding
				charset (cdr (assq 'encoding cont))))))
	      (setq coded (buffer-string)))
	  (mm-with-unibyte-buffer
	    (cond
	     ((cdr (assq 'buffer cont))
	      (insert-buffer-substring (cdr (assq 'buffer cont))))
	     ((and (setq filename (cdr (assq 'filename cont)))
		   (not (equal (cdr (assq 'nofile cont)) "yes")))
	      (let ((coding-system-for-read mm-binary-coding-system))
		(mm-insert-file-contents filename nil nil nil nil t)))
	     (t
	      (insert (cdr (assq 'contents cont)))))
	    (setq encoding (mm-encode-buffer type)
		  coded (buffer-string))))
	(mml-insert-mime-headers cont type charset encoding)
	(insert "\n")
	(insert coded)))
     ((eq (car cont) 'external)
      (insert "Content-Type: message/external-body")
      (let ((parameters (mml-parameter-string
			 cont '(expiration size permission)))
	    (name (cdr (assq 'name cont))))
	(when name
	  (setq name (mml-parse-file-name name))
	  (if (stringp name)
	      (mml-insert-parameter
	       (mail-header-encode-parameter "name" name)
	       "access-type=local-file")
	    (mml-insert-parameter
	     (mail-header-encode-parameter
	      "name" (file-name-nondirectory (nth 2 name)))
	     (mail-header-encode-parameter "site" (nth 1 name))
	     (mail-header-encode-parameter
	      "directory" (file-name-directory (nth 2 name))))
	    (mml-insert-parameter
	     (concat "access-type="
		     (if (member (nth 0 name) '("ftp@" "anonymous@"))
			 "anon-ftp"
		       "ftp")))))      
	(when parameters
	  (mml-insert-parameter-string
	   cont '(expiration size permission))))
      (insert "\n\n")
      (insert "Content-Type: " (cdr (assq 'type cont)) "\n")
      (insert "Content-ID: " (message-make-message-id) "\n")
      (insert "Content-Transfer-Encoding: "
	      (or (cdr (assq 'encoding cont)) "binary"))
      (insert "\n\n")
      (insert (or (cdr (assq 'contents cont))))
      (insert "\n"))
     ((eq (car cont) 'multipart)
      (let* ((type (or (cdr (assq 'type cont)) "mixed"))
	     (mml-generate-default-type (if (equal type "digest")
					    "message/rfc822"
					  "text/plain"))
	     (handler (assoc type mml-generate-multipart-alist)))
	(if handler
	    (funcall (cdr handler) cont)
	  ;; No specific handler.  Use default one.
	  (let ((mml-boundary (mml-compute-boundary cont)))
	    (insert (format "Content-Type: multipart/%s; boundary=\"%s\"\n"
			    type mml-boundary))
	    ;; Skip `multipart' and `type' elements.
	    (setq cont (cddr cont))
	    (while cont
	      (insert "\n--" mml-boundary "\n")
	      (mml-generate-mime-1 (pop cont)))
	    (insert "\n--" mml-boundary "--\n")))))
     (t
      (error "Invalid element: %S" cont)))
    (if mml-generate-mime-postprocess-function
	(funcall mml-generate-mime-postprocess-function cont))))

(defun mml-compute-boundary (cont)
  "Return a unique boundary that does not exist in CONT."
  (let ((mml-boundary (funcall mml-boundary-function
			       (incf mml-multipart-number))))
    ;; This function tries again and again until it has found
    ;; a unique boundary.
    (while (not (catch 'not-unique
		  (mml-compute-boundary-1 cont))))
    mml-boundary))

(defun mml-compute-boundary-1 (cont)
  (let (filename)
    (cond
     ((eq (car cont) 'part)
      (with-temp-buffer
	(cond
	 ((cdr (assq 'buffer cont))
	  (insert-buffer-substring (cdr (assq 'buffer cont))))
	 ((and (setq filename (cdr (assq 'filename cont)))
	       (not (equal (cdr (assq 'nofile cont)) "yes")))
	  (mm-insert-file-contents filename))
	 (t
	  (insert (cdr (assq 'contents cont)))))
	(goto-char (point-min))
	(when (re-search-forward (concat "^--" (regexp-quote mml-boundary))
				 nil t)
	  (setq mml-boundary (funcall mml-boundary-function
				      (incf mml-multipart-number)))
	  (throw 'not-unique nil))))
     ((eq (car cont) 'multipart)
      (mapcar 'mml-compute-boundary-1 (cddr cont))))
    t))

(defun mml-make-boundary (number)
  (concat (make-string (% number 60) ?=)
	  (if (> number 17)
	      (format "%x" number)
	    "")
	  mml-base-boundary))

(defun mml-insert-mime-headers (cont type charset encoding)
  (let (parameters disposition description)
    (setq parameters
	  (mml-parameter-string
	   cont '(name access-type expiration size permission)))
    (when (or charset
	      parameters
	      (not (equal type mml-generate-default-type)))
      (when (consp charset)
	(error
	 "Can't encode a part with several charsets."))
      (insert "Content-Type: " type)
      (when charset
	(insert "; " (mail-header-encode-parameter
		      "charset" (symbol-name charset))))
      (when parameters
	(mml-insert-parameter-string
	 cont '(name access-type expiration size permission)))
      (insert "\n"))
    (setq parameters
	  (mml-parameter-string
	   cont '(filename creation-date modification-date read-date)))
    (when (or (setq disposition (cdr (assq 'disposition cont)))
	      parameters)
      (insert "Content-Disposition: " (or disposition "inline"))
      (when parameters
	(mml-insert-parameter-string
	 cont '(filename creation-date modification-date read-date)))
      (insert "\n"))
    (unless (eq encoding '7bit)
      (insert (format "Content-Transfer-Encoding: %s\n" encoding)))
    (when (setq description (cdr (assq 'description cont)))
      (insert "Content-Description: "
	      (mail-encode-encoded-word-string description) "\n"))))

(defun mml-parameter-string (cont types)
  (let ((string "")
	value type)
    (while (setq type (pop types))
      (when (setq value (cdr (assq type cont)))
	;; Strip directory component from the filename parameter.
	(when (eq type 'filename)
	  (setq value (file-name-nondirectory value)))
	(setq string (concat string "; "
			     (mail-header-encode-parameter
			      (symbol-name type) value)))))
    (when (not (zerop (length string)))
      string)))

(defun mml-insert-parameter-string (cont types)
  (let (value type)
    (while (setq type (pop types))
      (when (setq value (cdr (assq type cont)))
	;; Strip directory component from the filename parameter.
	(when (eq type 'filename)
	  (setq value (file-name-nondirectory value)))
	(mml-insert-parameter
	 (mail-header-encode-parameter
	  (symbol-name type) value))))))

(defvar ange-ftp-name-format)
(defvar efs-path-regexp)
(defun mml-parse-file-name (path)
  (if (if (boundp 'efs-path-regexp)
	  (string-match efs-path-regexp path)
	(if (boundp 'ange-ftp-name-format)
	    (string-match (car ange-ftp-name-format) path)))
      (list (match-string 1 path) (match-string 2 path)
	    (substring path (1+ (match-end 2))))
    path))

(defun mml-insert-buffer (buffer)
  "Insert BUFFER at point and quote any MML markup."
  (save-restriction
    (narrow-to-region (point) (point))
    (insert-buffer-substring buffer)
    (mml-quote-region (point-min) (point-max))
    (goto-char (point-max))))

;;;
;;; Transforming MIME to MML
;;;

(defun mime-to-mml ()
  "Translate the current buffer (which should be a message) into MML."
  ;; First decode the head.
  (save-restriction
    (message-narrow-to-head)
    (mail-decode-encoded-word-region (point-min) (point-max)))
  (let ((handles (mm-dissect-buffer t)))
    (goto-char (point-min))
    (search-forward "\n\n" nil t)
    (delete-region (point) (point-max))
    (if (stringp (car handles))
	(mml-insert-mime handles)
      (mml-insert-mime handles t))
    (mm-destroy-parts handles))
  (save-restriction
    (message-narrow-to-head)
    ;; Remove them, they are confusing.
    (message-remove-header "Content-Type")
    (message-remove-header "MIME-Version")
    (message-remove-header "Content-Transfer-Encoding")))

(defun mml-to-mime ()
  "Translate the current buffer from MML to MIME."
  (message-encode-message-body)
  (save-restriction
    (message-narrow-to-headers-or-head)
    (let ((mail-parse-charset message-default-charset))
      (mail-encode-encoded-word-buffer))))

(defun mml-insert-mime (handle &optional no-markup)
  (let (textp buffer mmlp)
    ;; Determine type and stuff.
    (unless (stringp (car handle))
      (unless (setq textp (equal (mm-handle-media-supertype handle) "text"))
	(save-excursion
	  (set-buffer (setq buffer (mml-generate-new-buffer " *mml*")))
	  (mm-insert-part handle)
	  (if (setq mmlp (equal (mm-handle-media-type handle) 
				"message/rfc822"))
	      (mime-to-mml)))))
    (if mmlp
	(mml-insert-mml-markup handle nil t t)
      (unless (and no-markup
		   (equal (mm-handle-media-type handle) "text/plain"))
	(mml-insert-mml-markup handle buffer textp)))
    (cond
     (mmlp 
      (insert-buffer buffer)
      (goto-char (point-max))
      (insert "<#/mml>\n"))
     ((stringp (car handle))
      (mapcar 'mml-insert-mime (cdr handle))
      (insert "<#/multipart>\n"))
     (textp
      (let ((text (mm-get-part handle))
	    (charset (mail-content-type-get
		      (mm-handle-type handle) 'charset)))
	(insert (mm-decode-string text charset)))
      (goto-char (point-max)))
     (t
      (insert "<#/part>\n")))))

(defun mml-insert-mml-markup (handle &optional buffer nofile mmlp)
  "Take a MIME handle and insert an MML tag."
  (if (stringp (car handle))
      (insert "<#multipart type=" (mm-handle-media-subtype handle)
	      ">\n")
    (if mmlp
	(insert "<#mml type=" (mm-handle-media-type handle))
      (insert "<#part type=" (mm-handle-media-type handle)))
    (dolist (elem (append (cdr (mm-handle-type handle))
			  (cdr (mm-handle-disposition handle))))
      (insert " " (symbol-name (car elem)) "=\"" (cdr elem) "\""))
    (when (mm-handle-disposition handle)
      (insert " disposition=" (car (mm-handle-disposition handle))))
    (when buffer
      (insert " buffer=\"" (buffer-name buffer) "\""))
    (when nofile
      (insert " nofile=yes"))
    (when (mm-handle-description handle)
      (insert " description=\"" (mm-handle-description handle) "\""))
    (insert ">\n")))

(defun mml-insert-parameter (&rest parameters)
  "Insert PARAMETERS in a nice way."
  (dolist (param parameters)
    (insert ";")
    (let ((point (point)))
      (insert " " param)
      (when (> (current-column) 71)
	(goto-char point)
	(insert "\n ")
	(end-of-line)))))

;;;
;;; Mode for inserting and editing MML forms
;;;

(defvar mml-mode-map
  (let ((map (make-sparse-keymap))
	(main (make-sparse-keymap)))
    (define-key map "f" 'mml-attach-file)
    (define-key map "b" 'mml-attach-buffer)
    (define-key map "e" 'mml-attach-external)
    (define-key map "q" 'mml-quote-region)
    (define-key map "m" 'mml-insert-multipart)
    (define-key map "p" 'mml-insert-part)
    (define-key map "v" 'mml-validate)
    (define-key map "P" 'mml-preview)
    ;;(define-key map "n" 'mml-narrow-to-part)
    (define-key main "\M-m" map)
    main))

(easy-menu-define
 mml-menu mml-mode-map ""
 '("MML"
   ("Attach"
    ["File" mml-attach-file t]
    ["Buffer" mml-attach-buffer t]
    ["External" mml-attach-external t])
   ("Insert"
    ["Multipart" mml-insert-multipart t]
    ["Part" mml-insert-part t])
   ;;["Narrow" mml-narrow-to-part t]
   ["Quote" mml-quote-region t]
   ["Validate" mml-validate t]
   ["Preview" mml-preview t]))

(defvar mml-mode nil
  "Minor mode for editing MML.")

(defun mml-mode (&optional arg)
  "Minor mode for editing MML.

\\{mml-mode-map}"
  (interactive "P")
  (if (not (set (make-local-variable 'mml-mode)
		(if (null arg) (not mml-mode)
		  (> (prefix-numeric-value arg) 0))))
      nil
    (set (make-local-variable 'mml-mode) t)
    (unless (assq 'mml-mode minor-mode-alist)
      (push `(mml-mode " MML") minor-mode-alist))
    (unless (assq 'mml-mode minor-mode-map-alist)
      (push (cons 'mml-mode mml-mode-map)
	    minor-mode-map-alist)))
  (run-hooks 'mml-mode-hook))

;;;
;;; Helper functions for reading MIME stuff from the minibuffer and
;;; inserting stuff to the buffer.
;;;

(defun mml-minibuffer-read-file (prompt)
  (let ((file (read-file-name prompt nil nil t)))
    ;; Prevent some common errors.  This is inspired by similar code in
    ;; VM.
    (when (file-directory-p file)
      (error "%s is a directory, cannot attach" file))
    (unless (file-exists-p file)
      (error "No such file: %s" file))
    (unless (file-readable-p file)
      (error "Permission denied: %s" file))
    file))

(defun mml-minibuffer-read-type (name &optional default)
  (mailcap-parse-mimetypes)
  (let* ((default (or default
		      (mm-default-file-encoding name)
		      ;; Perhaps here we should check what the file
		      ;; looks like, and offer text/plain if it looks
		      ;; like text/plain.
		      "application/octet-stream"))
	 (string (completing-read
		  (format "Content type (default %s): " default)
		  (mapcar 'list (mailcap-mime-types)))))
    (if (not (equal string ""))
	string
      default)))

(defun mml-minibuffer-read-description ()
  (let ((description (read-string "One line description: ")))
    (when (string-match "\\`[ \t]*\\'" description)
      (setq description nil))
    description))

(defun mml-quote-region (beg end)
  "Quote the MML tags in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      ;; Temporarily narrow the region to defend from changes
      ;; invalidating END.
      (narrow-to-region beg end)
      (goto-char (point-min))
      ;; Quote parts.
      (while (re-search-forward
	      "<#!*/?\\(multipart\\|part\\|external\\|mml\\)" nil t)
	;; Insert ! after the #.
	(goto-char (+ (match-beginning 0) 2))
	(insert "!")))))

(defun mml-insert-tag (name &rest plist)
  "Insert an MML tag described by NAME and PLIST."
  (when (symbolp name)
    (setq name (symbol-name name)))
  (insert "<#" name)
  (while plist
    (let ((key (pop plist))
	  (value (pop plist)))
      (when value
	;; Quote VALUE if it contains suspicious characters.
	(when (string-match "[\"'\\~/*;() \t\n]" value)
	  (setq value (prin1-to-string value)))
	(insert (format " %s=%s" key value)))))
  (insert ">\n"))

(defun mml-insert-empty-tag (name &rest plist)
  "Insert an empty MML tag described by NAME and PLIST."
  (when (symbolp name)
    (setq name (symbol-name name)))
  (apply #'mml-insert-tag name plist)
  (insert "<#/" name ">\n"))

;;; Attachment functions.

(defun mml-attach-file (file &optional type description)
  "Attach a file to the outgoing MIME message.
The file is not inserted or encoded until you send the message with
`\\[message-send-and-exit]' or `\\[message-send]'.

FILE is the name of the file to attach.  TYPE is its content-type, a
string of the form \"type/subtype\".  DESCRIPTION is a one-line
description of the attachment."
  (interactive
   (let* ((file (mml-minibuffer-read-file "Attach file: "))
	  (type (mml-minibuffer-read-type file))
	  (description (mml-minibuffer-read-description)))
     (list file type description)))
  (mml-insert-empty-tag 'part 'type type 'filename file
			'disposition "attachment" 'description description))

(defun mml-attach-buffer (buffer &optional type description)
  "Attach a buffer to the outgoing MIME message.
See `mml-attach-file' for details of operation."
  (interactive
   (let* ((buffer (read-buffer "Attach buffer: "))
	  (type (mml-minibuffer-read-type buffer "text/plain"))
	  (description (mml-minibuffer-read-description)))
     (list buffer type description)))
  (mml-insert-empty-tag 'part 'type type 'buffer buffer
			'disposition "attachment" 'description description))

(defun mml-attach-external (file &optional type description)
  "Attach an external file into the buffer.
FILE is an ange-ftp/efs specification of the part location.
TYPE is the MIME type to use."
  (interactive
   (let* ((file (mml-minibuffer-read-file "Attach external file: "))
	  (type (mml-minibuffer-read-type file))
	  (description (mml-minibuffer-read-description)))
     (list file type description)))
  (mml-insert-empty-tag 'external 'type type 'name file
			'disposition "attachment" 'description description))

(defun mml-insert-multipart (&optional type)
  (interactive (list (completing-read "Multipart type (default mixed): "
				      '(("mixed") ("alternative") ("digest") ("parallel")
					("signed") ("encrypted"))
				      nil nil "mixed")))
  (or type
      (setq type "mixed"))
  (mml-insert-empty-tag "multipart" 'type type)
  (forward-line -1))

(defun mml-insert-part (&optional type)
  (interactive
   (list (mml-minibuffer-read-type "")))
  (mml-insert-tag 'part 'type type 'disposition "inline")
  (forward-line -1))

(defun mml-preview (&optional raw)
  "Display current buffer with Gnus, in a new buffer.
If RAW, don't highlight the article."
  (interactive "P")
  (let ((buf (current-buffer))
	(message-posting-charset (or (gnus-setup-posting-charset 
				      (save-restriction
					(message-narrow-to-headers-or-head)
					(message-fetch-field "Newsgroups")))
				     message-posting-charset)))
    (switch-to-buffer (get-buffer-create 
		       (concat (if raw "*Raw MIME preview of "
				 "*MIME preview of ") (buffer-name))))
    (erase-buffer)
    (insert-buffer buf)
    (if (re-search-forward
	 (concat "^" (regexp-quote mail-header-separator) "\n") nil t)
	(replace-match "\n"))
    (mml-to-mime)
    (if raw
	(when (fboundp 'set-buffer-multibyte)
	  (let ((s (buffer-string)))
	    ;; Insert the content into unibyte buffer.
	    (erase-buffer)
	    (mm-disable-multibyte)
	    (insert s)))
      (let ((gnus-newsgroup-charset (car message-posting-charset)))
	(run-hooks 'gnus-article-decode-hook)
	(let ((gnus-newsgroup-name "dummy"))
	  (gnus-article-prepare-display))))
    (fundamental-mode)
    (setq buffer-read-only t)
    (goto-char (point-min))))

(defun mml-validate ()
  "Validate the current MML document."
  (interactive)
  (mml-parse))

(provide 'mml)

;;; mml.el ends here
