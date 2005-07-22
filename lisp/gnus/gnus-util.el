;;; gnus-util.el --- utility functions for Gnus
;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Nothing in this file depends on any other parts of Gnus -- all
;; functions and macros in this file are utility functions that are
;; used by Gnus and may be used by any other package without loading
;; Gnus first.

;; [Unfortunately, it does depend on other parts of Gnus, e.g. the
;; autoloads below...]

;;; Code:

(require 'custom)
(eval-when-compile
  (require 'cl)
  ;; Fixme: this should be a gnus variable, not nnmail-.
  (defvar nnmail-pathname-coding-system)

  ;; Inappropriate references to other parts of Gnus.
  (defvar gnus-emphasize-whitespace-regexp)
  )
(require 'time-date)
(require 'netrc)

(eval-and-compile
  (autoload 'message-fetch-field "message")
  (autoload 'gnus-get-buffer-window "gnus-win")
  (autoload 'rmail-insert-rmail-file-header "rmail")
  (autoload 'rmail-count-new-messages "rmail")
  (autoload 'rmail-show-message "rmail")
  (autoload 'nnheader-narrow-to-headers "nnheader")
  (autoload 'nnheader-replace-chars-in-string "nnheader"))

(eval-and-compile
  (cond
   ((fboundp 'replace-in-string)
    (defalias 'gnus-replace-in-string 'replace-in-string))
   ((fboundp 'replace-regexp-in-string)
    (defun gnus-replace-in-string (string regexp newtext &optional literal)
      "Replace all matches for REGEXP with NEWTEXT in STRING.
If LITERAL is non-nil, insert NEWTEXT literally.  Return a new
string containing the replacements.

This is a compatibility function for different Emacsen."
      (replace-regexp-in-string regexp newtext string nil literal)))
   (t
    (defun gnus-replace-in-string (string regexp newtext &optional literal)
      "Replace all matches for REGEXP with NEWTEXT in STRING.
If LITERAL is non-nil, insert NEWTEXT literally.  Return a new
string containing the replacements.

This is a compatibility function for different Emacsen."
      (let ((start 0) tail)
	(while (string-match regexp string start)
	  (setq tail (- (length string) (match-end 0)))
	  (setq string (replace-match newtext nil literal string))
	  (setq start (- (length string) tail))))
      string))))

;;; bring in the netrc functions as aliases
(defalias 'gnus-netrc-get 'netrc-get)
(defalias 'gnus-netrc-machine 'netrc-machine)
(defalias 'gnus-parse-netrc 'netrc-parse)

(defun gnus-boundp (variable)
  "Return non-nil if VARIABLE is bound and non-nil."
  (and (boundp variable)
       (symbol-value variable)))

(defmacro gnus-eval-in-buffer-window (buffer &rest forms)
  "Pop to BUFFER, evaluate FORMS, and then return to the original window."
  (let ((tempvar (make-symbol "GnusStartBufferWindow"))
	(w (make-symbol "w"))
	(buf (make-symbol "buf")))
    `(let* ((,tempvar (selected-window))
	    (,buf ,buffer)
	    (,w (gnus-get-buffer-window ,buf 'visible)))
       (unwind-protect
	   (progn
	     (if ,w
		 (progn
		   (select-window ,w)
		   (set-buffer (window-buffer ,w)))
	       (pop-to-buffer ,buf))
	     ,@forms)
	 (select-window ,tempvar)))))

(put 'gnus-eval-in-buffer-window 'lisp-indent-function 1)
(put 'gnus-eval-in-buffer-window 'edebug-form-spec '(form body))

(defmacro gnus-intern-safe (string hashtable)
  "Set hash value.  Arguments are STRING, VALUE, and HASHTABLE."
  `(let ((symbol (intern ,string ,hashtable)))
     (or (boundp symbol)
	 (set symbol nil))
     symbol))

;; Added by Geoffrey T. Dairiki <dairiki@u.washington.edu>.  A safe way
;; to limit the length of a string.  This function is necessary since
;; `(substr "abc" 0 30)' pukes with "Args out of range".
;; Fixme: Why not `truncate-string-to-width'?
(defsubst gnus-limit-string (str width)
  (if (> (length str) width)
      (substring str 0 width)
    str))

(defsubst gnus-goto-char (point)
  (and point (goto-char point)))

(defmacro gnus-buffer-exists-p (buffer)
  `(let ((buffer ,buffer))
     (when buffer
       (funcall (if (stringp buffer) 'get-buffer 'buffer-name)
		buffer))))

(defalias 'gnus-point-at-bol
  (if (fboundp 'point-at-bol)
      'point-at-bol
    'line-beginning-position))

(defalias 'gnus-point-at-eol
  (if (fboundp 'point-at-eol)
      'point-at-eol
    'line-end-position))

;; The LOCAL arg to `add-hook' is interpreted differently in Emacs and
;; XEmacs.  In Emacs we don't need to call `make-local-hook' first.
;; It's harmless, though, so the main purpose of this alias is to shut
;; up the byte compiler.
(defalias 'gnus-make-local-hook
  (if (eq (get 'make-local-hook 'byte-compile)
	  'byte-compile-obsolete)
      'ignore				; Emacs
    'make-local-hook))			; XEmacs

(defun gnus-delete-first (elt list)
  "Delete by side effect the first occurrence of ELT as a member of LIST."
  (if (equal (car list) elt)
      (cdr list)
    (let ((total list))
      (while (and (cdr list)
		  (not (equal (cadr list) elt)))
	(setq list (cdr list)))
      (when (cdr list)
	(setcdr list (cddr list)))
      total)))

;; Delete the current line (and the next N lines).
(defmacro gnus-delete-line (&optional n)
  `(delete-region (gnus-point-at-bol)
		  (progn (forward-line ,(or n 1)) (point))))

(defun gnus-byte-code (func)
  "Return a form that can be `eval'ed based on FUNC."
  (let ((fval (indirect-function func)))
    (if (byte-code-function-p fval)
	(let ((flist (append fval nil)))
	  (setcar flist 'byte-code)
	  flist)
      (cons 'progn (cddr fval)))))

(defun gnus-extract-address-components (from)
  "Extract address components from a From header.
Given an RFC-822 address FROM, extract full name and canonical address.
Returns a list of the form (FULL-NAME CANONICAL-ADDRESS).  Much more simple
solution than `mail-extract-address-components', which works much better, but
is slower."
  (let (name address)
    ;; First find the address - the thing with the @ in it.  This may
    ;; not be accurate in mail addresses, but does the trick most of
    ;; the time in news messages.
    (when (string-match "\\b[^@ \t<>]+[!@][^@ \t<>]+\\b" from)
      (setq address (substring from (match-beginning 0) (match-end 0))))
    ;; Then we check whether the "name <address>" format is used.
    (and address
	 ;; Linear white space is not required.
	 (string-match (concat "[ \t]*<" (regexp-quote address) ">") from)
	 (and (setq name (substring from 0 (match-beginning 0)))
	      ;; Strip any quotes from the name.
	      (string-match "^\".*\"$" name)
	      (setq name (substring name 1 (1- (match-end 0))))))
    ;; If not, then "address (name)" is used.
    (or name
	(and (string-match "(.+)" from)
	     (setq name (substring from (1+ (match-beginning 0))
				   (1- (match-end 0)))))
	(and (string-match "()" from)
	     (setq name address))
	;; XOVER might not support folded From headers.
	(and (string-match "(.*" from)
	     (setq name (substring from (1+ (match-beginning 0))
				   (match-end 0)))))
    (list (if (string= name "") nil name) (or address from))))


(defun gnus-fetch-field (field)
  "Return the value of the header FIELD of current article."
  (save-excursion
    (save-restriction
      (let ((case-fold-search t)
	    (inhibit-point-motion-hooks t))
	(nnheader-narrow-to-headers)
	(message-fetch-field field)))))

(defun gnus-fetch-original-field (field)
  "Fetch FIELD from the original version of the current article."
  (with-current-buffer gnus-original-article-buffer
    (gnus-fetch-field field)))


(defun gnus-goto-colon ()
  (beginning-of-line)
  (let ((eol (gnus-point-at-eol)))
    (goto-char (or (text-property-any (point) eol 'gnus-position t)
		   (search-forward ":" eol t)
		   (point)))))

(defun gnus-decode-newsgroups (newsgroups group &optional method)
  (let ((method (or method (gnus-find-method-for-group group))))
    (mapconcat (lambda (group)
		 (gnus-group-name-decode group (gnus-group-name-charset
						method group)))
	       (message-tokenize-header newsgroups)
	       ",")))

(defun gnus-remove-text-with-property (prop)
  "Delete all text in the current buffer with text property PROP."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (while (get-text-property (point) prop)
	(delete-char 1))
      (goto-char (next-single-property-change (point) prop nil (point-max))))))

(defun gnus-newsgroup-directory-form (newsgroup)
  "Make hierarchical directory name from NEWSGROUP name."
  (let* ((newsgroup (gnus-newsgroup-savable-name newsgroup))
	 (idx (string-match ":" newsgroup)))
    (concat
     (if idx (substring newsgroup 0 idx))
     (if idx "/")
     (nnheader-replace-chars-in-string
      (if idx (substring newsgroup (1+ idx)) newsgroup)
      ?. ?/))))

(defun gnus-newsgroup-savable-name (group)
  ;; Replace any slashes in a group name (eg. an ange-ftp nndoc group)
  ;; with dots.
  (nnheader-replace-chars-in-string group ?/ ?.))

(defun gnus-string> (s1 s2)
  (not (or (string< s1 s2)
	   (string= s1 s2))))

;;; Time functions.

(defun gnus-file-newer-than (file date)
  (let ((fdate (nth 5 (file-attributes file))))
    (or (> (car fdate) (car date))
	(and (= (car fdate) (car date))
	     (> (nth 1 fdate) (nth 1 date))))))

;;; Keymap macros.

(defmacro gnus-local-set-keys (&rest plist)
  "Set the keys in PLIST in the current keymap."
  `(gnus-define-keys-1 (current-local-map) ',plist))

(defmacro gnus-define-keys (keymap &rest plist)
  "Define all keys in PLIST in KEYMAP."
  `(gnus-define-keys-1 (quote ,keymap) (quote ,plist)))

(defmacro gnus-define-keys-safe (keymap &rest plist)
  "Define all keys in PLIST in KEYMAP without overwriting previous definitions."
  `(gnus-define-keys-1 (quote ,keymap) (quote ,plist) t))

(put 'gnus-define-keys 'lisp-indent-function 1)
(put 'gnus-define-keys-safe 'lisp-indent-function 1)
(put 'gnus-local-set-keys 'lisp-indent-function 1)

(defmacro gnus-define-keymap (keymap &rest plist)
  "Define all keys in PLIST in KEYMAP."
  `(gnus-define-keys-1 ,keymap (quote ,plist)))

(put 'gnus-define-keymap 'lisp-indent-function 1)

(defun gnus-define-keys-1 (keymap plist &optional safe)
  (when (null keymap)
    (error "Can't set keys in a null keymap"))
  (cond ((symbolp keymap)
	 (setq keymap (symbol-value keymap)))
	((keymapp keymap))
	((listp keymap)
	 (set (car keymap) nil)
	 (define-prefix-command (car keymap))
	 (define-key (symbol-value (caddr keymap)) (cadr keymap) (car keymap))
	 (setq keymap (symbol-value (car keymap)))))
  (let (key)
    (while plist
      (when (symbolp (setq key (pop plist)))
	(setq key (symbol-value key)))
      (if (or (not safe)
	      (eq (lookup-key keymap key) 'undefined))
	  (define-key keymap key (pop plist))
	(pop plist)))))

(defun gnus-completing-read-with-default (default prompt &rest args)
  ;; Like `completing-read', except that DEFAULT is the default argument.
  (let* ((prompt (if default
		     (concat prompt " (default " default ") ")
		   (concat prompt " ")))
	 (answer (apply 'completing-read prompt args)))
    (if (or (null answer) (zerop (length answer)))
	default
      answer)))

;; Two silly functions to ensure that all `y-or-n-p' questions clear
;; the echo area.
(defun gnus-y-or-n-p (prompt)
  (prog1
      (y-or-n-p prompt)
    (message "")))

(defun gnus-yes-or-no-p (prompt)
  (prog1
      (yes-or-no-p prompt)
    (message "")))

;; By Frank Schmitt <ich@Frank-Schmitt.net>. Allows to have
;; age-depending date representations. (e.g. just the time if it's
;; from today, the day of the week if it's within the last 7 days and
;; the full date if it's older)

(defun gnus-seconds-today ()
  "Return the number of seconds passed today."
  (let ((now (decode-time (current-time))))
    (+ (car now) (* (car (cdr now)) 60) (* (car (nthcdr 2 now)) 3600))))

(defun gnus-seconds-month ()
  "Return the number of seconds passed this month."
  (let ((now (decode-time (current-time))))
    (+ (car now) (* (car (cdr now)) 60) (* (car (nthcdr 2 now)) 3600)
       (* (- (car (nthcdr 3 now)) 1) 3600 24))))

(defun gnus-seconds-year ()
  "Return the number of seconds passed this year."
  (let ((now (decode-time (current-time)))
	(days (format-time-string "%j" (current-time))))
    (+ (car now) (* (car (cdr now)) 60) (* (car (nthcdr 2 now)) 3600)
       (* (- (string-to-number days) 1) 3600 24))))

(defvar gnus-user-date-format-alist
  '(((gnus-seconds-today) . "%k:%M")
    (604800 . "%a %k:%M")                   ;;that's one week
    ((gnus-seconds-month) . "%a %d")
    ((gnus-seconds-year) . "%b %d")
    (t . "%b %d '%y"))                      ;;this one is used when no
					    ;;other does match
  "Specifies date format depending on age of article.
This is an alist of items (AGE . FORMAT).  AGE can be a number (of
seconds) or a Lisp expression evaluating to a number.  When the age of
the article is less than this number, then use `format-time-string'
with the corresponding FORMAT for displaying the date of the article.
If AGE is not a number or a Lisp expression evaluating to a
non-number, then the corresponding FORMAT is used as a default value.

Note that the list is processed from the beginning, so it should be
sorted by ascending AGE.  Also note that items following the first
non-number AGE will be ignored.

You can use the functions `gnus-seconds-today', `gnus-seconds-month'
and `gnus-seconds-year' in the AGE spec.  They return the number of
seconds passed since the start of today, of this month, of this year,
respectively.")

(defun gnus-user-date (messy-date)
  "Format the messy-date according to gnus-user-date-format-alist.
Returns \"  ?  \" if there's bad input or if an other error occurs.
Input should look like this: \"Sun, 14 Oct 2001 13:34:39 +0200\"."
  (condition-case ()
      (let* ((messy-date (time-to-seconds (safe-date-to-time messy-date)))
	     (now (time-to-seconds (current-time)))
	     ;;If we don't find something suitable we'll use this one
	     (my-format "%b %d '%y"))
	(let* ((difference (- now messy-date))
	       (templist gnus-user-date-format-alist)
	       (top (eval (caar templist))))
	  (while (if (numberp top) (< top difference) (not top))
	    (progn
	      (setq templist (cdr templist))
	      (setq top (eval (caar templist)))))
	  (if (stringp (cdr (car templist)))
	      (setq my-format (cdr (car templist)))))
	(format-time-string (eval my-format) (seconds-to-time messy-date)))
    (error "  ?   ")))

(defun gnus-dd-mmm (messy-date)
  "Return a string like DD-MMM from a big messy string."
  (condition-case ()
      (format-time-string "%d-%b" (safe-date-to-time messy-date))
    (error "  -   ")))

(defmacro gnus-date-get-time (date)
  "Convert DATE string to Emacs time.
Cache the result as a text property stored in DATE."
  ;; Either return the cached value...
  `(let ((d ,date))
     (if (equal "" d)
	 '(0 0)
       (or (get-text-property 0 'gnus-time d)
	   ;; or compute the value...
	   (let ((time (safe-date-to-time d)))
	     ;; and store it back in the string.
	     (put-text-property 0 1 'gnus-time time d)
	     time)))))

(defsubst gnus-time-iso8601 (time)
  "Return a string of TIME in YYYYMMDDTHHMMSS format."
  (format-time-string "%Y%m%dT%H%M%S" time))

(defun gnus-date-iso8601 (date)
  "Convert the DATE to YYYYMMDDTHHMMSS."
  (condition-case ()
      (gnus-time-iso8601 (gnus-date-get-time date))
    (error "")))

(defun gnus-mode-string-quote (string)
  "Quote all \"%\"'s in STRING."
  (gnus-replace-in-string string "%" "%%"))

;; Make a hash table (default and minimum size is 256).
;; Optional argument HASHSIZE specifies the table size.
(defun gnus-make-hashtable (&optional hashsize)
  (make-vector (if hashsize (max (gnus-create-hash-size hashsize) 256) 256) 0))

;; Make a number that is suitable for hashing; bigger than MIN and
;; equal to some 2^x.  Many machines (such as sparcs) do not have a
;; hardware modulo operation, so they implement it in software.  On
;; many sparcs over 50% of the time to intern is spent in the modulo.
;; Yes, it's slower than actually computing the hash from the string!
;; So we use powers of 2 so people can optimize the modulo to a mask.
(defun gnus-create-hash-size (min)
  (let ((i 1))
    (while (< i min)
      (setq i (* 2 i)))
    i))

(defcustom gnus-verbose 7
  "*Integer that says how verbose Gnus should be.
The higher the number, the more messages Gnus will flash to say what
it's doing.  At zero, Gnus will be totally mute; at five, Gnus will
display most important messages; and at ten, Gnus will keep on
jabbering all the time."
  :group 'gnus-start
  :type 'integer)

(defun gnus-message (level &rest args)
  "If LEVEL is lower than `gnus-verbose' print ARGS using `message'.

Guideline for numbers:
1 - error messages, 3 - non-serious error messages, 5 - messages for things
that take a long time, 7 - not very important messages on stuff, 9 - messages
inside loops."
  (if (<= level gnus-verbose)
      (apply 'message args)
    ;; We have to do this format thingy here even if the result isn't
    ;; shown - the return value has to be the same as the return value
    ;; from `message'.
    (apply 'format args)))

(defun gnus-error (level &rest args)
  "Beep an error if LEVEL is equal to or less than `gnus-verbose'."
  (when (<= (floor level) gnus-verbose)
    (apply 'message args)
    (ding)
    (let (duration)
      (when (and (floatp level)
		 (not (zerop (setq duration (* 10 (- level (floor level)))))))
	(sit-for duration))))
  nil)

(defun gnus-split-references (references)
  "Return a list of Message-IDs in REFERENCES."
  (let ((beg 0)
	ids)
    (while (string-match "<[^<]+[^< \t]" references beg)
      (push (substring references (match-beginning 0) (setq beg (match-end 0)))
	    ids))
    (nreverse ids)))

(defsubst gnus-parent-id (references &optional n)
  "Return the last Message-ID in REFERENCES.
If N, return the Nth ancestor instead."
  (when (and references
	     (not (zerop (length references))))
    (if n
	(let ((ids (inline (gnus-split-references references))))
	  (while (nthcdr n ids)
	    (setq ids (cdr ids)))
	  (car ids))
      (when (string-match "\\(<[^<]+>\\)[ \t]*\\'" references)
	(match-string 1 references)))))

(defun gnus-buffer-live-p (buffer)
  "Say whether BUFFER is alive or not."
  (and buffer
       (get-buffer buffer)
       (buffer-name (get-buffer buffer))))

(defun gnus-horizontal-recenter ()
  "Recenter the current buffer horizontally."
  (if (< (current-column) (/ (window-width) 2))
      (set-window-hscroll (gnus-get-buffer-window (current-buffer) t) 0)
    (let* ((orig (point))
	   (end (window-end (gnus-get-buffer-window (current-buffer) t)))
	   (max 0))
      (when end
	;; Find the longest line currently displayed in the window.
	(goto-char (window-start))
	(while (and (not (eobp))
		    (< (point) end))
	  (end-of-line)
	  (setq max (max max (current-column)))
	  (forward-line 1))
	(goto-char orig)
	;; Scroll horizontally to center (sort of) the point.
	(if (> max (window-width))
	    (set-window-hscroll
	     (gnus-get-buffer-window (current-buffer) t)
	     (min (- (current-column) (/ (window-width) 3))
		  (+ 2 (- max (window-width)))))
	  (set-window-hscroll (gnus-get-buffer-window (current-buffer) t) 0))
	max))))

(defun gnus-read-event-char (&optional prompt)
  "Get the next event."
  (let ((event (read-event prompt)))
    ;; should be gnus-characterp, but this can't be called in XEmacs anyway
    (cons (and (numberp event) event) event)))

(defun gnus-sortable-date (date)
  "Make string suitable for sorting from DATE."
  (gnus-time-iso8601 (date-to-time date)))

(defun gnus-copy-file (file &optional to)
  "Copy FILE to TO."
  (interactive
   (list (read-file-name "Copy file: " default-directory)
	 (read-file-name "Copy file to: " default-directory)))
  (unless to
    (setq to (read-file-name "Copy file to: " default-directory)))
  (when (file-directory-p to)
    (setq to (concat (file-name-as-directory to)
		     (file-name-nondirectory file))))
  (copy-file file to))

(defvar gnus-work-buffer " *gnus work*")

(defun gnus-set-work-buffer ()
  "Put point in the empty Gnus work buffer."
  (if (get-buffer gnus-work-buffer)
      (progn
	(set-buffer gnus-work-buffer)
	(erase-buffer))
    (set-buffer (gnus-get-buffer-create gnus-work-buffer))
    (kill-all-local-variables)
    (mm-enable-multibyte)))

(defmacro gnus-group-real-name (group)
  "Find the real name of a foreign newsgroup."
  `(let ((gname ,group))
     (if (string-match "^[^:]+:" gname)
	 (substring gname (match-end 0))
       gname)))

(defun gnus-make-sort-function (funs)
  "Return a composite sort condition based on the functions in FUNS."
  (cond
   ;; Just a simple function.
   ((functionp funs) funs)
   ;; No functions at all.
   ((null funs) funs)
   ;; A list of functions.
   ((or (cdr funs)
	(listp (car funs)))
    (gnus-byte-compile
     `(lambda (t1 t2)
	,(gnus-make-sort-function-1 (reverse funs)))))
   ;; A list containing just one function.
   (t
    (car funs))))

(defun gnus-make-sort-function-1 (funs)
  "Return a composite sort condition based on the functions in FUNS."
  (let ((function (car funs))
	(first 't1)
	(last 't2))
    (when (consp function)
      (cond
       ;; Reversed spec.
       ((eq (car function) 'not)
	(setq function (cadr function)
	      first 't2
	      last 't1))
       ((functionp function)
	;; Do nothing.
	)
       (t
	(error "Invalid sort spec: %s" function))))
    (if (cdr funs)
	`(or (,function ,first ,last)
	     (and (not (,function ,last ,first))
		  ,(gnus-make-sort-function-1 (cdr funs))))
      `(,function ,first ,last))))

(defun gnus-turn-off-edit-menu (type)
  "Turn off edit menu in `gnus-TYPE-mode-map'."
  (define-key (symbol-value (intern (format "gnus-%s-mode-map" type)))
    [menu-bar edit] 'undefined))

(defmacro gnus-bind-print-variables (&rest forms)
  "Bind print-* variables and evaluate FORMS.
This macro is used with `prin1', `pp', etc. in order to ensure printed
Lisp objects are loadable.  Bind `print-quoted' and `print-readably'
to t, and `print-escape-multibyte', `print-escape-newlines',
`print-escape-nonascii', `print-length', `print-level' and
`print-string-length' to nil."
  `(let ((print-quoted t)
	 (print-readably t)
	 ;;print-circle
	 ;;print-continuous-numbering
	 print-escape-multibyte
	 print-escape-newlines
	 print-escape-nonascii
	 ;;print-gensym
	 print-length
	 print-level
	 print-string-length)
     ,@forms))

(defun gnus-prin1 (form)
  "Use `prin1' on FORM in the current buffer.
Bind `print-quoted' and `print-readably' to t, and `print-length' and
`print-level' to nil.  See also `gnus-bind-print-variables'."
  (gnus-bind-print-variables (prin1 form (current-buffer))))

(defun gnus-prin1-to-string (form)
  "The same as `prin1'.
Bind `print-quoted' and `print-readably' to t, and `print-length' and
`print-level' to nil.  See also `gnus-bind-print-variables'."
  (gnus-bind-print-variables (prin1-to-string form)))

(defun gnus-pp (form)
  "Use `pp' on FORM in the current buffer.
Bind `print-quoted' and `print-readably' to t, and `print-length' and
`print-level' to nil.  See also `gnus-bind-print-variables'."
  (gnus-bind-print-variables (pp form (current-buffer))))

(defun gnus-pp-to-string (form)
  "The same as `pp-to-string'.
Bind `print-quoted' and `print-readably' to t, and `print-length' and
`print-level' to nil.  See also `gnus-bind-print-variables'."
  (gnus-bind-print-variables (pp-to-string form)))

(defun gnus-make-directory (directory)
  "Make DIRECTORY (and all its parents) if it doesn't exist."
  (require 'nnmail)
  (let ((file-name-coding-system nnmail-pathname-coding-system))
    (when (and directory
	       (not (file-exists-p directory)))
      (make-directory directory t)))
  t)

(defun gnus-write-buffer (file)
  "Write the current buffer's contents to FILE."
  ;; Make sure the directory exists.
  (gnus-make-directory (file-name-directory file))
  (let ((file-name-coding-system nnmail-pathname-coding-system))
    ;; Write the buffer.
    (write-region (point-min) (point-max) file nil 'quietly)))

(defun gnus-delete-file (file)
  "Delete FILE if it exists."
  (when (file-exists-p file)
    (delete-file file)))

(defun gnus-delete-directory (directory)
  "Delete files in DIRECTORY.  Subdirectories remain.
If there's no subdirectory, delete DIRECTORY as well."
  (when (file-directory-p directory)
    (let ((files (directory-files
		  directory t "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"))
	  file dir)
      (while files
	(setq file (pop files))
	(if (eq t (car (file-attributes file)))
	    ;; `file' is a subdirectory.
	    (setq dir t)
	  ;; `file' is a file or a symlink.
	  (delete-file file)))
      (unless dir
	(delete-directory directory)))))

(defun gnus-strip-whitespace (string)
  "Return STRING stripped of all whitespace."
  (while (string-match "[\r\n\t ]+" string)
    (setq string (replace-match "" t t string)))
  string)

(defsubst gnus-put-text-property-excluding-newlines (beg end prop val)
  "The same as `put-text-property', but don't put this prop on any newlines in the region."
  (save-match-data
    (save-excursion
      (save-restriction
	(goto-char beg)
	(while (re-search-forward gnus-emphasize-whitespace-regexp end 'move)
	  (gnus-put-text-property beg (match-beginning 0) prop val)
	  (setq beg (point)))
	(gnus-put-text-property beg (point) prop val)))))

(defsubst gnus-put-overlay-excluding-newlines (beg end prop val)
  "The same as `put-text-property', but don't put this prop on any newlines in the region."
  (save-match-data
    (save-excursion
      (save-restriction
	(goto-char beg)
	(while (re-search-forward gnus-emphasize-whitespace-regexp end 'move)
	  (gnus-overlay-put
	   (gnus-make-overlay beg (match-beginning 0))
	   prop val)
	  (setq beg (point)))
	(gnus-overlay-put (gnus-make-overlay beg (point)) prop val)))))

(defun gnus-put-text-property-excluding-characters-with-faces (beg end
								   prop val)
  "The same as `put-text-property', but don't put props on characters with the `gnus-face' property."
  (let ((b beg))
    (while (/= b end)
      (when (get-text-property b 'gnus-face)
	(setq b (next-single-property-change b 'gnus-face nil end)))
      (when (/= b end)
	(inline
	  (gnus-put-text-property
	   b (setq b (next-single-property-change b 'gnus-face nil end))
	   prop val))))))

(defmacro gnus-faces-at (position)
  "Return a list of faces at POSITION."
  (if (featurep 'xemacs)
      `(let ((pos ,position))
	 (mapcar-extents 'extent-face
			 nil (current-buffer) pos pos nil 'face))
    `(let ((pos ,position))
       (delq nil (cons (get-text-property pos 'face)
		       (mapcar
			(lambda (overlay)
			  (overlay-get overlay 'face))
			(overlays-at pos)))))))

;;; Protected and atomic operations.  dmoore@ucsd.edu 21.11.1996
;;; The primary idea here is to try to protect internal datastructures
;;; from becoming corrupted when the user hits C-g, or if a hook or
;;; similar blows up.  Often in Gnus multiple tables/lists need to be
;;; updated at the same time, or information can be lost.

(defvar gnus-atomic-be-safe t
  "If t, certain operations will be protected from interruption by C-g.")

(defmacro gnus-atomic-progn (&rest forms)
  "Evaluate FORMS atomically, which means to protect the evaluation
from being interrupted by the user.  An error from the forms themselves
will return without finishing the operation.  Since interrupts from
the user are disabled, it is recommended that only the most minimal
operations are performed by FORMS.  If you wish to assign many
complicated values atomically, compute the results into temporary
variables and then do only the assignment atomically."
  `(let ((inhibit-quit gnus-atomic-be-safe))
     ,@forms))

(put 'gnus-atomic-progn 'lisp-indent-function 0)

(defmacro gnus-atomic-progn-assign (protect &rest forms)
  "Evaluate FORMS, but insure that the variables listed in PROTECT
are not changed if anything in FORMS signals an error or otherwise
non-locally exits.  The variables listed in PROTECT are updated atomically.
It is safe to use gnus-atomic-progn-assign with long computations.

Note that if any of the symbols in PROTECT were unbound, they will be
set to nil on a successful assignment.  In case of an error or other
non-local exit, it will still be unbound."
  (let* ((temp-sym-map (mapcar (lambda (x) (list (make-symbol
						  (concat (symbol-name x)
							  "-tmp"))
						 x))
			       protect))
	 (sym-temp-map (mapcar (lambda (x) (list (cadr x) (car x)))
			       temp-sym-map))
	 (temp-sym-let (mapcar (lambda (x) (list (car x)
						 `(and (boundp ',(cadr x))
						       ,(cadr x))))
			       temp-sym-map))
	 (sym-temp-let sym-temp-map)
	 (temp-sym-assign (apply 'append temp-sym-map))
	 (sym-temp-assign (apply 'append sym-temp-map))
	 (result (make-symbol "result-tmp")))
    `(let (,@temp-sym-let
	   ,result)
       (let ,sym-temp-let
	 (setq ,result (progn ,@forms))
	 (setq ,@temp-sym-assign))
       (let ((inhibit-quit gnus-atomic-be-safe))
	 (setq ,@sym-temp-assign))
       ,result)))

(put 'gnus-atomic-progn-assign 'lisp-indent-function 1)
;(put 'gnus-atomic-progn-assign 'edebug-form-spec '(sexp body))

(defmacro gnus-atomic-setq (&rest pairs)
  "Similar to setq, except that the real symbols are only assigned when
there are no errors.  And when the real symbols are assigned, they are
done so atomically.  If other variables might be changed via side-effect,
see gnus-atomic-progn-assign.  It is safe to use gnus-atomic-setq
with potentially long computations."
  (let ((tpairs pairs)
	syms)
    (while tpairs
      (push (car tpairs) syms)
      (setq tpairs (cddr tpairs)))
    `(gnus-atomic-progn-assign ,syms
       (setq ,@pairs))))

;(put 'gnus-atomic-setq 'edebug-form-spec '(body))


;;; Functions for saving to babyl/mail files.

(eval-when-compile
  (condition-case nil
      (progn
	(require 'rmail)
	(autoload 'rmail-update-summary "rmailsum"))
    (error
     (define-compiler-macro rmail-select-summary (&rest body)
       ;; Rmail of the XEmacs version is supplied by the package, and
       ;; requires tm and apel packages.  However, there may be those
       ;; who haven't installed those packages.  This macro helps such
       ;; people even if they install those packages later.
       `(eval '(rmail-select-summary ,@body)))
     ;; If there's rmail but there's no tm (or there's apel of the
     ;; mainstream, not the XEmacs version), loading rmail of the XEmacs
     ;; version fails halfway, however it provides the rmail-select-summary
     ;; macro which uses the following functions:
     (autoload 'rmail-summary-displayed "rmail")
     (autoload 'rmail-maybe-display-summary "rmail")))
  (defvar rmail-default-rmail-file)
  (defvar mm-text-coding-system))

(defun gnus-output-to-rmail (filename &optional ask)
  "Append the current article to an Rmail file named FILENAME."
  (require 'rmail)
  (require 'mm-util)
  ;; Most of these codes are borrowed from rmailout.el.
  (setq filename (expand-file-name filename))
  (setq rmail-default-rmail-file filename)
  (let ((artbuf (current-buffer))
	(tmpbuf (get-buffer-create " *Gnus-output*")))
    (save-excursion
      (or (get-file-buffer filename)
	  (file-exists-p filename)
	  (if (or (not ask)
		  (gnus-yes-or-no-p
		   (concat "\"" filename "\" does not exist, create it? ")))
	      (let ((file-buffer (create-file-buffer filename)))
		(save-excursion
		  (set-buffer file-buffer)
		  (rmail-insert-rmail-file-header)
		  (let ((require-final-newline nil)
			(coding-system-for-write mm-text-coding-system))
		    (gnus-write-buffer filename)))
		(kill-buffer file-buffer))
	    (error "Output file does not exist")))
      (set-buffer tmpbuf)
      (erase-buffer)
      (insert-buffer-substring artbuf)
      (gnus-convert-article-to-rmail)
      ;; Decide whether to append to a file or to an Emacs buffer.
      (let ((outbuf (get-file-buffer filename)))
	(if (not outbuf)
	    (let ((file-name-coding-system nnmail-pathname-coding-system))
	      (mm-append-to-file (point-min) (point-max) filename))
	  ;; File has been visited, in buffer OUTBUF.
	  (set-buffer outbuf)
	  (let ((buffer-read-only nil)
		(msg (and (boundp 'rmail-current-message)
			  (symbol-value 'rmail-current-message))))
	    ;; If MSG is non-nil, buffer is in RMAIL mode.
	    (when msg
	      (widen)
	      (narrow-to-region (point-max) (point-max)))
	    (insert-buffer-substring tmpbuf)
	    (when msg
	      (goto-char (point-min))
	      (widen)
	      (search-backward "\n\^_")
	      (narrow-to-region (point) (point-max))
	      (rmail-count-new-messages t)
	      (when (rmail-summary-exists)
		(rmail-select-summary
		 (rmail-update-summary)))
	      (rmail-count-new-messages t)
	      (rmail-show-message msg))
	    (save-buffer)))))
    (kill-buffer tmpbuf)))

(defun gnus-output-to-mail (filename &optional ask)
  "Append the current article to a mail file named FILENAME."
  (setq filename (expand-file-name filename))
  (let ((artbuf (current-buffer))
	(tmpbuf (get-buffer-create " *Gnus-output*")))
    (save-excursion
      ;; Create the file, if it doesn't exist.
      (when (and (not (get-file-buffer filename))
		 (not (file-exists-p filename)))
	(if (or (not ask)
		(gnus-y-or-n-p
		 (concat "\"" filename "\" does not exist, create it? ")))
	    (let ((file-buffer (create-file-buffer filename)))
	      (save-excursion
		(set-buffer file-buffer)
		(let ((require-final-newline nil)
		      (coding-system-for-write mm-text-coding-system))
		  (gnus-write-buffer filename)))
	      (kill-buffer file-buffer))
	  (error "Output file does not exist")))
      (set-buffer tmpbuf)
      (erase-buffer)
      (insert-buffer-substring artbuf)
      (goto-char (point-min))
      (if (looking-at "From ")
	  (forward-line 1)
	(insert "From nobody " (current-time-string) "\n"))
      (let (case-fold-search)
	(while (re-search-forward "^From " nil t)
	  (beginning-of-line)
	  (insert ">")))
      ;; Decide whether to append to a file or to an Emacs buffer.
      (let ((outbuf (get-file-buffer filename)))
	(if (not outbuf)
	    (let ((buffer-read-only nil))
	      (save-excursion
		(goto-char (point-max))
		(forward-char -2)
		(unless (looking-at "\n\n")
		  (goto-char (point-max))
		  (unless (bolp)
		    (insert "\n"))
		  (insert "\n"))
		(goto-char (point-max))
		(let ((file-name-coding-system nnmail-pathname-coding-system))
		  (mm-append-to-file (point-min) (point-max) filename))))
	  ;; File has been visited, in buffer OUTBUF.
	  (set-buffer outbuf)
	  (let ((buffer-read-only nil))
	    (goto-char (point-max))
	    (unless (eobp)
	      (insert "\n"))
	    (insert "\n")
	    (insert-buffer-substring tmpbuf)))))
    (kill-buffer tmpbuf)))

(defun gnus-convert-article-to-rmail ()
  "Convert article in current buffer to Rmail message format."
  (let ((buffer-read-only nil))
    ;; Convert article directly into Babyl format.
    (goto-char (point-min))
    (insert "\^L\n0, unseen,,\n*** EOOH ***\n")
    (while (search-forward "\n\^_" nil t) ;single char
      (replace-match "\n^_" t t))	;2 chars: "^" and "_"
    (goto-char (point-max))
    (insert "\^_")))

(defun gnus-map-function (funs arg)
  "Apply the result of the first function in FUNS to the second, and so on.
ARG is passed to the first function."
  (while funs
    (setq arg (funcall (pop funs) arg)))
  arg)

(defun gnus-run-hooks (&rest funcs)
  "Does the same as `run-hooks', but saves the current buffer."
  (save-current-buffer
    (apply 'run-hooks funcs)))

(defun gnus-run-mode-hooks (&rest funcs)
  "Run `run-mode-hooks' if it is available, otherwise `run-hooks'.
This function saves the current buffer."
  (if (fboundp 'run-mode-hooks)
      (save-current-buffer (apply 'run-mode-hooks funcs))
    (save-current-buffer (apply 'run-hooks funcs))))

;;; Various

(defvar gnus-group-buffer)		; Compiler directive
(defun gnus-alive-p ()
  "Say whether Gnus is running or not."
  (and (boundp 'gnus-group-buffer)
       (get-buffer gnus-group-buffer)
       (save-excursion
	 (set-buffer gnus-group-buffer)
	 (eq major-mode 'gnus-group-mode))))

(defun gnus-remove-duplicates (list)
  (let (new)
    (while list
      (or (member (car list) new)
	  (setq new (cons (car list) new)))
      (setq list (cdr list)))
    (nreverse new)))

(defun gnus-remove-if (predicate list)
  "Return a copy of LIST with all items satisfying PREDICATE removed."
  (let (out)
    (while list
      (unless (funcall predicate (car list))
	(push (car list) out))
      (setq list (cdr list)))
    (nreverse out)))

(if (fboundp 'assq-delete-all)
    (defalias 'gnus-delete-alist 'assq-delete-all)
  (defun gnus-delete-alist (key alist)
    "Delete from ALIST all elements whose car is KEY.
Return the modified alist."
    (let (entry)
      (while (setq entry (assq key alist))
	(setq alist (delq entry alist)))
      alist)))

(defmacro gnus-pull (key alist &optional assoc-p)
  "Modify ALIST to be without KEY."
  (unless (symbolp alist)
    (error "Not a symbol: %s" alist))
  (let ((fun (if assoc-p 'assoc 'assq)))
    `(setq ,alist (delq (,fun ,key ,alist) ,alist))))

(defun gnus-globalify-regexp (re)
  "Return a regexp that matches a whole line, iff RE matches a part of it."
  (concat (unless (string-match "^\\^" re) "^.*")
	  re
	  (unless (string-match "\\$$" re) ".*$")))

(defun gnus-set-window-start (&optional point)
  "Set the window start to POINT, or (point) if nil."
  (let ((win (gnus-get-buffer-window (current-buffer) t)))
    (when win
      (set-window-start win (or point (point))))))

(defun gnus-annotation-in-region-p (b e)
  (if (= b e)
      (eq (cadr (memq 'gnus-undeletable (text-properties-at b))) t)
    (text-property-any b e 'gnus-undeletable t)))

(defun gnus-or (&rest elems)
  "Return non-nil if any of the elements are non-nil."
  (catch 'found
    (while elems
      (when (pop elems)
	(throw 'found t)))))

(defun gnus-and (&rest elems)
  "Return non-nil if all of the elements are non-nil."
  (catch 'found
    (while elems
      (unless (pop elems)
	(throw 'found nil)))
    t))

(defun gnus-write-active-file (file hashtb &optional full-names)
  (let ((coding-system-for-write nnmail-active-file-coding-system))
    (with-temp-file file
      (mapatoms
       (lambda (sym)
	 (when (and sym
		    (boundp sym)
		    (symbol-value sym))
	   (insert (format "%S %d %d y\n"
			   (if full-names
			       sym
			     (intern (gnus-group-real-name (symbol-name sym))))
			   (or (cdr (symbol-value sym))
			       (car (symbol-value sym)))
			   (car (symbol-value sym))))))
       hashtb)
      (goto-char (point-max))
      (while (search-backward "\\." nil t)
	(delete-char 1)))))

;; Fixme: Why not use `with-output-to-temp-buffer'?
(defmacro gnus-with-output-to-file (file &rest body)
  (let ((buffer (make-symbol "output-buffer"))
        (size (make-symbol "output-buffer-size"))
        (leng (make-symbol "output-buffer-length"))
        (append (make-symbol "output-buffer-append")))
    `(let* ((,size 131072)
            (,buffer (make-string ,size 0))
            (,leng 0)
            (,append nil)
            (standard-output
	     (lambda (c)
               (aset ,buffer ,leng c)

	       (if (= ,size (setq ,leng (1+ ,leng)))
		   (progn (write-region ,buffer nil ,file ,append 'no-msg)
			  (setq ,leng 0
				,append t))))))
       ,@body
       (when (> ,leng 0)
         (let ((coding-system-for-write 'no-conversion))
	 (write-region (substring ,buffer 0 ,leng) nil ,file
		       ,append 'no-msg))))))

(put 'gnus-with-output-to-file 'lisp-indent-function 1)
(put 'gnus-with-output-to-file 'edebug-form-spec '(form body))

(if (fboundp 'union)
    (defalias 'gnus-union 'union)
  (defun gnus-union (l1 l2)
    "Set union of lists L1 and L2."
    (cond ((null l1) l2)
	  ((null l2) l1)
	  ((equal l1 l2) l1)
	  (t
	   (or (>= (length l1) (length l2))
	       (setq l1 (prog1 l2 (setq l2 l1))))
	   (while l2
	     (or (member (car l2) l1)
		 (push (car l2) l1))
	     (pop l2))
	   l1))))

(defun gnus-add-text-properties-when
  (property value start end properties &optional object)
  "Like `gnus-add-text-properties', only applied on where PROPERTY is VALUE."
  (let (point)
    (while (and start
		(< start end) ;; XEmacs will loop for every when start=end.
		(setq point (text-property-not-all start end property value)))
      (gnus-add-text-properties start point properties object)
      (setq start (text-property-any point end property value)))
    (if start
	(gnus-add-text-properties start end properties object))))

(defun gnus-remove-text-properties-when
  (property value start end properties &optional object)
  "Like `remove-text-properties', only applied on where PROPERTY is VALUE."
  (let (point)
    (while (and start
		(< start end)
		(setq point (text-property-not-all start end property value)))
      (remove-text-properties start point properties object)
      (setq start (text-property-any point end property value)))
    (if start
	(remove-text-properties start end properties object))
    t))

;; This might use `compare-strings' to reduce consing in the
;; case-insensitive case, but it has to cope with null args.
;; (`string-equal' uses symbol print names.)
(defun gnus-string-equal (x y)
  "Like `string-equal', except it compares case-insensitively."
  (and (= (length x) (length y))
       (or (string-equal x y)
	   (string-equal (downcase x) (downcase y)))))

(defcustom gnus-use-byte-compile t
  "If non-nil, byte-compile crucial run-time code.
Setting it to nil has no effect after the first time `gnus-byte-compile'
is run."
  :type 'boolean
  :version "22.1"
  :group 'gnus-various)

(defun gnus-byte-compile (form)
  "Byte-compile FORM if `gnus-use-byte-compile' is non-nil."
  (if gnus-use-byte-compile
      (progn
	(condition-case nil
	    ;; Work around a bug in XEmacs 21.4
	    (require 'byte-optimize)
	  (error))
	(require 'bytecomp)
	(defalias 'gnus-byte-compile
	  (lambda (form)
	    (let ((byte-compile-warnings '(unresolved callargs redefine)))
	      (byte-compile form))))
	(gnus-byte-compile form))
    form))

(defun gnus-remassoc (key alist)
  "Delete by side effect any elements of LIST whose car is `equal' to KEY.
The modified LIST is returned.  If the first member
of LIST has a car that is `equal' to KEY, there is no way to remove it
by side effect; therefore, write `(setq foo (gnus-remassoc key foo))' to be
sure of changing the value of `foo'."
  (when alist
    (if (equal key (caar alist))
	(cdr alist)
      (setcdr alist (gnus-remassoc key (cdr alist)))
      alist)))

(defun gnus-update-alist-soft (key value alist)
  (if value
      (cons (cons key value) (gnus-remassoc key alist))
    (gnus-remassoc key alist)))

(defun gnus-create-info-command (node)
  "Create a command that will go to info NODE."
  `(lambda ()
     (interactive)
     ,(concat "Enter the info system at node " node)
     (Info-goto-node ,node)
     (setq gnus-info-buffer (current-buffer))
     (gnus-configure-windows 'info)))

(defun gnus-not-ignore (&rest args)
  t)

(defvar gnus-directory-sep-char-regexp "/"
  "The regexp of directory separator character.
If you find some problem with the directory separator character, try
\"[/\\\\\]\" for some systems.")

(defun gnus-url-unhex (x)
  (if (> x ?9)
      (if (>= x ?a)
	  (+ 10 (- x ?a))
	(+ 10 (- x ?A)))
    (- x ?0)))

;; Fixme: Do it like QP.
(defun gnus-url-unhex-string (str &optional allow-newlines)
  "Remove %XX, embedded spaces, etc in a url.
If optional second argument ALLOW-NEWLINES is non-nil, then allow the
decoding of carriage returns and line feeds in the string, which is normally
forbidden in URL encoding."
  (let ((tmp "")
	(case-fold-search t))
    (while (string-match "%[0-9a-f][0-9a-f]" str)
      (let* ((start (match-beginning 0))
	     (ch1 (gnus-url-unhex (elt str (+ start 1))))
	     (code (+ (* 16 ch1)
		      (gnus-url-unhex (elt str (+ start 2))))))
	(setq tmp (concat
		   tmp (substring str 0 start)
		   (cond
		    (allow-newlines
		     (char-to-string code))
		    ((or (= code ?\n) (= code ?\r))
		     " ")
		    (t (char-to-string code))))
	      str (substring str (match-end 0)))))
    (setq tmp (concat tmp str))
    tmp))

(defun gnus-make-predicate (spec)
  "Transform SPEC into a function that can be called.
SPEC is a predicate specifier that contains stuff like `or', `and',
`not', lists and functions.  The functions all take one parameter."
  `(lambda (elem) ,(gnus-make-predicate-1 spec)))

(defun gnus-make-predicate-1 (spec)
  (cond
   ((symbolp spec)
    `(,spec elem))
   ((listp spec)
    (if (memq (car spec) '(or and not))
	`(,(car spec) ,@(mapcar 'gnus-make-predicate-1 (cdr spec)))
      (error "Invalid predicate specifier: %s" spec)))))

(defun gnus-local-map-property (map)
  "Return a list suitable for a text property list specifying keymap MAP."
  (cond
   ((featurep 'xemacs)
    (list 'keymap map))
   ((>= emacs-major-version 21)
    (list 'keymap map))
   (t
    (list 'local-map map))))

(defmacro gnus-completing-read-maybe-default (prompt table &optional predicate
					      require-match initial-contents
					      history default)
  "Like `completing-read', allowing for non-existent 7th arg in older XEmacsen."
  `(completing-read ,prompt ,table ,predicate ,require-match
                    ,initial-contents ,history
                    ,@(if (and (featurep 'xemacs) (< emacs-minor-version 2))
                          ()
                        (list default))))

(defun gnus-completing-read (prompt table &optional predicate require-match
				    history)
  (when (and history
	     (not (boundp history)))
    (set history nil))
  (gnus-completing-read-maybe-default
   (if (symbol-value history)
       (concat prompt " (" (car (symbol-value history)) "): ")
     (concat prompt ": "))
   table
   predicate
   require-match
   nil
   history
   (car (symbol-value history))))

(defun gnus-graphic-display-p ()
  (or (and (fboundp 'display-graphic-p)
	   (display-graphic-p))
      ;;;!!!This is bogus.  Fixme!
      (and (featurep 'xemacs)
	   t)))

(put 'gnus-parse-without-error 'lisp-indent-function 0)
(put 'gnus-parse-without-error 'edebug-form-spec '(body))

(defmacro gnus-parse-without-error (&rest body)
  "Allow continuing onto the next line even if an error occurs."
  `(while (not (eobp))
     (condition-case ()
	 (progn
	   ,@body
	   (goto-char (point-max)))
       (error
	(gnus-error 4 "Invalid data on line %d"
		    (count-lines (point-min) (point)))
	(forward-line 1)))))

(defun gnus-cache-file-contents (file variable function)
  "Cache the contents of FILE in VARIABLE.  The contents come from FUNCTION."
  (let ((time (nth 5 (file-attributes file)))
	contents value)
    (if (or (null (setq value (symbol-value variable)))
	    (not (equal (car value) file))
	    (not (equal (nth 1 value) time)))
	(progn
	  (setq contents (funcall function file))
	  (set variable (list file time contents))
	  contents)
      (nth 2 value))))

(defun gnus-multiple-choice (prompt choice &optional idx)
  "Ask user a multiple choice question.
CHOICE is a list of the choice char and help message at IDX."
  (let (tchar buf)
    (save-window-excursion
      (save-excursion
	(while (not tchar)
	  (message "%s (%s): "
		   prompt
		   (concat
		    (mapconcat (lambda (s) (char-to-string (car s)))
			       choice ", ") ", ?"))
	  (setq tchar (read-char))
	  (when (not (assq tchar choice))
	    (setq tchar nil)
	    (setq buf (get-buffer-create "*Gnus Help*"))
	    (pop-to-buffer buf)
	    (fundamental-mode)		; for Emacs 20.4+
	    (buffer-disable-undo)
	    (erase-buffer)
	    (insert prompt ":\n\n")
	    (let ((max -1)
		  (list choice)
		  (alist choice)
		  (idx (or idx 1))
		  (i 0)
		  n width pad format)
	      ;; find the longest string to display
	      (while list
		(setq n (length (nth idx (car list))))
		(unless (> max n)
		  (setq max n))
		(setq list (cdr list)))
	      (setq max (+ max 4))	; %c, `:', SPACE, a SPACE at end
	      (setq n (/ (1- (window-width)) max)) ; items per line
	      (setq width (/ (1- (window-width)) n)) ; width of each item
	      ;; insert `n' items, each in a field of width `width'
	      (while alist
		(if (< i n)
		    ()
		  (setq i 0)
		  (delete-char -1)		; the `\n' takes a char
		  (insert "\n"))
		(setq pad (- width 3))
		(setq format (concat "%c: %-" (int-to-string pad) "s"))
		(insert (format format (caar alist) (nth idx (car alist))))
		(setq alist (cdr alist))
		(setq i (1+ i))))))))
    (if (buffer-live-p buf)
	(kill-buffer buf))
    tchar))

(defun gnus-select-frame-set-input-focus (frame)
  "Select FRAME, raise it, and set input focus, if possible."
  (cond ((featurep 'xemacs)
	 (raise-frame frame)
	 (select-frame frame)
	 (focus-frame frame))
	;; The function `select-frame-set-input-focus' won't set
	;; the input focus under Emacs 21.2 and X window system.
	;;((fboundp 'select-frame-set-input-focus)
	;; (defalias 'gnus-select-frame-set-input-focus
	;;   'select-frame-set-input-focus)
	;; (select-frame-set-input-focus frame))
	(t
	 (raise-frame frame)
	 (select-frame frame)
	 (cond ((and (eq window-system 'x)
		     (fboundp 'x-focus-frame))
		(x-focus-frame frame))
	       ((eq window-system 'w32)
		(w32-focus-frame frame)))
	 (when focus-follows-mouse
	   (set-mouse-position frame (1- (frame-width frame)) 0)))))

(defun gnus-frame-or-window-display-name (object)
  "Given a frame or window, return the associated display name.
Return nil otherwise."
  (if (featurep 'xemacs)
      (device-connection (dfw-device object))
    (if (or (framep object)
	    (and (windowp object)
		 (setq object (window-frame object))))
	(let ((display (frame-parameter object 'display)))
	  (if (and (stringp display)
		   ;; Exclude invalid display names.
		   (string-match "\\`[^:]*:[0-9]+\\(\\.[0-9]+\\)?\\'"
				 display))
	      display)))))

;; Fixme: This has only one use (in gnus-agent), which isn't worthwhile.
(defmacro gnus-mapcar (function seq1 &rest seqs2_n)
  "Apply FUNCTION to each element of the sequences, and make a list of the results.
If there are several sequences, FUNCTION is called with that many arguments,
and mapping stops as soon as the shortest sequence runs out.  With just one
sequence, this is like `mapcar'.  With several, it is like the Common Lisp
`mapcar' function extended to arbitrary sequence types."

  (if seqs2_n
      (let* ((seqs (cons seq1 seqs2_n))
	     (cnt 0)
	     (heads (mapcar (lambda (seq)
			      (make-symbol (concat "head"
						   (int-to-string
						    (setq cnt (1+ cnt))))))
			    seqs))
	     (result (make-symbol "result"))
	     (result-tail (make-symbol "result-tail")))
	`(let* ,(let* ((bindings (cons nil nil))
		       (heads heads))
		  (nconc bindings (list (list result '(cons nil nil))))
		  (nconc bindings (list (list result-tail result)))
		  (while heads
		    (nconc bindings (list (list (pop heads) (pop seqs)))))
		  (cdr bindings))
	   (while (and ,@heads)
	     (setcdr ,result-tail (cons (funcall ,function
						 ,@(mapcar (lambda (h) (list 'car h))
							   heads))
					nil))
	     (setq ,result-tail (cdr ,result-tail)
		   ,@(apply 'nconc (mapcar (lambda (h) (list h (list 'cdr h))) heads))))
	   (cdr ,result)))
    `(mapcar ,function ,seq1)))

(if (fboundp 'merge)
    (defalias 'gnus-merge 'merge)
  ;; Adapted from cl-seq.el
  (defun gnus-merge (type list1 list2 pred)
    "Destructively merge lists LIST1 and LIST2 to produce a new list.
Argument TYPE is for compatibility and ignored.
Ordering of the elements is preserved according to PRED, a `less-than'
predicate on the elements."
    (let ((res nil))
      (while (and list1 list2)
	(if (funcall pred (car list2) (car list1))
	    (push (pop list2) res)
	  (push (pop list1) res)))
      (nconc (nreverse res) list1 list2))))

(eval-when-compile
  (defvar xemacs-codename))

(defun gnus-emacs-version ()
  "Stringified Emacs version."
  (let ((system-v
	 (cond
	  ((eq gnus-user-agent 'emacs-gnus-config)
	   system-configuration)
	  ((eq gnus-user-agent 'emacs-gnus-type)
	   (symbol-name system-type))
	  (t nil))))
    (cond
     ((eq gnus-user-agent 'gnus)
      nil)
     ((string-match "^\\(\\([.0-9]+\\)*\\)\\.[0-9]+$" emacs-version)
      (concat "Emacs/" (match-string 1 emacs-version)
	      (if system-v
		  (concat " (" system-v ")")
		"")))
     ((string-match
       "\\([A-Z]*[Mm][Aa][Cc][Ss]\\)[^(]*\\(\\((beta.*)\\|'\\)\\)?"
       emacs-version)
      (concat
       (match-string 1 emacs-version)
       (format "/%d.%d" emacs-major-version emacs-minor-version)
       (if (match-beginning 3)
	   (match-string 3 emacs-version)
	 "")
       (if (boundp 'xemacs-codename)
	   (concat
	    " (" xemacs-codename
	    (if system-v
		(concat ", " system-v ")")
	      ")"))
	 "")))
     (t emacs-version))))

(defun gnus-rename-file (old-path new-path &optional trim)
  "Rename OLD-PATH as NEW-PATH.  If TRIM, recursively delete
empty directories from OLD-PATH."
  (when (file-exists-p old-path)
    (let* ((old-dir (file-name-directory old-path))
	   (old-name (file-name-nondirectory old-path))
	   (new-dir (file-name-directory new-path))
	   (new-name (file-name-nondirectory new-path))
	   temp)
      (gnus-make-directory new-dir)
      (rename-file old-path new-path t)
      (when trim
	(while (progn (setq temp (directory-files old-dir))
		      (while (member (car temp) '("." ".."))
			(setq temp (cdr temp)))
		      (= (length temp) 0))
	  (delete-directory old-dir)
	  (setq old-dir (file-name-as-directory
			 (file-truename
			  (concat old-dir "..")))))))))

(if (fboundp 'set-process-query-on-exit-flag)
    (defalias 'gnus-set-process-query-on-exit-flag
      'set-process-query-on-exit-flag)
  (defalias 'gnus-set-process-query-on-exit-flag
    'process-kill-without-query))

(defun gnus-beginning-of-window ()
  "Move point to the beginning of the window."
  (move-to-window-line
   (if (featurep 'xemacs)
       0
     (min scroll-margin
	  (max 1 (- (window-height)
		    (if mode-line-format 1 0)
		    (if (and (boundp 'header-line-format)
			     (symbol-value 'header-line-format))
			1 0)))))))

(defun gnus-end-of-window ()
  "Move point to the end of the window."
  (move-to-window-line
   (if (featurep 'xemacs)
       -1
     (max (- -1 scroll-margin)
	  (- -1 (max 1 (- (window-height)
			  (if mode-line-format 1 0)
			  (if (and (boundp 'header-line-format)
				   (symbol-value 'header-line-format))
			      1 0))))))))

(provide 'gnus-util)

;;; arch-tag: f94991af-d32b-4c97-8c26-ca12a934de49
;;; gnus-util.el ends here
