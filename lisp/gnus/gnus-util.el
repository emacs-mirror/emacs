;;; gnus-util.el --- utility functions for Gnus
;; Copyright (C) 1996,97 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Nothing in this file depends on any other parts of Gnus -- all
;; functions and macros in this file are utility functions that are
;; used by Gnus and may be used by any other package without loading
;; Gnus first.

;;; Code:

(require 'custom)
(eval-when-compile (require 'cl))
(require 'nnheader)
(require 'timezone)
(require 'message)

(eval-and-compile
  (autoload 'nnmail-date-to-time "nnmail"))

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
            (,w (get-buffer-window ,buf 'visible)))
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

;; modified by MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;   function `substring' might cut on a middle of multi-octet
;;   character.
(defun gnus-truncate-string (str width)
  (substring str 0 width))

;; Added by Geoffrey T. Dairiki <dairiki@u.washington.edu>.  A safe way
;; to limit the length of a string.  This function is necessary since
;; `(substr "abc" 0 30)' pukes with "Args out of range".
(defsubst gnus-limit-string (str width)
  (if (> (length str) width)
      (substring str 0 width)
    str))

(defsubst gnus-functionp (form)
  "Return non-nil if FORM is funcallable."
  (or (and (symbolp form) (fboundp form))
      (and (listp form) (eq (car form) 'lambda))
      (compiled-function-p form)))

(defsubst gnus-goto-char (point)
  (and point (goto-char point)))

(defmacro gnus-buffer-exists-p (buffer)
  `(let ((buffer ,buffer))
     (when buffer
       (funcall (if (stringp buffer) 'get-buffer 'buffer-name)
		buffer))))

(defmacro gnus-kill-buffer (buffer)
  `(let ((buf ,buffer))
     (when (gnus-buffer-exists-p buf)
       (kill-buffer buf))))

(if (fboundp 'point-at-bol)
    (fset 'gnus-point-at-bol 'point-at-bol)
  (defun gnus-point-at-bol ()
    "Return point at the beginning of the line."
    (let ((p (point)))
      (beginning-of-line)
      (prog1
	  (point)
	(goto-char p)))))

(if (fboundp 'point-at-eol)
    (fset 'gnus-point-at-eol 'point-at-eol)
  (defun gnus-point-at-eol ()
    "Return point at the end of the line."
    (let ((p (point)))
      (end-of-line)
      (prog1
	  (point)
	(goto-char p)))))

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
  `(delete-region (progn (beginning-of-line) (point))
		  (progn (forward-line ,(or n 1)) (point))))

(defun gnus-byte-code (func)
  "Return a form that can be `eval'ed based on FUNC."
  (let ((fval (symbol-function func)))
    (if (compiled-function-p fval)
	(let ((flist (append fval nil)))
	  (setcar flist 'byte-code)
	  flist)
      (cons 'progn (cddr fval)))))

(defun gnus-extract-address-components (from)
  (let (name address)
    ;; First find the address - the thing with the @ in it.  This may
    ;; not be accurate in mail addresses, but does the trick most of
    ;; the time in news messages.
    (when (string-match "\\b[^@ \t<>]+[!@][^@ \t<>]+\\b" from)
      (setq address (substring from (match-beginning 0) (match-end 0))))
    ;; Then we check whether the "name <address>" format is used.
    (and address
	 ;; Fix by MORIOKA Tomohiko <morioka@jaist.ac.jp>
	 ;; Linear white space is not required.
	 (string-match (concat "[ \t]*<" (regexp-quote address) ">") from)
	 (and (setq name (substring from 0 (match-beginning 0)))
	      ;; Strip any quotes from the name.
	      (string-match "\".*\"" name)
	      (setq name (substring name 1 (1- (match-end 0))))))
    ;; If not, then "address (name)" is used.
    (or name
	(and (string-match "(.+)" from)
	     (setq name (substring from (1+ (match-beginning 0))
				   (1- (match-end 0)))))
	(and (string-match "()" from)
	     (setq name address))
	;; Fix by MORIOKA Tomohiko <morioka@jaist.ac.jp>.
	;; XOVER might not support folded From headers.
	(and (string-match "(.*" from)
	     (setq name (substring from (1+ (match-beginning 0))
				   (match-end 0)))))
    ;; Fix by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
    (list (or name from) (or address from))))

(defun gnus-fetch-field (field)
  "Return the value of the header FIELD of current article."
  (save-excursion
    (save-restriction
      (let ((case-fold-search t)
	    (inhibit-point-motion-hooks t))
	(nnheader-narrow-to-headers)
	(message-fetch-field field)))))

(defun gnus-goto-colon ()
  (beginning-of-line)
  (search-forward ":" (gnus-point-at-eol) t))

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
  (let ((newsgroup (gnus-newsgroup-savable-name newsgroup))
	(len (length newsgroup))
	idx)
    ;; If this is a foreign group, we don't want to translate the
    ;; entire name.
    (if (setq idx (string-match ":" newsgroup))
	(aset newsgroup idx ?/)
      (setq idx 0))
    ;; Replace all occurrences of `.' with `/'.
    (while (< idx len)
      (when (= (aref newsgroup idx) ?.)
	(aset newsgroup idx ?/))
      (setq idx (1+ idx)))
    newsgroup))

(defun gnus-newsgroup-savable-name (group)
  ;; Replace any slashes in a group name (eg. an ange-ftp nndoc group)
  ;; with dots.
  (nnheader-replace-chars-in-string group ?/ ?.))

(defun gnus-string> (s1 s2)
  (not (or (string< s1 s2)
	   (string= s1 s2))))

;;; Time functions.

(defun gnus-days-between (date1 date2)
  ;; Return the number of days between date1 and date2.
  (- (gnus-day-number date1) (gnus-day-number date2)))

(defun gnus-day-number (date)
  (let ((dat (mapcar (lambda (s) (and s (string-to-int s)) )
		     (timezone-parse-date date))))
    (timezone-absolute-from-gregorian
     (nth 1 dat) (nth 2 dat) (car dat))))

(defun gnus-time-to-day (time)
  "Convert TIME to day number."
  (let ((tim (decode-time time)))
    (timezone-absolute-from-gregorian
     (nth 4 tim) (nth 3 tim) (nth 5 tim))))

(defun gnus-encode-date (date)
  "Convert DATE to internal time."
  (let* ((parse (timezone-parse-date date))
	 (date (mapcar (lambda (d) (and d (string-to-int d))) parse))
	 (time (mapcar 'string-to-int (timezone-parse-time (aref parse 3)))))
    (encode-time (caddr time) (cadr time) (car time)
		 (caddr date) (cadr date) (car date)
		 (* 60 (timezone-zone-to-minute (nth 4 date))))))

(defun gnus-time-minus (t1 t2)
  "Subtract two internal times."
  (let ((borrow (< (cadr t1) (cadr t2))))
    (list (- (car t1) (car t2) (if borrow 1 0))
	  (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2)))))

(defun gnus-time-less (t1 t2)
  "Say whether time T1 is less than time T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

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

(defun gnus-completing-read (default prompt &rest args)
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

;; I suspect there's a better way, but I haven't taken the time to do
;; it yet.  -erik selberg@cs.washington.edu
(defun gnus-dd-mmm (messy-date)
  "Return a string like DD-MMM from a big messy string"
  (let ((datevec (ignore-errors (timezone-parse-date messy-date))))
    (if (not datevec)
	"??-???"
      (format "%2s-%s"
	      (condition-case ()
		  ;; Make sure leading zeroes are stripped.
		  (number-to-string (string-to-number (aref datevec 2)))
		(error "??"))
	      (capitalize
	       (or (car
		    (nth (1- (string-to-number (aref datevec 1)))
			 timezone-months-assoc))
		   "???"))))))

(defmacro gnus-date-get-time (date)
  "Convert DATE string to Emacs time.
Cache the result as a text property stored in DATE."
  ;; Either return the cached value...
  `(let ((d ,date))
     (if (equal "" d)
	 '(0 0)
       (or (get-text-property 0 'gnus-time d)
	   ;; or compute the value...
	   (let ((time (nnmail-date-to-time d)))
	     ;; and store it back in the string.
	     (put-text-property 0 1 'gnus-time time d)
	     time)))))

(defsubst gnus-time-iso8601 (time)
  "Return a string of TIME in YYMMDDTHHMMSS format."
  (format-time-string "%Y%m%dT%H%M%S" time))

(defun gnus-date-iso8601 (header)
  "Convert the date field in HEADER to YYMMDDTHHMMSS"
  (condition-case ()
      (gnus-time-iso8601 (gnus-date-get-time (mail-header-date header)))
    (error "")))

(defun gnus-mode-string-quote (string)
  "Quote all \"%\"'s in STRING."
  (save-excursion
    (gnus-set-work-buffer)
    (insert string)
    (goto-char (point-min))
    (while (search-forward "%" nil t)
      (insert "%"))
    (buffer-string)))

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

;; Show message if message has a lower level than `gnus-verbose'.
;; Guideline for numbers:
;; 1 - error messages, 3 - non-serious error messages, 5 - messages
;; for things that take a long time, 7 - not very important messages
;; on stuff, 9 - messages inside loops.
(defun gnus-message (level &rest args)
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
    (while (string-match "<[^>]+>" references beg)
      (push (substring references (match-beginning 0) (setq beg (match-end 0)))
	    ids))
    (nreverse ids)))

(defun gnus-parent-id (references &optional n)
  "Return the last Message-ID in REFERENCES.
If N, return the Nth ancestor instead."
  (when references
    (let ((ids (inline (gnus-split-references references))))
      (while (nthcdr (or n 1) ids)
	(setq ids (cdr ids)))
      (car ids))))

(defsubst gnus-buffer-live-p (buffer)
  "Say whether BUFFER is alive or not."
  (and buffer
       (get-buffer buffer)
       (buffer-name (get-buffer buffer))))

(defun gnus-horizontal-recenter ()
  "Recenter the current buffer horizontally."
  (if (< (current-column) (/ (window-width) 2))
      (set-window-hscroll (get-buffer-window (current-buffer) t) 0)
    (let* ((orig (point))
	   (end (window-end (get-buffer-window (current-buffer) t)))
	   (max 0))
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
	   (get-buffer-window (current-buffer) t)
	   (min (- (current-column) (/ (window-width) 3))
		(+ 2 (- max (window-width)))))
	(set-window-hscroll (get-buffer-window (current-buffer) t) 0))
      max)))

(defun gnus-read-event-char ()
  "Get the next event."
  (let ((event (read-event)))
    ;; should be gnus-characterp, but this can't be called in XEmacs anyway
    (cons (and (numberp event) event) event)))

(defun gnus-sortable-date (date)
  "Make sortable string by string-lessp from DATE.
Timezone package is used."
  (condition-case ()
      (progn
	(setq date (inline (timezone-fix-time
			    date nil
			    (aref (inline (timezone-parse-date date)) 4))))
	(inline
	  (timezone-make-sortable-date
	   (aref date 0) (aref date 1) (aref date 2)
	   (inline
	     (timezone-make-time-string
	      (aref date 3) (aref date 4) (aref date 5))))))
    (error "")))

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

(defun gnus-kill-all-overlays ()
  "Delete all overlays in the current buffer."
  (unless gnus-xemacs
    (let* ((overlayss (overlay-lists))
	   (buffer-read-only nil)
	   (overlays (delq nil (nconc (car overlayss) (cdr overlayss)))))
      (while overlays
	(delete-overlay (pop overlays))))))

(defvar gnus-work-buffer " *gnus work*")

(defun gnus-set-work-buffer ()
  "Put point in the empty Gnus work buffer."
  (if (get-buffer gnus-work-buffer)
      (progn
	(set-buffer gnus-work-buffer)
	(erase-buffer))
    (set-buffer (get-buffer-create gnus-work-buffer))
    (kill-all-local-variables)
    (buffer-disable-undo (current-buffer))))

(defmacro gnus-group-real-name (group)
  "Find the real name of a foreign newsgroup."
  `(let ((gname ,group))
     (if (string-match "^[^:]+:" gname)
	 (substring gname (match-end 0))
       gname)))

(defun gnus-make-sort-function (funs)
  "Return a composite sort condition based on the functions in FUNC."
  (cond
   ((not (listp funs)) funs)
   ((null funs) funs)
   ((cdr funs)
    `(lambda (t1 t2)
       ,(gnus-make-sort-function-1 (reverse funs))))
   (t
    (car funs))))

(defun gnus-make-sort-function-1 (funs)
  "Return a composite sort condition based on the functions in FUNC."
  (if (cdr funs)
      `(or (,(car funs) t1 t2)
	   (and (not (,(car funs) t2 t1))
		,(gnus-make-sort-function-1 (cdr funs))))
    `(,(car funs) t1 t2)))

(defun gnus-turn-off-edit-menu (type)
  "Turn off edit menu in `gnus-TYPE-mode-map'."
  (define-key (symbol-value (intern (format "gnus-%s-mode-map" type)))
    [menu-bar edit] 'undefined))

(defun gnus-prin1 (form)
  "Use `prin1' on FORM in the current buffer.
Bind `print-quoted' to t while printing."
  (let ((print-quoted t)
	print-level print-length)
    (prin1 form (current-buffer))))

(defun gnus-prin1-to-string (form)
  "The same as `prin1', but but `print-quoted' to t."
  (let ((print-quoted t))
    (prin1-to-string form)))

(defun gnus-make-directory (directory)
  "Make DIRECTORY (and all its parents) if it doesn't exist."
  (when (and directory
	     (not (file-exists-p directory)))
    (make-directory directory t))
  t)

(defun gnus-write-buffer (file)
  "Write the current buffer's contents to FILE."
  ;; Make sure the directory exists.
  (gnus-make-directory (file-name-directory file))
  ;; Write the buffer.
  (write-region (point-min) (point-max) file nil 'quietly))

(defmacro gnus-delete-assq (key list)
  `(let ((listval (eval ,list)))
     (setq ,list (delq (assq ,key listval) listval))))

(defmacro gnus-delete-assoc (key list)
  `(let ((listval ,list))
     (setq ,list (delq (assoc ,key listval) listval))))

(defun gnus-delete-file (file)
  "Delete FILE if it exists."
  (when (file-exists-p file)
    (delete-file file)))

(defun gnus-strip-whitespace (string)
  "Return STRING stripped of all whitespace."
  (while (string-match "[\r\n\t ]+" string)
    (setq string (replace-match "" t t string)))
  string)

(defun gnus-put-text-property-excluding-newlines (beg end prop val)
  "The same as `put-text-property', but don't put this prop on any newlines in the region."
  (save-match-data
    (save-excursion
      (save-restriction
	(goto-char beg)
	(while (re-search-forward "[ \t]*\n" end 'move)
	  (put-text-property beg (match-beginning 0) prop val)
	  (setq beg (point)))
	(put-text-property beg (point) prop val)))))

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
set to nil on a sucessful assignment.  In case of an error or other
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

(defvar rmail-default-rmail-file)
(defun gnus-output-to-rmail (filename &optional ask)
  "Append the current article to an Rmail file named FILENAME."
  (require 'rmail)
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
		  (let ((require-final-newline nil))
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
	    (append-to-file (point-min) (point-max) filename)
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
	      (search-backward "\^_")
	      (narrow-to-region (point) (point-max))
	      (goto-char (1+ (point-min)))
	      (rmail-count-new-messages t)
	      (rmail-show-message msg))))))
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
		(let ((require-final-newline nil))
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
		(append-to-file (point-min) (point-max) filename)))
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

(provide 'gnus-util)

;;; gnus-util.el ends here
