;;; tar-mode.el --- simple editing of tar files from GNU emacs

;;; Copyright (C) 1990, 1991, 1993, 1994, 1995 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@lucid.com>
;; Created: 04 Apr 1990
;; Keywords: unix

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
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; This package attempts to make dealing with Unix 'tar' archives easier.
;;; When this code is loaded, visiting a file whose name ends in '.tar' will
;;; cause the contents of that archive file to be displayed in a Dired-like
;;; listing.  It is then possible to use the customary Dired keybindings to
;;; extract sub-files from that archive, either by reading them into their own
;;; editor buffers, or by copying them directly to arbitrary files on disk.
;;; It is also possible to delete sub-files from within the tar file and write
;;; the modified archive back to disk, or to edit sub-files within the archive
;;; and re-insert the modified files into the archive.  See the documentation
;;; string of tar-mode for more info.

;;; This code now understands the extra fields that GNU tar adds to tar files.

;;; This interacts correctly with "uncompress.el" in the Emacs library,
;;; which you get with 
;;;
;;;  (autoload 'uncompress-while-visiting "uncompress")
;;;  (setq auto-mode-alist (cons '("\\.Z$" . uncompress-while-visiting)
;;;			   auto-mode-alist))
;;;
;;; Do not attempt to use tar-mode.el with crypt.el, you will lose.

;;;    ***************   TO DO   *************** 
;;;
;;; o  chmod should understand "a+x,og-w".
;;;
;;; o  It's not possible to add a NEW file to a tar archive; not that 
;;;    important, but still...
;;;
;;; o  The code is less efficient that it could be - in a lot of places, I
;;;    pull a 512-character string out of the buffer and parse it, when I could
;;;    be parsing it in place, not garbaging a string.  Should redo that.
;;;
;;; o  I'd like a command that searches for a string/regexp in every subfile
;;;    of an archive, where <esc> would leave you in a subfile-edit buffer.
;;;    (Like the Meta-R command of the Zmacs mail reader.)
;;;
;;; o  Sometimes (but not always) reverting the tar-file buffer does not 
;;;    re-grind the listing, and you are staring at the binary tar data.
;;;    Typing 'g' again immediately after that will always revert and re-grind
;;;    it, though.  I have no idea why this happens.
;;;
;;; o  Tar-mode interacts poorly with crypt.el and zcat.el because the tar
;;;    write-file-hook actually writes the file.  Instead it should remove the
;;;    header (and conspire to put it back afterwards) so that other write-file
;;;    hooks which frob the buffer have a chance to do their dirty work.  There
;;;    might be a problem if the tar write-file-hook does not come *first* on
;;;    the list.
;;;
;;; o  Block files, sparse files, continuation files, and the various header 
;;;    types aren't editable.  Actually I don't know that they work at all.

;;; Rationale:

;;; Why does tar-mode edit the file itself instead of using tar?

;;; That means that you can edit tar files which you don't have room for
;;; on your local disk.

;;; I don't know about recent features in gnu tar, but old versions of tar
;;; can't replace a file in the middle of a tar file with a new version.
;;; Tar-mode can.  I don't think tar can do things like chmod the subfiles.
;;; An implementation which involved unpacking and repacking the file into
;;; some scratch directory would be very wasteful, and wouldn't be able to
;;; preserve the file owners.

;;; Code:

(defvar tar-anal-blocksize 20
  "*The blocksize of tar files written by Emacs, or nil, meaning don't care.
The blocksize of a tar file is not really the size of the blocks; rather, it is
the number of blocks written with one system call.  When tarring to a tape, 
this is the size of the *tape* blocks, but when writing to a file, it doesn't
matter much.  The only noticeable difference is that if a tar file does not
have a blocksize of 20, tar will tell you that; all this really controls is
how many null padding bytes go on the end of the tar file.")

(defvar tar-update-datestamp nil
  "*Non-nil means tar-mode should play fast and loose with sub-file datestamps.
If this is true, then editing and saving a tar file entry back into its
tar file will update its datestamp.  If false, the datestamp is unchanged.
You may or may not want this - it is good in that you can tell when a file
in a tar archive has been changed, but it is bad for the same reason that
editing a file in the tar archive at all is bad - the changed version of 
the file never exists on disk.")

(defvar tar-mode-show-date nil
  "*Non-nil means Tar mode should show the date/time of each subfile.
This information is useful, but it takes screen space away from file names.")

(defvar tar-parse-info nil)
(defvar tar-header-offset nil)
(defvar tar-superior-buffer nil)
(defvar tar-superior-descriptor nil)
(defvar tar-subfile-mode nil)

(put 'tar-parse-info 'permanent-local t)
(put 'tar-header-offset 'permanent-local t)
(put 'tar-superior-buffer 'permanent-local t)
(put 'tar-superior-descriptor 'permanent-local t)

;;; First, duplicate some Common Lisp functions; I used to just (require 'cl)
;;; but "cl.el" was messing some people up (also it's really big).

(defmacro tar-setf (form val)
  "A mind-numbingly simple implementation of setf."
  (let ((mform (macroexpand form (and (boundp 'byte-compile-macro-environment)
				      byte-compile-macro-environment))))
    (cond ((symbolp mform) (list 'setq mform val))
	  ((not (consp mform)) (error "can't setf %s" form))
	  ((eq (car mform) 'aref)
	   (list 'aset (nth 1 mform) (nth 2 mform) val))
	  ((eq (car mform) 'car)
	   (list 'setcar (nth 1 mform) val))
	  ((eq (car mform) 'cdr)
	   (list 'setcdr (nth 1 mform) val))
	  (t (error "don't know how to setf %s" form)))))

(defmacro tar-dolist (control &rest body)
  "syntax: (dolist (var-name list-expr &optional return-value) &body body)"
  (let ((var (car control))
	(init (car (cdr control)))
	(val (car (cdr (cdr control)))))
    (list 'let (list (list '_dolist_iterator_ init))
	  (list 'while '_dolist_iterator_
	    (cons 'let
	      (cons (list (list var '(car _dolist_iterator_)))
		    (append body
			    (list (list 'setq '_dolist_iterator_
					(list 'cdr '_dolist_iterator_)))))))
	  val)))

(defmacro tar-dotimes (control &rest body)
  "syntax: (dolist (var-name count-expr &optional return-value) &body body)"
  (let ((var (car control))
	(n (car (cdr control)))
	(val (car (cdr (cdr control)))))
    (list 'let (list (list '_dotimes_end_ n)
		     (list var 0))
	  (cons 'while
		(cons (list '< var '_dotimes_end_)
		      (append body
			      (list (list 'setq var (list '1+ var))))))
	  val)))


;;; down to business.

(defmacro make-tar-header (name mode uid git size date ck lt ln
			   magic uname gname devmaj devmin)
  (list 'vector name mode uid git size date ck lt ln
	magic uname gname devmaj devmin))

(defmacro tar-header-name (x) (list 'aref x 0))
(defmacro tar-header-mode (x) (list 'aref x 1))
(defmacro tar-header-uid  (x) (list 'aref x 2))
(defmacro tar-header-gid  (x) (list 'aref x 3))
(defmacro tar-header-size (x) (list 'aref x 4))
(defmacro tar-header-date (x) (list 'aref x 5))
(defmacro tar-header-checksum  (x) (list 'aref x 6))
(defmacro tar-header-link-type (x) (list 'aref x 7))
(defmacro tar-header-link-name (x) (list 'aref x 8))
(defmacro tar-header-magic (x) (list 'aref x 9))
(defmacro tar-header-uname (x) (list 'aref x 10))
(defmacro tar-header-gname (x) (list 'aref x 11))
(defmacro tar-header-dmaj (x) (list 'aref x 12))
(defmacro tar-header-dmin (x) (list 'aref x 13))

(defmacro make-tar-desc (data-start tokens)
  (list 'cons data-start tokens))

(defmacro tar-desc-data-start (x) (list 'car x))
(defmacro tar-desc-tokens     (x) (list 'cdr x))

(defconst tar-name-offset 0)
(defconst tar-mode-offset (+ tar-name-offset 100))
(defconst tar-uid-offset  (+ tar-mode-offset 8))
(defconst tar-gid-offset  (+ tar-uid-offset 8))
(defconst tar-size-offset (+ tar-gid-offset 8))
(defconst tar-time-offset (+ tar-size-offset 12))
(defconst tar-chk-offset  (+ tar-time-offset 12))
(defconst tar-linkp-offset (+ tar-chk-offset 8))
(defconst tar-link-offset (+ tar-linkp-offset 1))
;;; GNU-tar specific slots.
(defconst tar-magic-offset (+ tar-link-offset 100))
(defconst tar-uname-offset (+ tar-magic-offset 8))
(defconst tar-gname-offset (+ tar-uname-offset 32))
(defconst tar-dmaj-offset (+ tar-gname-offset 32))
(defconst tar-dmin-offset (+ tar-dmaj-offset 8))
(defconst tar-end-offset (+ tar-dmin-offset 8))

(defun tar-header-block-tokenize (string)
  "Return a `tar-header' structure.
This is a list of name, mode, uid, gid, size, 
write-date, checksum, link-type, and link-name."
  (cond ((< (length string) 512) nil)
	(;(some 'plusp string)		 ; <-- oops, massive cycle hog!
	 (or (not (= 0 (aref string 0))) ; This will do.
	     (not (= 0 (aref string 101))))
	 (let* ((name-end (1- tar-mode-offset))
		(link-end (1- tar-magic-offset))
		(uname-end (1- tar-gname-offset))
		(gname-end (1- tar-dmaj-offset))
		(link-p (aref string tar-linkp-offset))
		(magic-str (substring string tar-magic-offset (1- tar-uname-offset)))
		(uname-valid-p (or (string= "ustar  " magic-str) (string= "GNUtar " magic-str)))
		name
		(nulsexp   "[^\000]*\000"))
	   (and (string-match nulsexp string tar-name-offset) (setq name-end (min name-end (1- (match-end 0)))))
	   (and (string-match nulsexp string tar-link-offset) (setq link-end (min link-end (1- (match-end 0)))))
	   (and (string-match nulsexp string tar-uname-offset) (setq uname-end (min uname-end (1- (match-end 0)))))
	   (and (string-match nulsexp string tar-gname-offset) (setq gname-end (min gname-end (1- (match-end 0)))))
	   (setq name (substring string tar-name-offset name-end)
		 link-p (if (or (= link-p 0) (= link-p ?0))
			    nil
			  (- link-p ?0)))
	   (if (and (null link-p) (string-match "/$" name)) (setq link-p 5)) ; directory
	   (make-tar-header
	     name
	     (tar-parse-octal-integer string tar-mode-offset (1- tar-uid-offset))
	     (tar-parse-octal-integer string tar-uid-offset (1- tar-gid-offset))
	     (tar-parse-octal-integer string tar-gid-offset (1- tar-size-offset))
	     (tar-parse-octal-integer string tar-size-offset (1- tar-time-offset))
	     (tar-parse-octal-long-integer string tar-time-offset (1- tar-chk-offset))
	     (tar-parse-octal-integer string tar-chk-offset (1- tar-linkp-offset))
	     link-p
	     (substring string tar-link-offset link-end)
	     uname-valid-p
	     (and uname-valid-p (substring string tar-uname-offset uname-end))
	     (and uname-valid-p (substring string tar-gname-offset gname-end))
	     (tar-parse-octal-integer string tar-dmaj-offset (1- tar-dmin-offset))
	     (tar-parse-octal-integer string tar-dmin-offset (1- tar-end-offset))
	     )))
	(t 'empty-tar-block)))


(defun tar-parse-octal-integer (string &optional start end)
  (if (null start) (setq start 0))
  (if (null end) (setq end (length string)))
  (if (= (aref string start) 0)
      0
    (let ((n 0))
      (while (< start end)
	(setq n (if (< (aref string start) ?0) n
		  (+ (* n 8) (- (aref string start) ?0)))
	      start (1+ start)))
      n)))

(defun tar-parse-octal-long-integer (string &optional start end)
  (if (null start) (setq start 0))
  (if (null end) (setq end (length string)))
  (if (= (aref string start) 0)
      (list 0 0)
    (let ((lo 0)
	  (hi 0))
      (while (< start end)
	(if (>= (aref string start) ?0)
	    (setq lo (+ (* lo 8) (- (aref string start) ?0))
		  hi (+ (* hi 8) (ash lo -16))
		  lo (logand lo 65535)))
	(setq start (1+ start)))
      (list hi lo))))

(defun tar-parse-octal-integer-safe (string)
  (let ((L (length string)))
    (if (= L 0) (error "empty string"))
    (tar-dotimes (i L)
       (if (or (< (aref string i) ?0)
	       (> (aref string i) ?7))
	   (error "'%c' is not an octal digit"))))
  (tar-parse-octal-integer string))


(defun tar-header-block-checksum (string)
  "Compute and return a tar-acceptable checksum for this block."
  (let* ((chk-field-start tar-chk-offset)
	 (chk-field-end (+ chk-field-start 8))
	 (sum 0)
	 (i 0))
    ;; Add up all of the characters except the ones in the checksum field.
    ;; Add that field as if it were filled with spaces.
    (while (< i chk-field-start)
      (setq sum (+ sum (aref string i))
	    i (1+ i)))
    (setq i chk-field-end)
    (while (< i 512)
      (setq sum (+ sum (aref string i))
	    i (1+ i)))
    (+ sum (* 32 8))))

(defun tar-header-block-check-checksum (hblock desired-checksum file-name)
  "Beep and print a warning if the checksum doesn't match."
  (if (not (= desired-checksum (tar-header-block-checksum hblock)))
      (progn (beep) (message "Invalid checksum for file %s!" file-name))))

(defun tar-header-block-recompute-checksum (hblock)
  "Modifies the given string to have a valid checksum field."
  (let* ((chk (tar-header-block-checksum hblock))
	 (chk-string (format "%6o" chk))
	 (l (length chk-string)))
    (aset hblock 154 0)
    (aset hblock 155 32)
    (tar-dotimes (i l) (aset hblock (- 153 i) (aref chk-string (- l i 1)))))
  hblock)

(defun tar-clip-time-string (time)
  (let ((str (current-time-string time)))
    (concat (substring str 4 16) (substring str 19 24))))

(defun tar-grind-file-mode (mode string start)
  "Store `-rw--r--r--' indicating MODE into STRING beginning at START.
MODE should be an integer which is a file mode value."
  (aset string start       (if (zerop (logand 256 mode)) ?- ?r))
  (aset string (+ start 1) (if (zerop (logand 128 mode)) ?- ?w))
  (aset string (+ start 2) (if (zerop (logand  64 mode)) ?- ?x)) 
  (aset string (+ start 3) (if (zerop (logand  32 mode)) ?- ?r))
  (aset string (+ start 4) (if (zerop (logand  16 mode)) ?- ?w))
  (aset string (+ start 5) (if (zerop (logand   8 mode)) ?- ?x))
  (aset string (+ start 6) (if (zerop (logand   4 mode)) ?- ?r))
  (aset string (+ start 7) (if (zerop (logand   2 mode)) ?- ?w))
  (aset string (+ start 8) (if (zerop (logand   1 mode)) ?- ?x))
  (if (zerop (logand 1024 mode)) nil (aset string (+ start 2) ?s))
  (if (zerop (logand 2048 mode)) nil (aset string (+ start 5) ?s))
  string)

(defun tar-header-block-summarize (tar-hblock &optional mod-p)
  "Returns a line similar to the output of `tar -vtf'."
  (let ((name (tar-header-name tar-hblock))
	(mode (tar-header-mode tar-hblock))
	(uid (tar-header-uid tar-hblock))
	(gid (tar-header-gid tar-hblock))
	(uname (tar-header-uname tar-hblock))
	(gname (tar-header-gname tar-hblock))
	(size (tar-header-size tar-hblock))
	(time (tar-header-date tar-hblock))
	(ck (tar-header-checksum tar-hblock))
	(link-p (tar-header-link-type tar-hblock))
	(link-name (tar-header-link-name tar-hblock))
	)
    (let* ((left 11)
	   (namew 8)
	   (groupw 8)
	   (sizew 8)
	   (datew (if tar-mode-show-date 18 0))
	   (slash (1- (+ left namew)))
	   (lastdigit (+ slash groupw sizew))
	   (datestart (+ lastdigit 2))
	   (namestart (+ datestart datew))
	   (string (make-string (+ namestart (length name) (if link-p (+ 5 (length link-name)) 0)) 32))
	   (type (tar-header-link-type tar-hblock)))
      (aset string 0 (if mod-p ?* ? ))
      (aset string 1
	    (cond ((or (eq type nil) (eq type 0)) ?-)
		  ((eq type 1) ?l)  ; link
		  ((eq type 2) ?s)  ; symlink
		  ((eq type 3) ?c)  ; char special
		  ((eq type 4) ?b)  ; block special
		  ((eq type 5) ?d)  ; directory
		  ((eq type 6) ?p)  ; FIFO/pipe
		  ((eq type 20) ?*) ; directory listing
		  ((eq type 29) ?M) ; multivolume continuation
		  ((eq type 35) ?S) ; sparse
		  ((eq type 38) ?V) ; volume header
		  ))
      (tar-grind-file-mode mode string 2)
      (setq uid (if (= 0 (length uname)) (int-to-string uid) uname))
      (setq gid (if (= 0 (length gname)) (int-to-string gid) gname))
      (setq size (int-to-string size))
      (setq time (tar-clip-time-string time))
      (tar-dotimes (i (min (1- namew) (length uid))) (aset string (- slash i) (aref uid (- (length uid) i 1))))
      (aset string (1+ slash) ?/)
      (tar-dotimes (i (min (1- groupw) (length gid))) (aset string (+ (+ slash 2) i) (aref gid i)))
      (tar-dotimes (i (min sizew (length size))) (aset string (- lastdigit i) (aref size (- (length size) i 1))))
      (if tar-mode-show-date
	  (tar-dotimes (i (length time)) (aset string (+ datestart i) (aref time i))))
      (tar-dotimes (i (length name)) (aset string (+ namestart i) (aref name i)))
      (if (or (eq link-p 1) (eq link-p 2))
	  (progn
	    (tar-dotimes (i 3) (aset string (+ namestart 1 (length name) i) (aref (if (= link-p 1) "==>" "-->") i)))
	    (tar-dotimes (i (length link-name)) (aset string (+ namestart 5 (length name) i) (aref link-name i)))))
      (put-text-property namestart (length string)
			 'mouse-face 'highlight string)
      string)))


(defun tar-summarize-buffer ()
  "Parse the contents of the tar file in the current buffer.
Place a dired-like listing on the front;
then narrow to it, so that only that listing
is visible (and the real data of the buffer is hidden)."
  (message "parsing tar file...")
  (let* ((result '())
	 (pos 1)
	 (bs (max 1 (- (buffer-size) 1024))) ; always 2+ empty blocks at end.
	 (bs100 (max 1 (/ bs 100)))
	 tokens)
    (while (and (<= (+ pos 512) (point-max))
		(not (eq 'empty-tar-block
			 (setq tokens
			       (tar-header-block-tokenize
				(buffer-substring pos (+ pos 512)))))))
      (setq pos (+ pos 512))
      (message "Parsing tar file...%d%%"
	       ;(/ (* pos 100) bs)   ; this gets round-off lossage
	       (/ pos bs100)         ; this doesn't
	       )
      (if (eq (tar-header-link-type tokens) 20)
	  ;; Foo.  There's an extra empty block after these.
	  (setq pos (+ pos 512)))
      (let ((size (tar-header-size tokens)))
	(if (< size 0)
	    (error "%s has size %s - corrupted"
		   (tar-header-name tokens) size))
	;
	; This is just too slow.  Don't really need it anyway....
	;(tar-header-block-check-checksum
	;  hblock (tar-header-block-checksum hblock)
	;  (tar-header-name tokens))

	(setq result (cons (make-tar-desc pos tokens) result))

	(and (null (tar-header-link-type tokens))
	     (> size 0)
	     (setq pos
		   (+ pos 512 (ash (ash (1- size) -9) 9))        ; this works
		   ;(+ pos (+ size (- 512 (rem (1- size) 512)))) ; this doesn't
		   ))))
    (make-local-variable 'tar-parse-info)
    (setq tar-parse-info (nreverse result))
    ;; A tar file should end with a block or two of nulls,
    ;; but let's not get a fatal error if it doesn't.
    (if (eq tokens 'empty-tar-block)
	(message "Parsing tar file...done.")
      (message "Warning: premature EOF parsing tar file")))
  (save-excursion
    (goto-char (point-min))
    (let ((buffer-read-only nil))
      (tar-dolist (tar-desc tar-parse-info)
	(insert-string
	  (tar-header-block-summarize (tar-desc-tokens tar-desc)))
	(insert-string "\n"))
      (make-local-variable 'tar-header-offset)
      (setq tar-header-offset (point))
      (narrow-to-region 1 tar-header-offset)
      (set-buffer-modified-p nil))))

(defvar tar-mode-map nil "*Local keymap for Tar mode listings.")

(if tar-mode-map
    nil
  (setq tar-mode-map (make-keymap))
  (suppress-keymap tar-mode-map)
  (define-key tar-mode-map " " 'tar-next-line)
  (define-key tar-mode-map "c" 'tar-copy)
  (define-key tar-mode-map "d" 'tar-flag-deleted)
  (define-key tar-mode-map "\^D" 'tar-flag-deleted)
  (define-key tar-mode-map "e" 'tar-extract)
  (define-key tar-mode-map "f" 'tar-extract)
  (define-key tar-mode-map "\C-m" 'tar-extract)
  (define-key tar-mode-map [mouse-2] 'tar-mouse-extract)
  (define-key tar-mode-map "g" 'revert-buffer)
  (define-key tar-mode-map "h" 'describe-mode)
  (define-key tar-mode-map "n" 'tar-next-line)
  (define-key tar-mode-map "\^N" 'tar-next-line)
  (define-key tar-mode-map "o" 'tar-extract-other-window)
  (define-key tar-mode-map "p" 'tar-previous-line)
  (define-key tar-mode-map "\^P" 'tar-previous-line)
  (define-key tar-mode-map "r" 'tar-rename-entry)
  (define-key tar-mode-map "u" 'tar-unflag)
  (define-key tar-mode-map "v" 'tar-view)
  (define-key tar-mode-map "x" 'tar-expunge)
  (define-key tar-mode-map "\177" 'tar-unflag-backwards)
  (define-key tar-mode-map "E" 'tar-extract-other-window)
  (define-key tar-mode-map "M" 'tar-chmod-entry)
  (define-key tar-mode-map "G" 'tar-chgrp-entry)
  (define-key tar-mode-map "O" 'tar-chown-entry)
  )

;; Make menu bar items.

;; Get rid of the Edit menu bar item to save space.
(define-key tar-mode-map [menu-bar edit] 'undefined)

(define-key tar-mode-map [menu-bar immediate]
  (cons "Immediate" (make-sparse-keymap "Immediate")))

(define-key tar-mode-map [menu-bar immediate view]
  '("View This File" . tar-view))
(define-key tar-mode-map [menu-bar immediate display]
  '("Display in Other Window" . tar-display-other-file))
(define-key tar-mode-map [menu-bar immediate find-file-other-window]
  '("Find in Other Window" . tar-extract-other-window))
(define-key tar-mode-map [menu-bar immediate find-file]
  '("Find This File" . tar-extract))

(define-key tar-mode-map [menu-bar mark]
  (cons "Mark" (make-sparse-keymap "Mark")))

(define-key tar-mode-map [menu-bar mark unmark-all]
  '("Unmark All" . tar-clear-modification-flags))
(define-key tar-mode-map [menu-bar mark deletion]
  '("Flag" . tar-flag-deleted))
(define-key tar-mode-map [menu-bar mark unmark]
  '("Unflag" . tar-unflag))

(define-key tar-mode-map [menu-bar operate]
  (cons "Operate" (make-sparse-keymap "Operate")))

(define-key tar-mode-map [menu-bar operate chown]
  '("Change Owner..." . tar-chown-entry))
(define-key tar-mode-map [menu-bar operate chgrp]
  '("Change Group..." . tar-chgrp-entry))
(define-key tar-mode-map [menu-bar operate chmod]
  '("Change Mode..." . tar-chmod-entry))
(define-key tar-mode-map [menu-bar operate rename]
  '("Rename to..." . tar-rename-entry))
(define-key tar-mode-map [menu-bar operate copy]
  '("Copy to..." . tar-copy))
(define-key tar-mode-map [menu-bar operate expunge]
  '("Expunge marked files" . tar-expunge))

;; tar mode is suitable only for specially formatted data.
(put 'tar-mode 'mode-class 'special)
(put 'tar-subfile-mode 'mode-class 'special)

;;;###autoload
(defun tar-mode ()
  "Major mode for viewing a tar file as a dired-like listing of its contents.
You can move around using the usual cursor motion commands. 
Letters no longer insert themselves.
Type `e' to pull a file out of the tar file and into its own buffer;
or click mouse-2 on the file's line in the Tar mode buffer.
Type `c' to copy an entry from the tar file into another file on disk.

If you edit a sub-file of this archive (as with the `e' command) and 
save it with Control-x Control-s, the contents of that buffer will be 
saved back into the tar-file buffer; in this way you can edit a file 
inside of a tar archive without extracting it and re-archiving it.

See also: variables `tar-update-datestamp' and `tar-anal-blocksize'.
\\{tar-mode-map}"
  ;; this is not interactive because you shouldn't be turning this
  ;; mode on and off.  You can corrupt things that way.
  ;; rms: with permanent locals, it should now be possible to make this work
  ;; interactively in some reasonable fashion.
  (kill-all-local-variables)
  (make-local-variable 'tar-header-offset)
  (make-local-variable 'tar-parse-info)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline nil) ; binary data, dude...
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'tar-mode-revert)
  (make-local-variable 'enable-local-variables)
  (setq enable-local-variables nil)
  (setq major-mode 'tar-mode)
  (setq mode-name "Tar")
  (use-local-map tar-mode-map)
  (auto-save-mode 0)
  (widen)
  (if (and (boundp 'tar-header-offset) tar-header-offset)
      (narrow-to-region 1 tar-header-offset)
      (tar-summarize-buffer))
  (run-hooks 'tar-mode-hook)
  )


;; This should be converted to use a minor mode keymap.

(defun tar-subfile-mode (p)
  "Minor mode for editing an element of a tar-file.
This mode redefines C-x C-s to save the current buffer back into its 
associated tar-file buffer.  You must save that buffer to actually
save your changes to disk."
  (interactive "P")
  (or (and (boundp 'tar-superior-buffer) tar-superior-buffer)
      (error "This buffer is not an element of a tar file"))
;;; Don't do this, because it is redundant and wastes mode line space.
;;;  (or (assq 'tar-subfile-mode minor-mode-alist)
;;;      (setq minor-mode-alist (append minor-mode-alist
;;;				     (list '(tar-subfile-mode " TarFile")))))
  (make-local-variable 'tar-subfile-mode)
  (setq tar-subfile-mode
	(if (null p)
	    (not tar-subfile-mode)
	    (> (prefix-numeric-value p) 0)))
  (cond (tar-subfile-mode
	 (make-local-variable 'local-write-file-hooks)
	 (setq local-write-file-hooks '(tar-subfile-save-buffer))
	 ;; turn off auto-save.
	 (auto-save-mode nil)
	 (setq buffer-auto-save-file-name nil)
	 (run-hooks 'tar-subfile-mode-hook))
	(t
	 (kill-local-variable 'local-write-file-hooks))))


;; Revert the buffer and recompute the dired-like listing.
(defun tar-mode-revert (&optional no-autosave no-confirm)
  (setq tar-header-offset nil)
  (let ((revert-buffer-function nil))
    (revert-buffer t no-confirm)
    (widen))
  (tar-mode))


(defun tar-next-line (p)
  (interactive "p")
  (forward-line p)
  (if (eobp) nil (forward-char (if tar-mode-show-date 54 36))))

(defun tar-previous-line (p)
  (interactive "p")
  (tar-next-line (- p)))

(defun tar-current-descriptor (&optional noerror)
  "Return the tar-descriptor of the current line, or signals an error."
  ;; I wish lines had plists, like in ZMACS...
  (or (nth (count-lines (point-min)
			(save-excursion (beginning-of-line) (point)))
	   tar-parse-info)
      (if noerror
	  nil
	  (error "This line does not describe a tar-file entry"))))

(defun tar-get-descriptor ()
  (let* ((descriptor (tar-current-descriptor))
	 (tokens (tar-desc-tokens descriptor))
	 (size (tar-header-size tokens))
	 (link-p (tar-header-link-type tokens)))
    (if link-p
	(error "This is a %s, not a real file"
	       (cond ((eq link-p 5) "directory")
		     ((eq link-p 20) "tar directory header")
		     ((eq link-p 29) "multivolume-continuation")
		     ((eq link-p 35) "sparse entry")
		     ((eq link-p 38) "volume header")
		     (t "link"))))
    (if (zerop size) (error "This is a zero-length file"))
    descriptor))

(defun tar-mouse-extract (event)
  "Extract a file whose tar directory line you click on."
  (interactive "e")
  (save-excursion
    (set-buffer (window-buffer (posn-window (event-end event))))
    (save-excursion
      (goto-char (posn-point (event-end event)))
      ;; Just make sure this doesn't get an error.
      (tar-get-descriptor)))
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (tar-extract))

(defun tar-extract (&optional other-window-p)
  "In Tar mode, extract this entry of the tar file into its own buffer."
  (interactive)
  (let* ((view-p (eq other-window-p 'view))
	 (descriptor (tar-get-descriptor))
	 (tokens (tar-desc-tokens descriptor))
	 (name (tar-header-name tokens))
	 (size (tar-header-size tokens))
	 (start (+ (tar-desc-data-start descriptor) tar-header-offset -1))
	 (end (+ start size)))
    (let* ((tar-buffer (current-buffer))
	   (tarname (file-name-nondirectory (buffer-file-name)))
	   (bufname (concat (file-name-nondirectory name)
			    " ("
			    tarname
			    ")"))
	   (read-only-p (or buffer-read-only view-p))
	   (buffer (get-buffer bufname))
	   (just-created nil))
      (if buffer
	  nil
	(setq buffer (get-buffer-create bufname))
	(setq just-created t)
	(unwind-protect
	    (progn
	      (widen)
	      (save-excursion
		(set-buffer buffer)
		(insert-buffer-substring tar-buffer start end)
		(goto-char 0)
		(setq buffer-file-name
		      (expand-file-name (concat tarname ":" name)))
		(setq buffer-file-truename
		      (abbreviate-file-name buffer-file-name))
		;; Set the default-directory to the dir of the
		;; superior buffer. 
		(setq default-directory
		      (save-excursion
			(set-buffer tar-buffer)
			default-directory))
		(normal-mode)  ; pick a mode.
		(rename-buffer bufname)
		(make-local-variable 'tar-superior-buffer)
		(make-local-variable 'tar-superior-descriptor)
		(setq tar-superior-buffer tar-buffer)
		(setq tar-superior-descriptor descriptor)
		(tar-subfile-mode 1)		
		(setq buffer-read-only read-only-p)
		(set-buffer-modified-p nil))
	      (set-buffer tar-buffer))
	  (narrow-to-region 1 tar-header-offset)))
      (if view-p
	  (progn
	    (view-buffer buffer)
	    (and just-created
		 (setq view-exit-action 'kill-buffer)))
	(if (eq other-window-p 'display)
	    (display-buffer buffer)
	  (if other-window-p
	      (switch-to-buffer-other-window buffer)
	    (switch-to-buffer buffer)))))))


(defun tar-extract-other-window ()
  "*In Tar mode, find this entry of the tar file in another window."
  (interactive)
  (tar-extract t))

(defun tar-display-other-window ()
  "*In Tar mode, display this entry of the tar file in another window."
  (interactive)
  (tar-extract 'display))

(defun tar-view ()
  "*In Tar mode, view the tar file entry on this line."
  (interactive)
  (tar-extract 'view))


(defun tar-read-file-name (&optional prompt)
  "Read a file name with this line's entry as the default."
  (or prompt (setq prompt "Copy to: "))
  (let* ((default-file (expand-file-name
			(tar-header-name (tar-desc-tokens
					  (tar-current-descriptor)))))
	 (target (expand-file-name
		  (read-file-name prompt
				  (file-name-directory default-file)
				  default-file nil))))
    (if (or (string= "" (file-name-nondirectory target))
	    (file-directory-p target))
	(setq target (concat (if (string-match "/$" target)
				 (substring target 0 (1- (match-end 0)))
				 target)
			     "/"
			     (file-name-nondirectory default-file))))
    target))


(defun tar-copy (&optional to-file)
  "*In Tar mode, extract this entry of the tar file into a file on disk.
If TO-FILE is not supplied, it is prompted for, defaulting to the name of
the current tar-entry."
  (interactive (list (tar-read-file-name)))
  (let* ((descriptor (tar-get-descriptor))
	 (tokens (tar-desc-tokens descriptor))
	 (name (tar-header-name tokens))
	 (size (tar-header-size tokens))
	 (start (+ (tar-desc-data-start descriptor) tar-header-offset -1))
	 (end (+ start size)))
    (save-restriction
      (widen)
      (write-region start end to-file))
    (message "Copied tar entry %s to %s" name to-file)))

(defun tar-flag-deleted (p &optional unflag)
  "*In Tar mode, mark this sub-file to be deleted from the tar file.
With a prefix argument, mark that many files."
  (interactive "p")
  (beginning-of-line)
  (tar-dotimes (i (if (< p 0) (- p) p))
    (if (tar-current-descriptor unflag) ; barf if we're not on an entry-line.
	(progn
	  (delete-char 1)
	  (insert (if unflag " " "D"))))
    (forward-line (if (< p 0) -1 1)))
  (if (eobp) nil (forward-char 36)))

(defun tar-unflag (p)
  "*In Tar mode, un-mark this sub-file if it is marked to be deleted.
With a prefix argument, un-mark that many files forward."
  (interactive "p")
  (tar-flag-deleted p t))

(defun tar-unflag-backwards (p)
  "*In Tar mode, un-mark this sub-file if it is marked to be deleted.
With a prefix argument, un-mark that many files backward."
  (interactive "p")
  (tar-flag-deleted (- p) t))


(defun tar-expunge-internal ()
  "Expunge the tar-entry specified by the current line."
  (let* ((descriptor (tar-current-descriptor))
	 (tokens (tar-desc-tokens descriptor))
	 (line (tar-desc-data-start descriptor))
	 (name (tar-header-name tokens))
	 (size (tar-header-size tokens))
	 (link-p (tar-header-link-type tokens))
	 (start (tar-desc-data-start descriptor))
	 (following-descs (cdr (memq descriptor tar-parse-info))))
    (if link-p (setq size 0)) ; size lies for hard-links.
    ;;
    ;; delete the current line...
    (beginning-of-line)
    (let ((line-start (point)))
      (end-of-line) (forward-char)
      (let ((line-len (- (point) line-start)))
	(delete-region line-start (point))
	;;
	;; decrement the header-pointer to be in synch...
	(setq tar-header-offset (- tar-header-offset line-len))))
    ;;
    ;; delete the data pointer...
    (setq tar-parse-info (delq descriptor tar-parse-info))
    ;;
    ;; delete the data from inside the file...
    (widen)
    (let* ((data-start (+ start tar-header-offset -513))
	   (data-end (+ data-start 512 (ash (ash (+ size 511) -9) 9))))
      (delete-region data-start data-end)
      ;;
      ;; and finally, decrement the start-pointers of all following
      ;; entries in the archive.  This is a pig when deleting a bunch
      ;; of files at once - we could optimize this to only do the
      ;; iteration over the files that remain, or only iterate up to
      ;; the next file to be deleted.
      (let ((data-length (- data-end data-start)))
	(tar-dolist (desc following-descs)
	  (tar-setf (tar-desc-data-start desc)
		    (- (tar-desc-data-start desc) data-length))))
      ))
  (narrow-to-region 1 tar-header-offset))


(defun tar-expunge (&optional noconfirm)
  "*In Tar mode, delete all the archived files flagged for deletion.
This does not modify the disk image; you must save the tar file itself
for this to be permanent."
  (interactive)
  (if (or noconfirm
	  (y-or-n-p "expunge files marked for deletion? "))
      (let ((n 0))
	(save-excursion
	  (goto-char 0)
	  (while (not (eobp))
	    (if (looking-at "D")
		(progn (tar-expunge-internal)
		       (setq n (1+ n)))
		(forward-line 1)))
	  ;; after doing the deletions, add any padding that may be necessary.
	  (tar-pad-to-blocksize)
	  (narrow-to-region 1 tar-header-offset)
	  )
	(if (zerop n)
	    (message "nothing to expunge.")
	    (message "%s expunged.  Be sure to save this buffer." n)))))


(defun tar-clear-modification-flags ()
  "Remove the stars at the beginning of each line."
  (interactive)
  (save-excursion
    (goto-char 1)
    (while (< (point) tar-header-offset)
      (if (not (eq (following-char) ?\ ))
	  (progn (delete-char 1) (insert " ")))
      (forward-line 1))))


(defun tar-chown-entry (new-uid)
  "*Change the user-id associated with this entry in the tar file.
If this tar file was written by GNU tar, then you will be able to edit
the user id as a string; otherwise, you must edit it as a number.
You can force editing as a number by calling this with a prefix arg.
This does not modify the disk image; you must save the tar file itself
for this to be permanent."
  (interactive (list
		 (let ((tokens (tar-desc-tokens (tar-current-descriptor))))
		   (if (or current-prefix-arg
			   (not (tar-header-magic tokens)))
		       (let (n)
			 (while (not (numberp (setq n (read-minibuffer
							"New UID number: "
							(format "%s" (tar-header-uid tokens)))))))
			 n)
		       (read-string "New UID string: " (tar-header-uname tokens))))))
  (cond ((stringp new-uid)
	 (tar-setf (tar-header-uname (tar-desc-tokens (tar-current-descriptor)))
		   new-uid)
	 (tar-alter-one-field tar-uname-offset (concat new-uid "\000")))
	(t
	 (tar-setf (tar-header-uid (tar-desc-tokens (tar-current-descriptor)))
		   new-uid)
	 (tar-alter-one-field tar-uid-offset
	   (concat (substring (format "%6o" new-uid) 0 6) "\000 ")))))


(defun tar-chgrp-entry (new-gid)
  "*Change the group-id associated with this entry in the tar file.
If this tar file was written by GNU tar, then you will be able to edit
the group id as a string; otherwise, you must edit it as a number.
You can force editing as a number by calling this with a prefix arg.
This does not modify the disk image; you must save the tar file itself
for this to be permanent."
  (interactive (list
		 (let ((tokens (tar-desc-tokens (tar-current-descriptor))))
		   (if (or current-prefix-arg
			   (not (tar-header-magic tokens)))
		       (let (n)
			 (while (not (numberp (setq n (read-minibuffer
							"New GID number: "
							(format "%s" (tar-header-gid tokens)))))))
			 n)
		       (read-string "New GID string: " (tar-header-gname tokens))))))
  (cond ((stringp new-gid)
	 (tar-setf (tar-header-gname (tar-desc-tokens (tar-current-descriptor)))
		   new-gid)
	 (tar-alter-one-field tar-gname-offset
	   (concat new-gid "\000")))
	(t
	 (tar-setf (tar-header-gid (tar-desc-tokens (tar-current-descriptor)))
		   new-gid)
	 (tar-alter-one-field tar-gid-offset
	   (concat (substring (format "%6o" new-gid) 0 6) "\000 ")))))

(defun tar-rename-entry (new-name)
  "*Change the name associated with this entry in the tar file.
This does not modify the disk image; you must save the tar file itself
for this to be permanent."
  (interactive
    (list (read-string "New name: "
	    (tar-header-name (tar-desc-tokens (tar-current-descriptor))))))
  (if (string= "" new-name) (error "zero length name"))
  (if (> (length new-name) 98) (error "name too long"))
  (tar-setf (tar-header-name (tar-desc-tokens (tar-current-descriptor)))
	    new-name)
  (tar-alter-one-field 0
    (substring (concat new-name (make-string 99 0)) 0 99)))


(defun tar-chmod-entry (new-mode)
  "*Change the protection bits associated with this entry in the tar file.
This does not modify the disk image; you must save the tar file itself
for this to be permanent."
  (interactive (list (tar-parse-octal-integer-safe
		       (read-string "New protection (octal): "))))
  (tar-setf (tar-header-mode (tar-desc-tokens (tar-current-descriptor)))
	    new-mode)
  (tar-alter-one-field tar-mode-offset
    (concat (substring (format "%6o" new-mode) 0 6) "\000 ")))


(defun tar-alter-one-field (data-position new-data-string)
  (let* ((descriptor (tar-current-descriptor))
	 (tokens (tar-desc-tokens descriptor)))
    (unwind-protect
	(save-excursion
	  ;;
	  ;; update the header-line.
	  (beginning-of-line)
	  (let ((p (point)))
	    (forward-line 1)
	    (delete-region p (point))
	    (insert (tar-header-block-summarize tokens) "\n")
	    (setq tar-header-offset (point-max)))
	  
	  (widen)
	  (let* ((start (+ (tar-desc-data-start descriptor) tar-header-offset -513)))
	    ;;
	    ;; delete the old field and insert a new one.
	    (goto-char (+ start data-position))
	    (delete-region (point) (+ (point) (length new-data-string))) ; <--
	    (insert new-data-string) ; <--
	    ;;
	    ;; compute a new checksum and insert it.
	    (let ((chk (tar-header-block-checksum
			(buffer-substring start (+ start 512)))))
	      (goto-char (+ start tar-chk-offset))
	      (delete-region (point) (+ (point) 8))
	      (insert (format "%6o" chk))
	      (insert 0)
	      (insert ? )
	      (tar-setf (tar-header-checksum tokens) chk)
	      ;;
	      ;; ok, make sure we didn't botch it.
	      (tar-header-block-check-checksum
	        (buffer-substring start (+ start 512))
	        chk (tar-header-name tokens))
	      )))
      (narrow-to-region 1 tar-header-offset))))


(defun tar-octal-time (timeval)
  ;; Format a timestamp as 11 octal digits.  Ghod, I hope this works...
  (let ((hibits (car timeval)) (lobits (car (cdr timeval))))
    (insert (format "%05o%01o%05o"
		    (lsh hibits -2)
		    (logior (lsh (logand 3 hibits) 1) (> (logand lobits 32768) 0))
		    (logand 32767 lobits)
		    ))))

(defun tar-subfile-save-buffer ()
  "In tar subfile mode, save this buffer into its parent tar-file buffer.
This doesn't write anything to disk; you must save the parent tar-file buffer
to make your changes permanent."
  (interactive)
  (if (not (and (boundp 'tar-superior-buffer) tar-superior-buffer))
    (error "This buffer has no superior tar file buffer"))
  (if (not (and (boundp 'tar-superior-descriptor) tar-superior-descriptor))
    (error "This buffer doesn't have an index into its superior tar file!"))
  (save-excursion
  (let ((subfile (current-buffer))
	(subfile-size (buffer-size))
	(descriptor tar-superior-descriptor))
    (set-buffer tar-superior-buffer)
    (let* ((tokens (tar-desc-tokens descriptor))
	   (start (tar-desc-data-start descriptor))
	   (name (tar-header-name tokens))
	   (size (tar-header-size tokens))
	   (size-pad (ash (ash (+ size 511) -9) 9))
	   (head (memq descriptor tar-parse-info))
	   (following-descs (cdr head)))
      (if (not head)
	(error "Can't find this tar file entry in its parent tar file!"))
      (unwind-protect
       (save-excursion
	(widen)
	;; delete the old data...
	(let* ((data-start (+ start tar-header-offset -1))
	       (data-end (+ data-start (ash (ash (+ size 511) -9) 9))))
	  (delete-region data-start data-end)
	  ;; insert the new data...
	  (goto-char data-start)
	  (insert-buffer subfile)
	  ;;
	  ;; pad the new data out to a multiple of 512...
	  (let ((subfile-size-pad (ash (ash (+ subfile-size 511) -9) 9)))
	    (goto-char (+ data-start subfile-size))
	    (insert (make-string (- subfile-size-pad subfile-size) 0))
	    ;;
	    ;; update the data pointer of this and all following files...
	    (tar-setf (tar-header-size tokens) subfile-size)
	    (let ((difference (- subfile-size-pad size-pad)))
	      (tar-dolist (desc following-descs)
		(tar-setf (tar-desc-data-start desc)
			  (+ (tar-desc-data-start desc) difference))))
	    ;;
	    ;; Update the size field in the header block.
	    (let ((header-start (- data-start 512)))
	      (goto-char (+ header-start tar-size-offset))
	      (delete-region (point) (+ (point) 12))
	      (insert (format "%11o" subfile-size))
	      (insert ? )
	      ;;
	      ;; Maybe update the datestamp.
	      (if (not tar-update-datestamp)
		  nil
		(goto-char (+ header-start tar-time-offset))
		(delete-region (point) (+ (point) 12))
		(insert (tar-octal-time (current-time)))
		(insert ? ))
	      ;;
	      ;; compute a new checksum and insert it.
	      (let ((chk (tar-header-block-checksum
			  (buffer-substring header-start data-start))))
		(goto-char (+ header-start tar-chk-offset))
		(delete-region (point) (+ (point) 8))
		(insert (format "%6o" chk))
		(insert 0)
		(insert ? )
		(tar-setf (tar-header-checksum tokens) chk)))
	    ;;
	    ;; alter the descriptor-line...
	    ;;
	    (let ((position (- (length tar-parse-info) (length head))))
	      (goto-char 1)
	      (next-line position)
	      (beginning-of-line)
	      (let ((p (point))
		    after
		    (m (set-marker (make-marker) tar-header-offset)))
		(forward-line 1)
		(setq after (point))
		;; Insert the new text after the old, before deleting,
		;; to preserve the window start.
		(insert-before-markers (tar-header-block-summarize tokens t) "\n")
		(delete-region p after)
		(setq tar-header-offset (marker-position m)))
	      )))
	;; after doing the insertion, add any final padding that may be necessary.
	(tar-pad-to-blocksize))
       (narrow-to-region 1 tar-header-offset)))
    (set-buffer-modified-p t)   ; mark the tar file as modified
    (set-buffer subfile)
    (set-buffer-modified-p nil) ; mark the tar subfile as unmodified
    (message "saved into tar-buffer `%s' -- remember to save that buffer!"
	     (buffer-name tar-superior-buffer))
    ;; Prevent ordinary saving from happening.
    t)))


(defun tar-pad-to-blocksize ()
  "If we are being anal about tar file blocksizes, fix up the current buffer.
Leaves the region wide."
  (if (null tar-anal-blocksize)
      nil
    (widen)
    (let* ((last-desc (nth (1- (length tar-parse-info)) tar-parse-info))
	   (start (tar-desc-data-start last-desc))
	   (tokens (tar-desc-tokens last-desc))
	   (link-p (tar-header-link-type tokens))
	   (size (if link-p 0 (tar-header-size tokens)))
	   (data-end (+ start size))
	   (bbytes (ash tar-anal-blocksize 9))
	   (pad-to (+ bbytes (* bbytes (/ (1- data-end) bbytes))))
	   (inhibit-read-only t) ; ##
	   )
      ;; If the padding after the last data is too long, delete some;
      ;; else insert some until we are padded out to the right number of blocks.
      ;;
      (goto-char (+ (or tar-header-offset 0) data-end))
      (if (> (1+ (buffer-size)) (+ (or tar-header-offset 0) pad-to))
	  (delete-region (+ (or tar-header-offset 0) pad-to) (1+ (buffer-size)))
	  (insert (make-string (- (+ (or tar-header-offset 0) pad-to)
				  (1+ (buffer-size)))
			       0)))
      )))


;; Used in write-file-hook to write tar-files out correctly.
(defun tar-mode-maybe-write-tar-file ()
  ;;
  ;; If the current buffer is in Tar mode and has its header-offset set,
  ;; only write out the part of the file after the header-offset.
  ;;
  (if (and (eq major-mode 'tar-mode)
	   (and (boundp 'tar-header-offset) tar-header-offset))
      (unwind-protect
	(save-excursion
	  (tar-clear-modification-flags)
	  (widen)
	  ;; Doing this here confuses things - the region gets left too wide!
	  ;; I suppose this is run in a context where changing the buffer is bad.
	  ;; (tar-pad-to-blocksize)
	  (write-region tar-header-offset (1+ (buffer-size)) buffer-file-name nil t)
	  ;; return T because we've written the file.
	  t)
	(narrow-to-region 1 tar-header-offset)
	t)
      ;; return NIL because we haven't.
      nil))


;;; Patch it in.

(or (memq 'tar-mode-maybe-write-tar-file write-file-hooks)
    (setq write-file-hooks
	  (cons 'tar-mode-maybe-write-tar-file write-file-hooks)))

(provide 'tar-mode)

;;; tar-mode.el ends here
