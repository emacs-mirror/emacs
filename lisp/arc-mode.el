;;; arc-mode.el --- simple editing of archives

;;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Morten Welinder (terra@diku.dk)
;; Keywords: archives msdog editing major-mode
;; Favourite-brand-of-beer: None, I hate beer.

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
;;
;; NAMING: "arc" is short for "archive" and does not refer specifically
;; to files whose name end in ".arc"
;;
;; This code does not decode any files internally, although it does
;; understand the directory level of the archives.  For this reason,
;; you should expect this code to need more fiddling than tar-mode.el
;; (although it at present has fewer bugs :-)  In particular, I have
;; not tested this under Ms-Dog myself.
;; -------------------------------------
;; INTERACTION: arc-mode.el should play together with
;;
;; * ange-ftp.el: Remote archives (i.e., ones that ange-ftp has brought
;;                to you) are handled by doing all updates on a local
;;                copy.  When you make changes to a remote file the
;;                changes will first take effect when the archive buffer
;;                is saved.  You will be warned about this.
;;
;; * dos-fns.el:  (Part of Emacs 19).  You get automatic ^M^J <--> ^J
;;                conversion.
;;
;; arc-mode.el does not work well with crypt++.el; for the archives as
;; such this could be fixed (but wouldn't be useful) by declaring such
;; archives to be "remote".  For the members this is a general Emacs
;; problem that 19.29's file formats may fix.
;; -------------------------------------
;; ARCHIVE TYPES: Currently only the archives below are handled, but the
;; structure for handling just about anything is in place.
;;
;;                        Arc     Lzh     Zip     Zoo
;;                        --------------------------------
;; View listing           Intern  Intern  Intern  Intern
;; Extract member         Y       Y       Y       Y
;; Save changed member    Y       Y       Y       Y
;; Add new member         N       N       N       N
;; Delete member          Y       Y       Y       Y
;; Rename member          Y       Y       N       N
;; Chmod                  -       Y       Y       -
;; Chown                  -       Y       -       -
;; Chgrp                  -       Y       -       -
;;
;; Special thanks to Bill Brodie <wbrodie@panix.com> for very useful tips
;; on the first released version of this package.
;;
;; This code is partly based on tar-mode.el from Emacs.
;; -------------------------------------
;; ARCHIVE STRUCTURES:
;; (This is mostly for myself.)
;;
;; ARC         A series of (header,file).  No interactions among members.
;;
;; LZH         A series of (header,file).  Headers are checksummed.  No
;;             interaction among members.
;;
;; ZIP         A series of (lheader,fil) followed by a "central directory"
;;             which is a series of (cheader) followed by an end-of-
;;             central-dir record possibly followed by junk.  The e-o-c-d
;;             links to c-d.  cheaders link to lheaders which are basically
;;             cut-down versions of the cheaders.
;;
;; ZOO         An archive header followed by a series of (header,file).
;;             Each member header points to the next.  The archive is
;;             terminated by a bogus header with a zero next link.
;; -------------------------------------
;; SETUP: .emacs fodder:
;;
;; (setq auto-mode-alist
;;       (cons '("\\.\\(arc\\|zip\\|lzh\\|zoo\\)\\'" . archive-mode)
;;             auto-mode-alist))
;; (autoload 'archive-mode "arc-mode" "Major mode for editing archives." t)
;;
;; Furthermore, for msdog, you need to make sure that the archives are loaded
;; as binary files.  For arc/zip/pak/lzh/zoo this is the default.
;; -------------------------------------
;; HOOKS: `foo' means one the the supported archive types.
;;
;; archive-mode-hook
;; archive-foo-mode-hook
;; archive-extract-hooks

;;; Code:

;; -------------------------------------------------------------------------
;; Section: Configuration.

(defvar archive-dos-members t
  "*If non-nil then recognize member files using ^M^J as line terminator
and do The Right Thing.")

(defvar archive-tmpdir
  (expand-file-name
   (make-temp-name (if (eq system-type 'ms-dos) "ar" "archive.tmp"))
   (or (getenv "TMPDIR") (getenv "TMP") "/tmp"))
  "*Directory for temporary files made by arc-mode.el")

(defvar archive-remote-regexp "^/[^/:]*[^/:]:"
  "*Regexp recognizing archive files names that are not local (i.e., are
not proper file names outside Emacs).  A local copy a the archive will
be used when updating.")

(defvar archive-extract-hooks nil
  "*Hooks to run when an archive member has been extracted.")
;; ------------------------------
;; Arc archive configuration

;; We always go via a local file since there seems to be no reliable way
;; to extract to stdout without junk getting added.
(defvar archive-arc-extract
  '("arc" "x")
  "*Program and its options to run in order to extract an arc file member
to the current directory.  Archive and member name will be added.")

(defvar archive-arc-expunge
  '("arc" "d")
  "*Program and its options to run in order to delete arc file members.
Archive and member names will be added.")

(defvar archive-arc-write-file-member
  '("arc" "u")
  "*Program and its options to run in order to update an arc file member.
Archive and member name will be added.")
;; ------------------------------
;; Lzh archive configuration

(defvar archive-lzh-extract
  '("lha" "pq")
  "*Program and its options to run in order to extract an lzh file member
to standard output.  Archive and member name will be added.")

(defvar archive-lzh-expunge
  '("lha" "d")
  "*Program and its options to run in order to delete lzh file members.
Archive and member names will be added.")

(defvar archive-lzh-write-file-member
  '("lha" "a")
  "*Program and its options to run in order to update an lzh file member.
Archive and member name will be added.")
;; ------------------------------
;; Zip archive configuration

(defvar archive-zip-use-pkzip (memq system-type '(ms-dos windows-nt))
  "*If non-nil then all zip options default to values suitable when using
pkzip and pkunzip.  Only set to true for msdog systems!")

(defvar archive-zip-extract
  (if archive-zip-use-pkzip '("pkunzip" "-e") '("unzip" "-qq" "-c"))
  "*Program and its options to run in order to extract a zip file member
to standard output.  Archive and member name will be added.\n
If `archive-zip-use-pkzip' is non-nil then this program is expected to
extract to a file junking the directory part of the name.")

;; For several reasons the latter behaviour is not desireable in general.
;; (1) It uses more disk space.  (2) Error checking is worse or non-
;; existent.  (3) It tends to do funny things with other systems' file
;; names.

(defvar archive-zip-expunge
  (if archive-zip-use-pkzip '("pkzip" "-d") '("zip" "-d" "-q"))
  "*Program and its options to run in order to delete zip file members.
Archive and member names will be added.")

(defvar archive-zip-update
  (if archive-zip-use-pkzip '("pkzip" "-u") '("zip" "-q"))
  "*Program and its options to run in order to update a zip file member.
Options should ensure that specified directory will be put into the zip
file.  Archive and member name will be added.")

(defvar archive-zip-update-case
  (if archive-zip-use-pkzip archive-zip-update '("zip" "-q" "-k"))
  "*Program and its options to run in order to update a case fiddled
zip file member.  Options should ensure that specified directory will
be put into the zip file.  Archive and member name will be added.")

(defvar archive-zip-case-fiddle t
  "*If non-nil then zip file members are mapped to lower case if created
by a system that under single case file names.")
;; ------------------------------
;; Zoo archive configuration

(defvar archive-zoo-extract
  '("zoo" "xpq")
  "*Program and its options to run in order to extract a zoo file member
to standard output.  Archive and member name will be added.")

(defvar archive-zoo-expunge
  '("zoo" "DqPP")
  "*Program and its options to run in order to delete zoo file members.
Archive and member names will be added.")

(defvar archive-zoo-write-file-member
  '("zoo" "a")
  "*Program and its options to run in order to update a zoo file member.
Archive and member name will be added.")
;; -------------------------------------------------------------------------
;; Section: Variables

(defvar archive-subtype nil "*Symbol describing archive type.")
(defvar archive-file-list-start nil "*Position of first contents line.")
(defvar archive-file-list-end nil "*Position just after last contents line.")
(defvar archive-proper-file-start nil "*Position of real archive's start.")
(defvar archive-read-only nil "*Non-nil if the archive is read-only on disk.")
(defvar archive-remote nil "*Non-nil if the archive is outside file system.")
(defvar archive-local-name nil "*Name of local copy of remote archive.")
(defvar archive-mode-map nil "*Local keymap for archive mode listings.")
(defvar archive-file-name-indent nil "*Column where file names start.")

(defvar archive-alternate-display nil
  "*Non-nil when alternate information is shown.")
(make-variable-buffer-local 'archive-alternate-display)
(put 'archive-alternate-display 'permanent-local t)

(defvar archive-superior-buffer nil "*In archive members, points to archive.")
(put 'archive-superior-buffer 'permanent-local t)

(defvar archive-subfile-mode nil "*Non-nil in archive member buffers.")
(make-variable-buffer-local 'archive-subfile-mode)
(put 'archive-subfile-mode 'permanent-local t)

;; buffer-file-type is a per-buffer variable in the msdog configuration
(if (boundp 'buffer-file-type) nil
  (defvar buffer-file-type nil
    "*Nil for dos-style text file, non-nil otherwise.")
  (make-variable-buffer-local 'buffer-file-type)
  (put 'buffer-file-type 'permanent-local t)
  (setq-default buffer-file-type nil))

(defvar archive-subfile-dos nil
  "Negation of `buffer-file-type' which see.")
(make-variable-buffer-local 'archive-subfile-dos)
(put 'archive-subfile-dos 'permanent-local t)

(defvar archive-files nil "Vector of file descriptors.  Each descriptor is
a vector of [ext-file-name int-file-name case-fiddled mode ...]")
(make-variable-buffer-local 'archive-files)
;; -------------------------------------------------------------------------
;; Section: Support functions.

(defsubst archive-name (suffix)
  (intern (concat "archive-" (symbol-name archive-subtype) "-" suffix)))

(defun archive-l-e (str &optional len)
  "Convert little endian string/vector to integer.  Alternatively, first
argument may be a buffer position in the current buffer in which case a
second arguemnt, length, should be supplied."
  (if (stringp str)
      (setq len (length str))
    (setq str (buffer-substring str (+ str len))))
  (let ((result 0)
        (i 0))
    (while (< i len)
      (setq i (1+ i)
            result (+ (ash result 8) (aref str (- len i)))))
    result))

(defun archive-int-to-mode (mode)
  "Turn an integer like 0700 (i.e., 448) into a mode string like -rwx------"
  (let ((str (make-string 10 ?-)))
    (or (zerop (logand 16384 mode)) (aset str 0 ?d))
    (or (zerop (logand  8192 mode)) (aset str 0 ?c)) ; completeness
    (or (zerop (logand   256 mode)) (aset str 1 ?r))
    (or (zerop (logand   128 mode)) (aset str 2 ?w))
    (or (zerop (logand    64 mode)) (aset str 3 ?x))
    (or (zerop (logand    32 mode)) (aset str 4 ?r))
    (or (zerop (logand    16 mode)) (aset str 5 ?w))
    (or (zerop (logand     8 mode)) (aset str 6 ?x))
    (or (zerop (logand     4 mode)) (aset str 7 ?r))
    (or (zerop (logand     2 mode)) (aset str 8 ?w))
    (or (zerop (logand     1 mode)) (aset str 9 ?x))
    (or (zerop (logand  1024 mode)) (aset str 3 (if (zerop (logand 64 mode))
						    ?S ?s)))
    (or (zerop (logand  2048 mode)) (aset str 6 (if (zerop (logand  8 mode))
						    ?S ?s)))
    str))

(defun archive-calc-mode (oldmode newmode &optional error)
  "From the integer OLDMODE and the string NEWMODE calculate a new file
mode.\n
NEWMODE may be an octal number including a leading zero in which case it
will become the new mode.\n
NEWMODE may also be a relative specification like \"og-rwx\" in which case
OLDMODE will be modified accordingly just like chmod(2) would have done.\n
If optional third argument ERROR is non-nil an error will be signaled if
the mode is invalid.  If ERROR is nil then nil will be returned."
  (cond ((string-match "^0[0-7]*$" newmode)
	 (let ((result 0)
	       (len (length newmode))
	       (i 1))
	   (while (< i len)
	     (setq result (+ (lsh result 3) (aref newmode i) (- ?0))
		   i (1+ i)))
	   (logior (logand oldmode 65024) result)))
	((string-match "^\\([agou]+\\)\\([---+=]\\)\\([rwxst]+\\)$" newmode)
	 (let ((who 0)
	       (result oldmode)
	       (op (aref newmode (match-beginning 2)))
	       (bits 0)
	       (i (match-beginning 3)))
	   (while (< i (match-end 3))
	     (let ((rwx (aref newmode i)))
	       (setq bits (logior bits (cond ((= rwx ?r)  292)
					     ((= rwx ?w)  146)
					     ((= rwx ?x)   73)
					     ((= rwx ?s) 3072)
					     ((= rwx ?t)  512)))
		     i (1+ i))))
	   (while (< who (match-end 1))
	     (let* ((whoc (aref newmode who))
		    (whomask (cond ((= whoc ?a) 4095)
				   ((= whoc ?u) 1472)
				   ((= whoc ?g) 2104)
				   ((= whoc ?o)    7))))
	       (if (= op ?=)
		   (setq result (logand result (lognot whomask))))
	       (if (= op ?-)
		   (setq result (logand result (lognot (logand whomask bits))))
		 (setq result (logior result (logand whomask bits)))))
	     (setq who (1+ who)))
	   result))
	(t
	 (if error
	     (error "Invalid mode specification: %s" newmode)))))

(defun archive-dosdate (date)
  "Stringify dos packed DATE record."
  (let ((year (+ 1980 (logand (ash date -9) 127)))
        (month (logand (ash date -5) 15))
        (day (logand date 31)))
    (if (or (> month 12) (< month 1))
        ""
      (format "%2d-%s-%d"
              day
              (aref ["Jan" "Feb" "Mar" "Apr" "May" "Jun"
                     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"] (1- month))
              year))))

(defun archive-dostime (time)
  "Stringify dos packed TIME record."
  (let ((hour (logand (ash time -11) 31))
        (minute (logand (ash time -5) 53))
        (second (* 2 (logand time 31)))) ; 2 seconds resolution
    (format "%02d:%02d:%02d" hour minute second)))

;;(defun archive-unixdate (low high)
;;  "Stringify unix (LOW HIGH) date."
;;  (let ((str (current-time-string (cons high low))))
;;    (format "%s-%s-%s"
;;	    (substring str 8 9)
;;	    (substring str 4 7)
;;	    (substring str 20 24))))

;;(defun archive-unixtime (low high)
;;  "Stringify unix (LOW HIGH) time."
;;  (let ((str (current-time-string (cons high low))))
;;    (substring str 11 19)))

(defun archive-get-lineno ()
  (if (>= (point) archive-file-list-start)
      (count-lines archive-file-list-start
		   (save-excursion (beginning-of-line) (point)))
    0))

(defun archive-get-descr (&optional noerror)
  "Return the descriptor vector for file at point.  Do not signal an error
if optional second argument NOERROR is non-nil."
  (let ((no (archive-get-lineno)))
    (if (and (>= (point) archive-file-list-start)
             (< no (length archive-files)))
	(let ((item (aref archive-files no)))
	  (if (vectorp item)
	      item
	    (if (not noerror)
		(error "Entry is not a regular member of the archive"))))
      (if (not noerror)
          (error "Line does not describe a member of the archive")))))
;; -------------------------------------------------------------------------
;; Section: the mode definition

(defun archive-mode (&optional force)
  "Major mode for viewing an archive file as a dired-like listing of its
contents.  You can move around using the usual cursor motion commands.
Letters no longer insert themselves.
Type `e' to pull a file out of the archive and into its own buffer;
or click mouse-2 on the file's line in the archive mode buffer.

If you edit a sub-file of this archive (as with the `e' command) and
save it, the contents of that buffer will be saved back into the
archive.

\\{archive-mode-map}"
  ;; This is not interactive because you shouldn't be turning this
  ;; mode on and off.  You can corrupt things that way.
  (if (zerop (buffer-size))
      ;; At present we cannot create archives from scratch
      (funcall default-major-mode)
    (if (and (not force) archive-files) nil
      (let* ((type (archive-find-type))
	     (typename (copy-sequence (symbol-name type))))
	(aset typename 0 (upcase (aref typename 0)))
	(kill-all-local-variables)
	(make-local-variable 'archive-subtype)
	(setq archive-subtype type)

	;; Buffer contains treated image of file before the file contents
	(make-local-variable 'revert-buffer-function)
	(setq revert-buffer-function 'archive-mode-revert)
	(auto-save-mode 0)
	(make-local-variable 'local-write-file-hooks)
	(add-hook 'local-write-file-hooks 'archive-write-file)

	;; Real file contents is binary
	(make-local-variable 'require-final-newline)
	(setq require-final-newline nil)
	(make-local-variable 'enable-local-variables)
	(setq enable-local-variables nil)
	(setq buffer-file-type t)

	(make-local-variable 'archive-read-only)
	(setq archive-read-only (not (file-writable-p (buffer-file-name))))

	;; Should we use a local copy when accessing from outside Emacs?
	(make-local-variable 'archive-local-name)
	(make-local-variable 'archive-remote)
	(setq archive-remote (string-match archive-remote-regexp
					   (buffer-file-name)))

	(setq major-mode 'archive-mode)
	(setq mode-name (concat typename "-Archive"))
	;; Run archive-foo-mode-hook and archive-mode-hook
	(run-hooks (archive-name "mode-hook") 'archive-mode-hook)
	(use-local-map archive-mode-map))

      (make-local-variable 'archive-proper-file-start)
      (make-local-variable 'archive-file-list-start)
      (make-local-variable 'archive-file-list-end)
      (make-local-variable 'archive-file-name-indent)
      (archive-summarize)
      (setq buffer-read-only t))))

;; Archive mode is suitable only for specially formatted data.
(put 'archive-mode 'mode-class 'special)
;; -------------------------------------------------------------------------
;; Section: Key maps

(if archive-mode-map nil
  (setq archive-mode-map (make-keymap))
  (suppress-keymap archive-mode-map)
  (define-key archive-mode-map " " 'archive-next-line)
  (define-key archive-mode-map "a" 'archive-alternate-display)
  ;;(define-key archive-mode-map "c" 'archive-copy)
  (define-key archive-mode-map "d" 'archive-flag-deleted)
  (define-key archive-mode-map "\C-d" 'archive-flag-deleted)
  (define-key archive-mode-map "e" 'archive-extract)
  (define-key archive-mode-map "f" 'archive-extract)
  (define-key archive-mode-map "\C-m" 'archive-extract)
  (define-key archive-mode-map [mouse-2] 'archive-mouse-extract)
  (define-key archive-mode-map "g" 'revert-buffer)
  (define-key archive-mode-map "h" 'describe-mode)
  (define-key archive-mode-map "m" 'archive-mark)
  (define-key archive-mode-map "n" 'archive-next-line)
  (define-key archive-mode-map "\C-n" 'archive-next-line)
  (define-key archive-mode-map [down] 'archive-next-line)
  (define-key archive-mode-map "o" 'archive-extract-other-window)
  (define-key archive-mode-map "p" 'archive-previous-line)
  (define-key archive-mode-map "\C-p" 'archive-previous-line)
  (define-key archive-mode-map [up] 'archive-previous-line)
  (define-key archive-mode-map "r" 'archive-rename-entry)
  (define-key archive-mode-map "u" 'archive-unflag)
  (define-key archive-mode-map "\M-\C-?" 'archive-unmark-all-files)
  (define-key archive-mode-map "v" 'archive-view)
  (define-key archive-mode-map "x" 'archive-expunge)
  (define-key archive-mode-map "\177" 'archive-unflag-backwards)
  (define-key archive-mode-map "E" 'archive-extract-other-window)
  (define-key archive-mode-map "M" 'archive-chmod-entry)
  (define-key archive-mode-map "G" 'archive-chgrp-entry)
  (define-key archive-mode-map "O" 'archive-chown-entry)
  (substitute-key-definition 'undo 'archive-undo archive-mode-map global-map)

  ;; Get rid of the Edit menu bar item to save space.
  (define-key archive-mode-map [menu-bar edit] 'undefined)

  (define-key archive-mode-map [menu-bar immediate]
    (cons "Immediate" (make-sparse-keymap "Immediate")))
  (define-key archive-mode-map [menu-bar immediate alternate]
    '("Alternate Display" . archive-alternate-display))
  (put 'archive-alternate-display 'menu-enable
       '(boundp (archive-name "alternate-display")))
  (define-key archive-mode-map [menu-bar immediate view]
    '("View This File" . archive-view))
  (define-key archive-mode-map [menu-bar immediate display]
    '("Display in Other Window" . archive-display-other-window))
  (define-key archive-mode-map [menu-bar immediate find-file-other-window]
    '("Find in Other Window" . archive-extract-other-window))
  (define-key archive-mode-map [menu-bar immediate find-file]
    '("Find This File" . archive-extract))

  (define-key archive-mode-map [menu-bar mark]
    (cons "Mark" (make-sparse-keymap "Mark")))
  (define-key archive-mode-map [menu-bar mark unmark-all]
    '("Unmark All" . archive-unmark-all-files))
  (define-key archive-mode-map [menu-bar mark deletion]
    '("Flag" . archive-flag-deleted))
  (define-key archive-mode-map [menu-bar mark unmark]
    '("Unflag" . archive-unflag))
  (define-key archive-mode-map [menu-bar mark mark]
    '("Mark" . archive-mark))

  (define-key archive-mode-map [menu-bar operate]
    (cons "Operate" (make-sparse-keymap "Operate")))
  (define-key archive-mode-map [menu-bar operate chown]
    '("Change Owner..." . archive-chown-entry))
  (put 'archive-chown-entry 'menu-enable
       '(fboundp (archive-name "chown-entry")))
  (define-key archive-mode-map [menu-bar operate chgrp]
    '("Change Group..." . archive-chgrp-entry))
  (put 'archive-chgrp-entry 'menu-enable
       '(fboundp (archive-name "chgrp-entry")))
  (define-key archive-mode-map [menu-bar operate chmod]
    '("Change Mode..." . archive-chmod-entry))
  (put 'archive-chmod-entry 'menu-enable
       '(fboundp (archive-name "chmod-entry")))
  (define-key archive-mode-map [menu-bar operate rename]
    '("Rename to..." . archive-rename-entry))
  (put 'archive-rename-entry 'menu-enable
       '(fboundp (archive-name "rename-entry")))
  ;;(define-key archive-mode-map [menu-bar operate copy]
  ;;  '("Copy to..." . archive-copy))
  (define-key archive-mode-map [menu-bar operate expunge]
    '("Expunge Marked Files" . archive-expunge))
  )

(let* ((item1 '(archive-subfile-mode " Archive"))
       (item2 '(archive-subfile-dos " Dos"))
       (items (if (memq system-type '(ms-dos windows-nt))
		  (list item1) ; msdog has its own indicator
		(list item1 item2))))
  (or (member item1 minor-mode-alist)
      (setq minor-mode-alist (append items minor-mode-alist))))
;; -------------------------------------------------------------------------
(defun archive-find-type ()
  (widen)
  (goto-char (point-min))
  ;; The funny [] here make it unlikely that the .elc file will be treated
  ;; as an archive by other software.
  (let (case-fold-search)
    (cond ((looking-at "[P]K\003\004") 'zip)
	  ((looking-at "..-l[hz][0-9]-") 'lzh)
	  ((looking-at "....................[\334]\247\304\375") 'zoo)
	  ((and (looking-at "\C-z")	; signature too simple, IMHO
		(string-match "\\.[aA][rR][cC]$"
			      (or buffer-file-name (buffer-name))))
	   'arc)
	  (t (error "Buffer format not recognized.")))))
;; -------------------------------------------------------------------------
(defun archive-summarize ()
  "Parse the contents of the archive file in the current buffer.
Place a dired-like listing on the front;
then narrow to it, so that only that listing
is visible (and the real data of the buffer is hidden)."
  (widen)
  (let (buffer-read-only)
    (message "Parsing archive file...")
    (buffer-disable-undo (current-buffer))
    (setq archive-files (funcall (archive-name "summarize")))
    (message "Parsing archive file...done.")
    (setq archive-proper-file-start (point-marker))
    (narrow-to-region (point-min) (point))
    (set-buffer-modified-p nil)
    (buffer-enable-undo))
  (goto-char archive-file-list-start)
  (archive-next-line 0))

(defun archive-resummarize ()
  "Recreate the contents listing of an archive."
  (let ((modified (buffer-modified-p))
	(no (archive-get-lineno))
	buffer-read-only)
    (widen)
    (delete-region (point-min) archive-proper-file-start)
    (archive-summarize)
    (set-buffer-modified-p modified)
    (goto-char archive-file-list-start)
    (archive-next-line no)))

(defun archive-summarize-files (files)
  "Insert a desciption of a list of files annotated with proper mouse face"
  (setq archive-file-list-start (point-marker))
  (setq archive-file-name-indent (if files (aref (car files) 1) 0))
  ;; We don't want to do an insert for each element since that takes too
  ;; long when the archive -- which has to be moved in memory -- is large.
  (insert
   (apply
    (function concat)
    (mapcar
     (lambda (fil)
       ;; Using `concat' here copies the text also, so we can add
       ;; properties without problems.
       (let ((text (concat (aref fil 0) "\n")))
	 (put-text-property (aref fil 1) (aref fil 2)
			    'mouse-face 'highlight
			    text)
	 text))
     files)))
  (setq archive-file-list-end (point-marker)))

(defun archive-alternate-display ()
  "Toggle alternative display.  To avoid very long lines some archive mode
don't show all information.  This function changes the set of information
shown for each files."
  (interactive)
  (setq archive-alternate-display (not archive-alternate-display))
  (archive-resummarize))
;; -------------------------------------------------------------------------
;; Section: Local archive copy handling

(defun archive-maybe-copy (archive)
  (if archive-remote
      (let ((start (point-max)))
	(setq archive-local-name (expand-file-name
				  (file-name-nondirectory archive)
				  archive-tmpdir))
	(make-directory archive-tmpdir t)
	(save-restriction
	  (widen)
	  (write-region start (point-max) archive-local-name nil 'nomessage))
	archive-local-name)
    (if (buffer-modified-p) (save-buffer))
    archive))

(defun archive-maybe-update (unchanged)
  (if archive-remote
      (let ((name archive-local-name)
	    (modified (buffer-modified-p))
	    buffer-read-only)
	(if unchanged nil
	  (erase-buffer)
	  (insert-file-contents name)
	  (archive-mode t))
	(archive-delete-local name)
	(if (not unchanged)
	    (message "Archive file must be saved for changes to take effect"))
	(set-buffer-modified-p (or modified (not unchanged))))))

(defun archive-delete-local (name)
  "Delete (robust) the file NAME and its parents up to and including the
value of `archive-tmpdir'."
  (let ((again t)
	(top (directory-file-name (file-name-as-directory archive-tmpdir))))
    (condition-case nil
	(delete-file name)
      (error nil))
    (while again
      (setq name (directory-file-name (file-name-directory name)))
      (condition-case nil
	  (delete-directory name)
	(error nil))
      (if (string= name top) (setq again nil)))))
;; -------------------------------------------------------------------------
;; Section: Member extraction

(defun archive-mouse-extract (event)
  "Extract a file whose name you click on."
  (interactive "e")
  (save-excursion
    (set-buffer (window-buffer (posn-window (event-end event))))
    (save-excursion
      (goto-char (posn-point (event-end event)))
      ;; Just make sure this doesn't get an error.
      (archive-get-descr)))
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (archive-extract))

(defun archive-extract (&optional other-window-p)
  "In archive mode, extract this entry of the archive into its own buffer."
  (interactive)
  (let* ((view-p (eq other-window-p 'view))
	 (descr (archive-get-descr))
         (ename (aref descr 0))
         (iname (aref descr 1))
         (archive-buffer (current-buffer))
         (arcdir default-directory)
         (archive (buffer-file-name))
         (arcname (file-name-nondirectory archive))
         (bufname (concat (file-name-nondirectory iname) " (" arcname ")"))
         (extractor (archive-name "extract"))
         (read-only-p (or archive-read-only view-p))
         (buffer (get-buffer bufname))
         (just-created nil))
      (if buffer
          nil
	(setq archive (archive-maybe-copy archive))
        (setq buffer (get-buffer-create bufname))
        (setq just-created t)
        (save-excursion
          (set-buffer buffer)
          (setq buffer-file-name
                (expand-file-name (concat arcname ":" iname)))
          (setq buffer-file-truename
                (abbreviate-file-name buffer-file-name))
          ;; Set the default-directory to the dir of the superior buffer.
          (setq default-directory arcdir)
          (make-local-variable 'archive-superior-buffer)
          (setq archive-superior-buffer archive-buffer)
          (make-local-variable 'local-write-file-hooks)
          (add-hook 'local-write-file-hooks 'archive-write-file-member)
          (setq archive-subfile-mode descr)
	  (setq archive-subfile-dos nil
		buffer-file-type t)
	  (if (fboundp extractor)
	      (funcall extractor archive ename)
	    (archive-*-extract archive ename (symbol-value extractor)))
          (if archive-dos-members (archive-check-dos))
          (goto-char (point-min))
          (rename-buffer bufname)
          (setq buffer-read-only read-only-p)
	  (setq buffer-undo-list nil)
          (set-buffer-modified-p nil)
	  (setq buffer-saved-size (buffer-size))
          (normal-mode)
	  ;; Just in case an archive occurs inside another archive.
	  (if (eq major-mode 'archive-mode)
	      (setq archive-remote t))
	  (run-hooks 'archive-extract-hooks))
	(archive-maybe-update t))
      (if view-p
          (progn
            (view-buffer buffer)
            (and just-created (setq view-exit-action 'kill-buffer)))
        (if (eq other-window-p 'display)
            (display-buffer buffer)
          (if other-window-p
              (switch-to-buffer-other-window buffer)
            (switch-to-buffer buffer))))))

(defun archive-*-extract (archive name command)
  (let* ((default-directory (file-name-as-directory archive-tmpdir))
	 (tmpfile (expand-file-name (file-name-nondirectory name)
				    default-directory)))
    (make-directory (directory-file-name default-directory) t)
    (apply 'call-process
	   (car command)
	   nil
	   nil
	   nil
	   (append (cdr command) (list archive name)))
    (insert-file-contents tmpfile)
    (archive-delete-local tmpfile)))

(defun archive-extract-by-stdout (archive name command)
  (let ((binary-process-output t)) ; for Ms-Dos
    (apply 'call-process
	   (car command)
	   nil
	   t
	   nil
	   (append (cdr command) (list archive name)))))

(defun archive-extract-other-window ()
  "In archive mode, find this member in another window."
  (interactive)
  (archive-extract t))

(defun archive-display-other-window ()
  "In archive mode, display this member in another window."
  (interactive)
  (archive-extract 'display))

(defun archive-view ()
  "In archive mode, view the member on this line."
  (interactive)
  (archive-extract 'view))

(defun archive-add-new-member (arcbuf name)
  "Add the file in the current buffer to the archive in ARCBUF naming it
NAME."
  (interactive
   (list (get-buffer
	  (read-buffer "Buffer containing archive: "
		       ;; Find first archive buffer and suggest that
		       (let ((bufs (buffer-list)))
			 (while (and bufs (not (eq (save-excursion
						     (set-buffer (car bufs))
						     major-mode)
						   'archive-mode)))
			   (setq bufs (cdr bufs)))
			 (if bufs
			     (car bufs)
			   (error "There are no archive buffers")))
		       t))
	 (read-string "File name in archive: "
		      (if buffer-file-name
			  (file-name-nondirectory buffer-file-name)
			""))))
  (save-excursion
    (set-buffer arcbuf)
    (or (eq major-mode 'archive-mode)
	(error "Buffer is not an archive buffer"))
    (if archive-read-only
	(error "Archive is read-only")))
  (if (eq arcbuf (current-buffer))
      (error "An archive buffer cannot be added to itself"))
  (if (string= name "")
      (error "Archive members may not be given empty names"))
  (let ((func (save-excursion (set-buffer arcbuf)
			      (archive-name "add-new-member")))
	(membuf (current-buffer)))
    (if (fboundp func)
	(save-excursion
	  (set-buffer arcbuf)
	  (funcall func buffer-file-name membuf name))
      (error "Adding a new member is not supported for this archive type"))))
;; -------------------------------------------------------------------------
;; Section: IO stuff

(defun archive-check-dos (&optional force)
  "*If this looks like a buffer with ^M^J as line terminator then remove
those ^Ms and set archive-subfile-dos."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (setq archive-subfile-dos
	    (or force (not (search-forward-regexp "[^\r]\n" nil t))))
      (setq buffer-file-type (not archive-subfile-dos))
      (if archive-subfile-dos
          (let ((modified (buffer-modified-p)))
            (buffer-disable-undo (current-buffer))
            (goto-char (point-min))
            (while (search-forward "\r\n" nil t)
              (replace-match "\n"))
            (buffer-enable-undo)
            (set-buffer-modified-p modified))))))

(defun archive-write-file-member ()
  (if archive-subfile-dos
      (save-restriction
	(widen)
        (save-excursion
          (goto-char (point-min))
          ;; We don't want our ^M^J <--> ^J changes to show in the undo list
          (let ((undo-list buffer-undo-list))
            (unwind-protect
                (progn
                  (setq buffer-undo-list t)
                  (while (search-forward "\n" nil t)
                    (replace-match "\r\n"))
                  (setq archive-subfile-dos nil)
                  (setq buffer-file-type t)
                  ;; OK, we're now have explicit ^M^Js -- save and re-unixfy
                  (archive-write-file-member))
              (progn
                (archive-check-dos t)
                (setq buffer-undo-list undo-list))))
          t))
    (save-excursion
      (save-restriction
        (message "Updating archive...")
        (widen)
	(let ((writer  (save-excursion (set-buffer archive-superior-buffer)
				       (archive-name "write-file-member")))
	      (archive (save-excursion (set-buffer archive-superior-buffer)
				       (buffer-file-name))))
	  (if (fboundp writer)
	      (funcall writer archive archive-subfile-mode)
	    (archive-*-write-file-member archive
					 archive-subfile-mode
					 (symbol-value writer))))
	(set-buffer-modified-p nil)
        (message "Updating archive...done")
        (set-buffer archive-superior-buffer)
        (revert-buffer)
        t))))

(defun archive-*-write-file-member (archive descr command)
  (let* ((ename (aref descr 0))
         (tmpfile (expand-file-name ename archive-tmpdir))
         (top (directory-file-name (file-name-as-directory archive-tmpdir)))
	 (default-directory (file-name-as-directory top)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory tmpfile) t)
	  (write-region (point-min) (point-max) tmpfile nil 'nomessage)
	  (if (aref descr 3)
	      ;; Set the file modes, but make sure we can read it.
	      (set-file-modes tmpfile (logior ?\400 (aref descr 3))))
          (let ((exitcode (apply 'call-process
                                 (car command)
                                 nil
                                 nil
                                 nil
                                 (append (cdr command) (list archive ename)))))
            (if (equal exitcode 0)
                nil
              (error "Updating was unsuccessful (%S)" exitcode))))
      (archive-delete-local tmpfile))))

(defun archive-write-file ()
  (save-excursion
    (write-region archive-proper-file-start (point-max) buffer-file-name nil t)
    (set-buffer-modified-p nil)
    t))
;; -------------------------------------------------------------------------
;; Section: Marking and unmarking.

(defun archive-flag-deleted (p &optional type)
  "In archive mode, mark this member to be deleted from the archive.
With a prefix argument, mark that many files."
  (interactive "p")
  (or type (setq type ?D))
  (beginning-of-line)
  (let ((sign (if (>= p 0) +1 -1))
	(modified (buffer-modified-p))
        buffer-read-only)
    (while (not (zerop p))
      (if (archive-get-descr t)
          (progn
            (delete-char 1)
            (insert type)))
      (forward-line sign)
      (setq p (- p sign)))
    (set-buffer-modified-p modified))
  (archive-next-line 0))

(defun archive-unflag (p)
  "In archive mode, un-mark this member if it is marked to be deleted.
With a prefix argument, un-mark that many files forward."
  (interactive "p")
  (archive-flag-deleted p ? ))

(defun archive-unflag-backwards (p)
  "In archive mode, un-mark this member if it is marked to be deleted.
With a prefix argument, un-mark that many members backward."
  (interactive "p")
  (archive-flag-deleted (- p) ? ))

(defun archive-unmark-all-files ()
  "Remove all marks."
  (interactive)
  (let ((modified (buffer-modified-p))
	buffer-read-only)
    (save-excursion
      (goto-char archive-file-list-start)
      (while (< (point) archive-file-list-end)
        (or (= (following-char) ? )
            (progn (delete-char 1) (insert ? )))
        (forward-line 1)))
    (set-buffer-modified-p modified)))

(defun archive-mark (p)
  "In archive mode, mark this member for group operations.
With a prefix argument, mark that many members.
Use \\[archive-unmark-all-files] to remove all marks."
  (interactive "p")
  (archive-flag-deleted p ?*))

(defun archive-get-marked (mark &optional default)
  (let (files)
    (save-excursion
      (goto-char archive-file-list-start)
      (while (< (point) archive-file-list-end)
        (if (= (following-char) mark)
	    (setq files (cons (archive-get-descr) files)))
        (forward-line 1)))
    (or (nreverse files)
	(and default
	     (list (archive-get-descr))))))
;; -------------------------------------------------------------------------
;; Section: Operate

(defun archive-next-line (p)
  (interactive "p")
  (forward-line p)
  (or (eobp)
      (forward-char archive-file-name-indent)))

(defun archive-previous-line (p)
  (interactive "p")
  (archive-next-line (- p)))

(defun archive-chmod-entry (new-mode)
  "Change the protection bits associated with all marked or this member
in the archive.\n\
The new protection bits can either be specified as an octal number or
as a relative change like \"g+rw\" as for chmod(2)"
  (interactive "sNew mode (octal or relative): ")
  (if archive-read-only (error "Archive is read-only"))
  (let ((func (archive-name "chmod-entry")))
    (if (fboundp func)
	(progn
	  (funcall func new-mode (archive-get-marked ?* t))
	  (archive-resummarize))
      (error "Setting mode bits is not supported for this archive type"))))

(defun archive-chown-entry (new-uid)
  "Change the owner of all marked or this member."
  (interactive "nNew uid: ")
  (if archive-read-only (error "Archive is read-only"))
  (let ((func (archive-name "chown-entry")))
    (if (fboundp func)
	(progn
	  (funcall func new-uid (archive-get-marked ?* t))
	  (archive-resummarize))
      (error "Setting owner is not supported for this archive type"))))

(defun archive-chgrp-entry (new-gid)
  "Change the group of all marked or this member."
  (interactive "nNew gid: ")
  (if archive-read-only (error "Archive is read-only"))
  (let ((func (archive-name "chgrp-entry")))
    (if (fboundp func)
	(progn
	  (funcall func new-gid (archive-get-marked ?* t))
	  (archive-resummarize))
      (error "Setting group is not supported for this archive type"))))

(defun archive-expunge ()
  "Do the flagged deletions."
  (interactive)
  (let (files)
    (save-excursion
      (goto-char archive-file-list-start)
      (while (< (point) archive-file-list-end)
        (if (= (following-char) ?D)
	    (setq files (cons (aref (archive-get-descr) 0) files)))
        (forward-line 1)))
    (setq files (nreverse files))
    (and files
	 (or (not archive-read-only)
	     (error "Archive is read-only"))
	 (or (yes-or-no-p (format "Really delete %d member%s? "
				  (length files)
				  (if (null (cdr files)) "" "s")))
	     (error "Operation aborted"))
	 (let ((archive (archive-maybe-copy (buffer-file-name)))
	       (expunger (archive-name "expunge")))
	   (if (fboundp expunger)
	       (funcall expunger archive files)
	     (archive-*-expunge archive files (symbol-value expunger)))
	   (archive-maybe-update nil)
	   (if archive-remote
	       (archive-resummarize)
	     (revert-buffer))))))

(defun archive-*-expunge (archive files command)
  (apply 'call-process
	 (car command)
	 nil
	 nil
	 nil
	 (append (cdr command) (cons archive files))))

(defun archive-rename-entry (newname)
  "Change the name associated with this entry in the tar file."
  (interactive "sNew name: ")
  (if archive-read-only (error "Archive is read-only"))
  (if (string= newname "")
      (error "Archive members may not be given empty names"))
  (let ((func (archive-name "rename-entry"))
	(descr (archive-get-descr)))
    (if (fboundp func)
        (progn
	  (funcall func (buffer-file-name) newname descr)
	  (archive-resummarize))
      (error "Renaming is not supported for this archive type"))))

;; Revert the buffer and recompute the dired-like listing.
(defun archive-mode-revert (&optional no-autosave no-confirm)
  (let ((no (archive-get-lineno)))
    (setq archive-files nil)
    (let ((revert-buffer-function nil))
      (revert-buffer t t))
    (archive-mode)
    (goto-char archive-file-list-start)
    (archive-next-line no)))

(defun archive-undo ()
  "Undo in an archive buffer.
This doesn't recover lost files, it just undoes changes in the buffer itself."
  (interactive)
  (let (buffer-read-only)
    (undo)))
;; -------------------------------------------------------------------------
;; Section: Arc Archives

(defun archive-arc-summarize ()
  (let ((p 1)
	(totalsize 0)
	(maxlen 8)
        files
	visual)
    (while (and (< (+ p 29) (point-max))
		(= (char-after p) ?\C-z)
		(> (char-after (1+ p)) 0))
      (let* ((namefld (buffer-substring (+ p 2) (+ p 2 13)))
	     (fnlen   (or (string-match "\0" namefld) 13))
	     (efnname (substring namefld 0 fnlen))
             (csize   (archive-l-e (+ p 15) 4))
             (moddate (archive-l-e (+ p 19) 2))
             (modtime (archive-l-e (+ p 21) 2))
             (ucsize  (archive-l-e (+ p 25) 4))
	     (fiddle  (string= efnname (upcase efnname)))
             (ifnname (if fiddle (downcase efnname) efnname))
             (text    (format "  %8d  %-11s  %-8s  %s"
                              ucsize
                              (archive-dosdate moddate)
                              (archive-dostime modtime)
                              ifnname)))
        (setq maxlen (max maxlen fnlen)
	      totalsize (+ totalsize ucsize)
	      visual (cons (vector text
				   (- (length text) (length ifnname))
				   (length text))
			   visual)
	      files (cons (vector efnname ifnname fiddle nil (1- p))
                          files)
              p (+ p 29 csize))))
    (goto-char (point-min))
    (let ((dash (concat "- --------  -----------  --------  "
			(make-string maxlen ?-)
			"\n")))
      (insert "M   Length  Date         Time      File\n"
	      dash)
      (archive-summarize-files (nreverse visual))
      (insert dash
	      (format "  %8d                         %d file%s"
		      totalsize
		      (length files)
		      (if (= 1 (length files)) "" "s"))
	      "\n"))
    (apply 'vector (nreverse files))))

(defun archive-arc-rename-entry (archive newname descr)
  (if (string-match "[:\\\\/]" newname)
      (error "File names in arc files may not contain a path"))
  (if (> (length newname) 12)
      (error "File names in arc files are limited to 12 characters"))
  (let ((name (concat newname (substring "\0\0\0\0\0\0\0\0\0\0\0\0\0"
					 (length newname))))
	buffer-read-only)
    (save-restriction
      (save-excursion
	(widen)
	(goto-char (+ archive-proper-file-start (aref descr 4) 2))
	(delete-char 13)
	(insert name)))))
;; -------------------------------------------------------------------------
;; Section: Lzh Archives

(defun archive-lzh-summarize ()
  (let ((p 1)
	(totalsize 0)
	(maxlen 8)
        files
	visual)
    (while (progn (goto-char p) (looking-at "..-l[hz][0-9]-"))
      (let* ((hsize   (char-after p))
             (csize   (archive-l-e (+ p 7) 4))
             (ucsize  (archive-l-e (+ p 11) 4))
	     (modtime (archive-l-e (+ p 15) 2))
	     (moddate (archive-l-e (+ p 17) 2))
	     (fnlen   (char-after (+ p 21)))
	     (efnname (buffer-substring (+ p 22) (+ p 22 fnlen)))
	     (fiddle  (string= efnname (upcase efnname)))
             (ifnname (if fiddle (downcase efnname) efnname))
	     (p2      (+ p 22 fnlen))
	     (creator (if (>= (- hsize fnlen) 24) (char-after (+ p2 2)) 0))
	     (mode    (if (= creator ?U) (archive-l-e (+ p2 8) 2) ?\666))
	     (modestr (if mode (archive-int-to-mode mode) "??????????"))
	     (uid     (if (= creator ?U) (archive-l-e (+ p2 10) 2)))
	     (gid     (if (= creator ?U) (archive-l-e (+ p2 12) 2)))
	     (text    (if archive-alternate-display
			  (format "  %8d  %5S  %5S  %s"
				  ucsize
				  (or uid "?")
				  (or gid "?")
				  ifnname)
			(format "  %10s  %8d  %-11s  %-8s  %s"
				modestr
				ucsize
				(archive-dosdate moddate)
				(archive-dostime modtime)
				ifnname))))
        (setq maxlen (max maxlen fnlen)
	      totalsize (+ totalsize ucsize)
	      visual (cons (vector text
				   (- (length text) (length ifnname))
				   (length text))
			   visual)
	      files (cons (vector efnname ifnname fiddle mode (1- p))
                          files)
              p (+ p hsize 2 csize))))
    (goto-char (point-min))
    (let ((dash (concat (if archive-alternate-display
			    "- --------  -----  -----  "
			  "- ----------  --------  -----------  --------  ")
			(make-string maxlen ?-)
			"\n"))
	  (header (if archive-alternate-display
		       "M   Length    Uid    Gid  File\n"
		    "M   Filemode    Length  Date         Time      File\n"))
	  (sumline (if archive-alternate-display
		       "  %8d                %d file%s"
		     "              %8d                         %d file%s")))
      (insert header dash)
      (archive-summarize-files (nreverse visual))
      (insert dash
	      (format sumline
		      totalsize
		      (length files)
		      (if (= 1 (length files)) "" "s"))
	      "\n"))
    (apply 'vector (nreverse files))))

(defconst archive-lzh-alternate-display t)

(defun archive-lzh-extract (archive name)
  (archive-extract-by-stdout archive name archive-lzh-extract))

(defun archive-lzh-resum (p count)
  (let ((sum 0))
    (while (> count 0)
      (setq count (1- count)
	    sum (+ sum (char-after p))
	    p (1+ p)))
    (logand sum 255)))

(defun archive-lzh-rename-entry (archive newname descr)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((p        (+ archive-proper-file-start (aref descr 4)))
	     (oldhsize (char-after p))
	     (oldfnlen (char-after (+ p 21)))
	     (newfnlen (length newname))
	     (newhsize (+ oldhsize newfnlen (- oldfnlen)))
	     buffer-read-only)
	(if (> newhsize 255)
	    (error "The file name is too long"))
	(goto-char (+ p 21))
	(delete-char (1+ oldfnlen))
	(insert newfnlen newname)
	(goto-char p)
	(delete-char 2)
	(insert newhsize (archive-lzh-resum p newhsize))))))

(defun archive-lzh-ogm (newval files errtxt ofs)
  (save-restriction
    (save-excursion
      (widen)
      (while files
	(let* ((fil (car files))
	       (p (+ archive-proper-file-start (aref fil 4)))
	       (hsize   (char-after p))
	       (fnlen   (char-after (+ p 21)))
	       (p2      (+ p 22 fnlen))
	       (creator (if (>= (- hsize fnlen) 24) (char-after (+ p2 2)) 0))
	       buffer-read-only)
	  (if (= creator ?U)
	      (progn
		(or (numberp newval)
		    (setq newval (funcall newval (archive-l-e (+ p2 ofs) 2))))
		(goto-char (+ p2 ofs))
		(delete-char 2)
		(insert (logand newval 255) (lsh newval -8))
		(goto-char (1+ p))
		(delete-char 1)
		(insert (archive-lzh-resum (1+ p) hsize)))
	    (message "Member %s does not have %s field"
		     (aref fil 1) errtxt)))
	(setq files (cdr files))))))

(defun archive-lzh-chown-entry (newuid files)
  (archive-lzh-ogm newuid files "an uid" 10))

(defun archive-lzh-chgrp-entry (newgid files)
  (archive-lzh-ogm newgid files "a gid" 12))

(defun archive-lzh-chmod-entry (newmode files)
  (archive-lzh-ogm
   ;; This should work even though newmode will be dynamically accessed.
   (lambda (old) (archive-calc-mode old newmode t))
   files "a unix-style mode" 8))
;; -------------------------------------------------------------------------
;; Section: Zip Archives

(defun archive-zip-summarize ()
  (goto-char (- (point-max) (- 22 18)))
  (search-backward-regexp "[P]K\005\006")
  (let ((p (1+ (archive-l-e (+ (point) 16) 4)))
        (maxlen 8)
	(totalsize 0)
        files
	visual)
    (while (string= "PK\001\002" (buffer-substring p (+ p 4)))
      (let* ((creator (char-after (+ p 5)))
	     (method  (archive-l-e (+ p 10) 2))
             (modtime (archive-l-e (+ p 12) 2))
             (moddate (archive-l-e (+ p 14) 2))
             (ucsize  (archive-l-e (+ p 24) 4))
             (fnlen   (archive-l-e (+ p 28) 2))
             (exlen   (archive-l-e (+ p 30) 2))
             (lheader (archive-l-e (+ p 42) 4))
             (efnname (buffer-substring (+ p 46) (+ p 46 fnlen)))
	     (isdir   (and (= ucsize 0)
			   (string= (file-name-nondirectory efnname) "")))
	     (mode    (cond ((memq creator '(2 3)) ; Unix + VMS
			     (archive-l-e (+ p 40) 2))
			    ((memq creator '(0 5 6 7 10 11)) ; Dos etc.
			     (logior ?\444
				     (if isdir (logior 16384 ?\111) 0)
				     (if (zerop
					  (logand 1 (char-after (+ p 38))))
					 ?\222 0)))
			    (t nil)))
	     (modestr (if mode (archive-int-to-mode mode) "??????????"))
	     (fiddle  (and archive-zip-case-fiddle
			   (not (not (memq creator '(0 2 4 5 9))))))
             (ifnname (if fiddle (downcase efnname) efnname))
             (text    (format "  %10s  %8d  %-11s  %-8s  %s"
			      modestr
                              ucsize
                              (archive-dosdate moddate)
                              (archive-dostime modtime)
                              ifnname)))
        (setq maxlen (max maxlen fnlen)
	      totalsize (+ totalsize ucsize)
	      visual (cons (vector text
				   (- (length text) (length ifnname))
				   (length text))
			   visual)
	      files (cons (if isdir
			      nil
			    (vector efnname ifnname fiddle mode
				    (list (1- p) lheader)))
                          files)
              p (+ p 46 fnlen exlen))))
    (goto-char (point-min))
    (let ((dash (concat "- ----------  --------  -----------  --------  "
			(make-string maxlen ?-)
			"\n")))
      (insert "M Filemode      Length  Date         Time      File\n"
	      dash)
      (archive-summarize-files (nreverse visual))
      (insert dash
	      (format "              %8d                         %d file%s"
		      totalsize
		      (length files)
		      (if (= 1 (length files)) "" "s"))
	      "\n"))
    (apply 'vector (nreverse files))))

(defun archive-zip-extract (archive name)
  (if archive-zip-use-pkzip
      (archive-*-extract archive name archive-zip-extract)
    (archive-extract-by-stdout archive name archive-zip-extract)))

(defun archive-zip-write-file-member (archive descr)
  (archive-*-write-file-member
   archive
   descr
   (if (aref descr 2) archive-zip-update-case archive-zip-update)))

(defun archive-zip-chmod-entry (newmode files)
  (save-restriction
    (save-excursion
      (widen)
      (while files
	(let* ((fil (car files))
	       (p (+ archive-proper-file-start (car (aref fil 4))))
	       (creator (char-after (+ p 5)))
	       (oldmode (aref fil 3))
	       (newval  (archive-calc-mode oldmode newmode t))
	       buffer-read-only)
	  (cond ((memq creator '(2 3)) ; Unix + VMS
		 (goto-char (+ p 40))
		 (delete-char 2)
		 (insert (logand newval 255) (lsh newval -8)))
		((memq creator '(0 5 6 7 10 11)) ; Dos etc.
		 (goto-char (+ p 38))
		 (insert (logior (logand (char-after (point)) 254)
				 (logand (logxor 1 (lsh newval -7)) 1)))
		 (delete-char 1))
		(t (message "Don't know how to change mode for this member"))))
	(setq files (cdr files))))))
;; -------------------------------------------------------------------------
;; Section: Zoo Archives

(defun archive-zoo-summarize ()
  (let ((p (1+ (archive-l-e 25 4)))
        (maxlen 8)
	(totalsize 0)
        files
	visual)
    (while (and (string= "\334\247\304\375" (buffer-substring p (+ p 4)))
		(> (archive-l-e (+ p 6) 4) 0))
      (let* ((next    (1+ (archive-l-e (+ p 6) 4)))
             (moddate (archive-l-e (+ p 14) 2))
             (modtime (archive-l-e (+ p 16) 2))
             (ucsize  (archive-l-e (+ p 20) 4))
	     (namefld (buffer-substring (+ p 38) (+ p 38 13)))
	     (fnlen   (or (string-match "\0" namefld) 13))
	     (efnname (substring namefld 0 fnlen))
	     (fiddle  (string= efnname (upcase efnname)))
             (ifnname (if fiddle (downcase efnname) efnname))
             (text    (format "  %8d  %-11s  %-8s  %s"
                              ucsize
                              (archive-dosdate moddate)
                              (archive-dostime modtime)
                              ifnname)))
        (setq maxlen (max maxlen fnlen)
	      totalsize (+ totalsize ucsize)
	      visual (cons (vector text
				   (- (length text) (length ifnname))
				   (length text))
			   visual)
	      files (cons (vector efnname ifnname fiddle nil (1- p))
                          files)
              p next)))
    (goto-char (point-min))
    (let ((dash (concat "- --------  -----------  --------  "
			(make-string maxlen ?-)
			"\n")))
      (insert "M   Length  Date         Time      File\n"
	      dash)
      (archive-summarize-files (nreverse visual))
      (insert dash
	      (format "  %8d                         %d file%s"
		      totalsize
		      (length files)
		      (if (= 1 (length files)) "" "s"))
	      "\n"))
    (apply 'vector (nreverse files))))

(defun archive-zoo-extract (archive name)
  (archive-extract-by-stdout archive name archive-zoo-extract))
;; -------------------------------------------------------------------------
(provide 'archive-mode)

;; arc-mode.el ends here.
