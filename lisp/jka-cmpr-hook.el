;;; jka-cmpr-hook.el --- preloaded code to enable jka-compr.el

;; Copyright (C) 1993, 1994, 1995, 1997, 1999, 2000, 2002, 2003,
;;   2004, 2005 Free Software Foundation, Inc.

;; Author: jka@ece.cmu.edu (Jay K. Adams)
;; Maintainer: FSF
;; Keywords: data

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains the  code to enable and disable Auto-Compression mode.
;; It is preloaded.  The guts of this mode are in jka-compr.el, which
;; is loaded only when you really try to uncompress something.

;;; Code:

(defgroup compression nil
  "Data compression utilities."
  :group 'data)

(defgroup jka-compr nil
  "jka-compr customization."
  :group 'compression)

;;; I have this defined so that .Z files are assumed to be in unix
;;; compress format; and .gz files, in gzip format, and .bz2 files in bzip fmt.
(defcustom jka-compr-compression-info-list
  ;;[regexp
  ;; compr-message  compr-prog  compr-args
  ;; uncomp-message uncomp-prog uncomp-args
  ;; can-append auto-mode-flag strip-extension-flag file-magic-bytes]
  '(["\\.Z\\(~\\|\\.~[0-9]+~\\)?\\'"
     "compressing"    "compress"     ("-c")
     "uncompressing"  "uncompress"   ("-c")
     nil t "\037\235"]
     ;; Formerly, these had an additional arg "-c", but that fails with
     ;; "Version 0.1pl2, 29-Aug-97." (RedHat 5.1 GNU/Linux) and
     ;; "Version 0.9.0b, 9-Sept-98".
    ["\\.bz2\\'"
     "bzip2ing"        "bzip2"         nil
     "bunzip2ing"      "bzip2"         ("-d")
     nil t "BZh"]
    ["\\.tbz\\'"
     "bzip2ing"        "bzip2"         nil
     "bunzip2ing"      "bzip2"         ("-d")
     nil nil "BZh"]
    ["\\.tgz\\'"
     "compressing"        "gzip"         ("-c" "-q")
     "uncompressing"      "gzip"         ("-c" "-q" "-d")
     t nil "\037\213"]
    ["\\.g?z\\(~\\|\\.~[0-9]+~\\)?\\'"
     "compressing"        "gzip"         ("-c" "-q")
     "uncompressing"      "gzip"         ("-c" "-q" "-d")
     t t "\037\213"]
    ;; dzip is gzip with random access.  Its compression program can't
    ;; read/write stdin/out, so .dz files can only be viewed without
    ;; saving, having their contents decompressed with gzip.
    ["\\.dz\\'"
     nil              nil            nil
     "uncompressing"      "gzip"         ("-c" "-q" "-d")
     nil t "\037\213"])

  "List of vectors that describe available compression techniques.
Each element, which describes a compression technique, is a vector of
the form [REGEXP COMPRESS-MSG COMPRESS-PROGRAM COMPRESS-ARGS
UNCOMPRESS-MSG UNCOMPRESS-PROGRAM UNCOMPRESS-ARGS
APPEND-FLAG STRIP-EXTENSION-FLAG FILE-MAGIC-CHARS], where:

   regexp                is a regexp that matches filenames that are
                         compressed with this format

   compress-msg          is the message to issue to the user when doing this
                         type of compression (nil means no message)

   compress-program      is a program that performs this compression
                         (nil means visit file in read-only mode)

   compress-args         is a list of args to pass to the compress program

   uncompress-msg        is the message to issue to the user when doing this
                         type of uncompression (nil means no message)

   uncompress-program    is a program that performs this compression

   uncompress-args       is a list of args to pass to the uncompress program

   append-flag           is non-nil if this compression technique can be
                         appended

   strip-extension-flag  non-nil means strip the regexp from file names
                         before attempting to set the mode.

   file-magic-chars      is a string of characters that you would find
			 at the beginning of a file compressed in this way.

Because of the way `call-process' is defined, discarding the stderr output of
a program adds the overhead of starting a shell each time the program is
invoked."
  :type '(repeat (vector regexp
			 (choice :tag "Compress Message"
				 (string :format "%v")
				 (const :tag "No Message" nil))
			 (choice :tag "Compress Program"
				 (string)
				 (const :tag "None" nil))
			 (repeat :tag "Compress Arguments" string)
			 (choice :tag "Uncompress Message"
				 (string :format "%v")
				 (const :tag "No Message" nil))
			 (choice :tag "Uncompress Program"
				 (string)
				 (const :tag "None" nil))
			 (repeat :tag "Uncompress Arguments" string)
			 (boolean :tag "Append")
			 (boolean :tag "Strip Extension")
			 (string :tag "Magic Bytes")))
  :group 'jka-compr)

(defcustom jka-compr-mode-alist-additions
  (list (cons "\\.tgz\\'" 'tar-mode) (cons "\\.tbz\\'" 'tar-mode))
  "A list of pairs to add to `auto-mode-alist' when jka-compr is installed."
  :type '(repeat (cons string symbol))
  :group 'jka-compr)

(defcustom jka-compr-load-suffixes '(".gz")
  "List of suffixes to try when loading files."
  :type '(repeat string)
  :group 'jka-compr)

;; List of all the elements we actually added to file-coding-system-alist.
(defvar jka-compr-added-to-file-coding-system-alist nil)

(defvar jka-compr-file-name-handler-entry
  nil
  "The entry in `file-name-handler-alist' used by the jka-compr I/O functions.")

(defun jka-compr-build-file-regexp ()
  (mapconcat
   'jka-compr-info-regexp
   jka-compr-compression-info-list
   "\\|"))

;;; Functions for accessing the return value of jka-compr-get-compression-info
(defun jka-compr-info-regexp               (info)  (aref info 0))
(defun jka-compr-info-compress-message     (info)  (aref info 1))
(defun jka-compr-info-compress-program     (info)  (aref info 2))
(defun jka-compr-info-compress-args        (info)  (aref info 3))
(defun jka-compr-info-uncompress-message   (info)  (aref info 4))
(defun jka-compr-info-uncompress-program   (info)  (aref info 5))
(defun jka-compr-info-uncompress-args      (info)  (aref info 6))
(defun jka-compr-info-can-append           (info)  (aref info 7))
(defun jka-compr-info-strip-extension      (info)  (aref info 8))
(defun jka-compr-info-file-magic-bytes     (info)  (aref info 9))


(defun jka-compr-get-compression-info (filename)
  "Return information about the compression scheme of FILENAME.
The determination as to which compression scheme, if any, to use is
based on the filename itself and `jka-compr-compression-info-list'."
  (catch 'compression-info
    (let ((case-fold-search nil))
      (mapcar
       (function (lambda (x)
		   (and (string-match (jka-compr-info-regexp x) filename)
			(throw 'compression-info x))))
       jka-compr-compression-info-list)
      nil)))

(defun jka-compr-install ()
  "Install jka-compr.
This adds entries to `file-name-handler-alist' and `auto-mode-alist'
and `inhibit-first-line-modes-suffixes'."

  (setq jka-compr-file-name-handler-entry
	(cons (jka-compr-build-file-regexp) 'jka-compr-handler))

  (setq file-name-handler-alist (cons jka-compr-file-name-handler-entry
				      file-name-handler-alist))

  (setq jka-compr-added-to-file-coding-system-alist nil)

  (mapcar
   (function (lambda (x)
	       ;; Don't do multibyte encoding on the compressed files.
	       (let ((elt (cons (jka-compr-info-regexp x)
				 '(no-conversion . no-conversion))))
		 (setq file-coding-system-alist
		       (cons elt file-coding-system-alist))
		 (setq jka-compr-added-to-file-coding-system-alist
		       (cons elt jka-compr-added-to-file-coding-system-alist)))

	       (and (jka-compr-info-strip-extension x)
		    ;; Make entries in auto-mode-alist so that modes
		    ;; are chosen right according to the file names
		    ;; sans `.gz'.
		    (setq auto-mode-alist
			  (cons (list (jka-compr-info-regexp x)
				      nil 'jka-compr)
				auto-mode-alist))
		    ;; Also add these regexps to
		    ;; inhibit-first-line-modes-suffixes, so that a
		    ;; -*- line in the first file of a compressed tar
		    ;; file doesn't override tar-mode.
		    (setq inhibit-first-line-modes-suffixes
			  (cons (jka-compr-info-regexp x)
				inhibit-first-line-modes-suffixes)))))
   jka-compr-compression-info-list)
  (setq auto-mode-alist
	(append auto-mode-alist jka-compr-mode-alist-additions))

  ;; Make sure that (load "foo") will find /bla/foo.el.gz.
  (setq load-suffixes
	(apply 'append
	       (mapcar (lambda (suffix)
			 (cons suffix
			       (mapcar (lambda (ext) (concat suffix ext))
				       jka-compr-load-suffixes)))
		       load-suffixes))))


(defun jka-compr-installed-p ()
  "Return non-nil if jka-compr is installed.
The return value is the entry in `file-name-handler-alist' for jka-compr."

  (let ((fnha file-name-handler-alist)
	(installed nil))

    (while (and fnha (not installed))
     (and (eq (cdr (car fnha)) 'jka-compr-handler)
	   (setq installed (car fnha)))
      (setq fnha (cdr fnha)))

    installed))

(define-minor-mode auto-compression-mode
  "Toggle automatic file compression and uncompression.
With prefix argument ARG, turn auto compression on if positive, else off.
Returns the new status of auto compression (non-nil means on)."
  :global t :group 'jka-compr
  (let* ((installed (jka-compr-installed-p))
	 (flag auto-compression-mode))
    (cond
     ((and flag installed) t)		; already installed
     ((and (not flag) (not installed)) nil) ; already not installed
     (flag (jka-compr-install))
     (t (jka-compr-uninstall)))))

(defmacro with-auto-compression-mode (&rest body)
  "Evalute BODY with automatic file compression and uncompression enabled."
  (let ((already-installed (make-symbol "already-installed")))
    `(let ((,already-installed (jka-compr-installed-p)))
       (unwind-protect
	   (progn
	     (unless ,already-installed
	       (jka-compr-install))
	     ,@body)
	 (unless ,already-installed
	   (jka-compr-uninstall))))))
(put 'with-auto-compression-mode 'lisp-indent-function 0)


;;; This is what we need to know about jka-compr-handler
;;; in order to decide when to call it.

(put 'jka-compr-handler 'safe-magic t)
(put 'jka-compr-handler 'operations '(jka-compr-byte-compiler-base-file-name
				      write-region insert-file-contents
				      file-local-copy load))

;;; Turn on the mode.
(auto-compression-mode 1)

(provide 'jka-cmpr-hook)

;; arch-tag: 4bd73429-f400-45fe-a065-270a113e31a8
;;; jka-cmpr-hook.el ends here
