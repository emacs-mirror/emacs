;;; jka-compr.el --- reading/writing/loading compressed files

;; Copyright (C) 1993, 1994, 1995, 1997, 1999  Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary: 

;; This package implements low-level support for reading, writing,
;; and loading compressed files.  It hooks into the low-level file
;; I/O functions (including write-region and insert-file-contents) so
;; that they automatically compress or uncompress a file if the file
;; appears to need it (based on the extension of the file name).
;; Packages like Rmail, VM, GNUS, and Info should be able to work
;; with compressed files without modification.


;; INSTRUCTIONS:
;;
;; To use jka-compr, simply load this package, and edit as usual.
;; Its operation should be transparent to the user (except for
;; messages appearing when a file is being compressed or
;; uncompressed).
;;
;; The variable, jka-compr-compression-info-list can be used to
;; customize jka-compr to work with other compression programs.
;; The default value of this variable allows jka-compr to work with
;; Unix compress and gzip.
;;
;; If you are concerned about the stderr output of gzip and other
;; compression/decompression programs showing up in your buffers, you
;; should set the discard-error flag in the compression-info-list.
;; This will cause the stderr of all programs to be discarded.
;; However, it also causes emacs to call compression/uncompression
;; programs through a shell (which is specified by jka-compr-shell).
;; This may be a drag if, on your system, starting up a shell is
;; slow.
;;
;; If you don't want messages about compressing and decompressing
;; to show up in the echo area, you can set the compress-name and
;; decompress-name fields of the jka-compr-compression-info-list to
;; nil.


;; APPLICATION NOTES:
;;
;; crypt++
;;   jka-compr can coexist with crpyt++ if you take all the decompression
;;   entries out of the crypt-encoding-list.  Clearly problems will arise if
;;   you have two programs trying to compress/decompress files.  jka-compr
;;   will not "work with" crypt++ in the following sense: you won't be able to
;;   decode encrypted compressed files--that is, files that have been
;;   compressed then encrypted (in that order).  Theoretically, crypt++ and
;;   jka-compr could properly handle a file that has been encrypted then
;;   compressed, but there is little point in trying to compress an encrypted
;;   file.
;;


;; ACKNOWLEDGMENTS
;; 
;; jka-compr is a V19 adaptation of jka-compr for V18 of Emacs.  Many people
;; have made helpful suggestions, reported bugs, and even fixed bugs in 
;; jka-compr.  I recall the following people as being particularly helpful.
;;
;;   Jean-loup Gailly
;;   David Hughes
;;   Richard Pieri
;;   Daniel Quinlan
;;   Chris P. Ross
;;   Rick Sladkey
;;
;; Andy Norman's ange-ftp was the inspiration for the original jka-compr for
;; Version 18 of Emacs.
;;
;; After I had made progress on the original jka-compr for V18, I learned of a
;; package written by Kazushi Jam Marukawa, called jam-zcat, that did exactly
;; what I was trying to do.  I looked over the jam-zcat source code and
;; probably got some ideas from it.
;;

;;; Code:

(defgroup compression nil
  "Data compression utilities"
  :group 'data)

(defgroup jka-compr nil
  "jka-compr customization"
  :group 'compression)


(defcustom jka-compr-shell "sh"
  "*Shell to be used for calling compression programs.
The value of this variable only matters if you want to discard the
stderr of a compression/decompression program (see the documentation
for `jka-compr-compression-info-list')."
  :type 'string
  :group 'jka-compr)

(defvar jka-compr-use-shell 
  (not (memq system-type '(ms-dos windows-nt))))

;;; I have this defined so that .Z files are assumed to be in unix
;;; compress format; and .gz files, in gzip format, and .bz2 files in bzip fmt.
(defcustom jka-compr-compression-info-list
  ;;[regexp
  ;; compr-message  compr-prog  compr-args
  ;; uncomp-message uncomp-prog uncomp-args
  ;; can-append auto-mode-flag]
  '(["\\.Z\\(~\\|\\.~[0-9]+~\\)?\\'"
     "compressing"    "compress"     ("-c")
     "uncompressing"  "uncompress"   ("-c")
     nil t]
     ;; Formerly, these had an additional arg "-c", but that fails with
     ;; "Version 0.1pl2, 29-Aug-97." (RedHat 5.1 GNU/Linux) and
     ;; "Version 0.9.0b, 9-Sept-98".
    ["\\.bz2\\'"
     "bzip2ing"        "bzip2"         nil
     "bunzip2ing"      "bzip2"         ("-d")
     nil t]
    ["\\.tgz\\'"
     "zipping"        "gzip"         ("-c" "-q")
     "unzipping"      "gzip"         ("-c" "-q" "-d")
     t nil]
    ["\\.gz\\(~\\|\\.~[0-9]+~\\)?\\'"
     "zipping"        "gzip"         ("-c" "-q")
     "unzipping"      "gzip"         ("-c" "-q" "-d")
     t t])

  "List of vectors that describe available compression techniques.
Each element, which describes a compression technique, is a vector of
the form [REGEXP COMPRESS-MSG COMPRESS-PROGRAM COMPRESS-ARGS
UNCOMPRESS-MSG UNCOMPRESS-PROGRAM UNCOMPRESS-ARGS
APPEND-FLAG EXTENSION], where:

   regexp                is a regexp that matches filenames that are
                         compressed with this format

   compress-msg          is the message to issue to the user when doing this
                         type of compression (nil means no message)

   compress-program      is a program that performs this compression

   compress-args         is a list of args to pass to the compress program

   uncompress-msg        is the message to issue to the user when doing this
                         type of uncompression (nil means no message)

   uncompress-program    is a program that performs this compression

   uncompress-args       is a list of args to pass to the uncompress program

   append-flag           is non-nil if this compression technique can be
                         appended

   auto-mode flag        non-nil means strip the regexp from file names
                         before attempting to set the mode.

Because of the way `call-process' is defined, discarding the stderr output of
a program adds the overhead of starting a shell each time the program is
invoked."
  :type '(repeat (vector regexp
			 (choice :tag "Compress Message"
				 (string :format "%v")
				 (const :tag "No Message" nil))
			 (string :tag "Compress Program")
			 (repeat :tag "Compress Arguments" string)
			 (choice :tag "Uncompress Message"
				 (string :format "%v")
				 (const :tag "No Message" nil))
			 (string :tag "Uncompress Program")
			 (repeat :tag "Uncompress Arguments" string)
			 (boolean :tag "Append")
			 (boolean :tag "Auto Mode")))
  :group 'jka-compr)

(defvar jka-compr-mode-alist-additions
  (list (cons "\\.tgz\\'" 'tar-mode))
  "A list of pairs to add to `auto-mode-alist' when jka-compr is installed.")

;; List of all the elements we actually added to file-coding-system-alist.
(defvar jka-compr-added-to-file-coding-system-alist nil)

(defvar jka-compr-file-name-handler-entry
  nil
  "The entry in `file-name-handler-alist' used by the jka-compr I/O functions.")

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


(put 'compression-error 'error-conditions '(compression-error file-error error))


(defvar jka-compr-acceptable-retval-list '(0 2 141))


(defun jka-compr-error (prog args infile message &optional errfile)

  (let ((errbuf (get-buffer-create " *jka-compr-error*"))
	(curbuf (current-buffer)))
    (with-current-buffer errbuf
      (widen) (erase-buffer)
      (insert (format "Error while executing \"%s %s < %s\"\n\n"
		      prog
		      (mapconcat 'identity args " ")
		      infile))

      (and errfile
	   (insert-file-contents errfile)))
     (display-buffer errbuf))

  (signal 'compression-error
	  (list "Opening input file" (format "error %s" message) infile)))
			
   
(defvar jka-compr-dd-program
  "/bin/dd")


(defvar jka-compr-dd-blocksize 256)


(defun jka-compr-partial-uncompress (prog message args infile beg len)
  "Call program PROG with ARGS args taking input from INFILE.
Fourth and fifth args, BEG and LEN, specify which part of the output
to keep: LEN chars starting BEG chars from the beginning."
  (let* ((skip (/ beg jka-compr-dd-blocksize))
	 (prefix (- beg (* skip jka-compr-dd-blocksize)))
	 (count (and len (1+ (/ (+ len prefix) jka-compr-dd-blocksize))))
	 (start (point))
	 (err-file (jka-compr-make-temp-name))
	 (run-string (format "%s %s 2> %s | %s bs=%d skip=%d %s 2> /dev/null"
			     prog
			     (mapconcat 'identity args " ")
			     err-file
			     jka-compr-dd-program
			     jka-compr-dd-blocksize
			     skip
			     ;; dd seems to be unreliable about
			     ;; providing the last block.  So, always
			     ;; read one more than you think you need.
			     (if count (concat "count=" (1+ count)) ""))))

    (unwind-protect
	(or (memq (call-process jka-compr-shell
				infile t nil "-c"
				run-string)
		  jka-compr-acceptable-retval-list)
	    
	    (jka-compr-error prog args infile message err-file))

      (jka-compr-delete-temp-file err-file))

    ;; Delete the stuff after what we want, if there is any.
    (and
     len
     (< (+ start prefix len) (point))
     (delete-region (+ start prefix len) (point)))

    ;; Delete the stuff before what we want.
    (delete-region start (+ start prefix))))


(defun jka-compr-call-process (prog message infile output temp args)
  (if jka-compr-use-shell

      (let ((err-file (jka-compr-make-temp-name))
	    (coding-system-for-read (or coding-system-for-read 'undecided))
            (coding-system-for-write 'no-conversion))

	(unwind-protect

	    (or (memq
		 (call-process jka-compr-shell infile
			       (if (stringp output) nil output)
			       nil
			       "-c"
			       (format "%s %s 2> %s %s"
				       prog
				       (mapconcat 'identity args " ")
				       err-file
				       (if (stringp output)
					   (concat "> " output)
					 "")))
		 jka-compr-acceptable-retval-list)

		(jka-compr-error prog args infile message err-file))

	  (jka-compr-delete-temp-file err-file)))

    (or (zerop
	 (apply 'call-process
		prog
		infile
		(if (stringp output) temp output)
		nil
		args))
	(jka-compr-error prog args infile message))

    (and (stringp output)
	 (with-current-buffer temp
	   (write-region (point-min) (point-max) output)
	   (erase-buffer)))))


;;; Support for temp files.  Much of this was inspired if not lifted
;;; from ange-ftp.

(defcustom jka-compr-temp-name-template
  (expand-file-name "jka-com" temporary-file-directory)
  "Prefix added to all temp files created by jka-compr.
There should be no more than seven characters after the final `/'."
  :type 'string
  :group 'jka-compr)

(defvar jka-compr-temp-name-table (make-vector 31 nil))

(defun jka-compr-make-temp-name (&optional local-copy)
  "This routine will return the name of a new file."
  (let* ((lastchar ?a)
	 (prevchar ?a)
	 (template (concat jka-compr-temp-name-template "aa"))
	 (lastpos (1- (length template)))
	 (not-done t)
	 file
	 entry)

    (while not-done
      (aset template lastpos lastchar)
      (setq file (concat (make-temp-name template) "#"))
      (setq entry (intern file jka-compr-temp-name-table))
      (if (or (get entry 'active)
	      (file-exists-p file))

	  (progn
	    (setq lastchar (1+ lastchar))
	    (if (> lastchar ?z)
		(progn
		  (setq prevchar (1+ prevchar))
		  (setq lastchar ?a)
		  (if (> prevchar ?z)
		      (error "Can't allocate temp file.")
		    (aset template (1- lastpos) prevchar)))))

	(put entry 'active (not local-copy))
	(setq not-done nil)))

    file))


(defun jka-compr-delete-temp-file (temp)

  (put (intern temp jka-compr-temp-name-table)
       'active nil)

  (condition-case ()
      (delete-file temp)
    (error nil)))


(defun jka-compr-write-region (start end file &optional append visit)
  (let* ((filename (expand-file-name file))
	 (visit-file (if (stringp visit) (expand-file-name visit) filename))
	 (info (jka-compr-get-compression-info visit-file)))
      
      (if info

	  (let ((can-append (jka-compr-info-can-append info))
		(compress-program (jka-compr-info-compress-program info))
		(compress-message (jka-compr-info-compress-message info))
		(uncompress-program (jka-compr-info-uncompress-program info))
		(uncompress-message (jka-compr-info-uncompress-message info))
		(compress-args (jka-compr-info-compress-args info))
		(uncompress-args (jka-compr-info-uncompress-args info))
		(base-name (file-name-nondirectory visit-file))
		temp-file temp-buffer
		;; we need to leave `last-coding-system-used' set to its
		;; value after calling write-region the first time, so
		;; that `basic-save-buffer' sees the right value.
		(coding-system-used last-coding-system-used))

	    (setq temp-buffer (get-buffer-create " *jka-compr-wr-temp*"))
	    (with-current-buffer temp-buffer
	      (widen) (erase-buffer))

	    (if (and append
		     (not can-append)
		     (file-exists-p filename))
		
		(let* ((local-copy (file-local-copy filename))
		       (local-file (or local-copy filename)))
		  
		  (setq temp-file local-file))

	      (setq temp-file (jka-compr-make-temp-name)))

	    (and 
	     compress-message
	     (message "%s %s..." compress-message base-name))
	    
	    (jka-compr-run-real-handler 'write-region
					(list start end temp-file t 'dont))
	    ;; save value used by the real write-region
	    (setq coding-system-used last-coding-system-used)

	    ;; Here we must read the output of compress program as is
	    ;; without any code conversion.
	    (let ((coding-system-for-read 'no-conversion))
	      (jka-compr-call-process compress-program
				      (concat compress-message
					      " " base-name)
				      temp-file
				      temp-buffer
				      nil
				      compress-args))

	    (with-current-buffer temp-buffer
              (let ((coding-system-for-write 'no-conversion))
                (if (memq system-type '(ms-dos windows-nt))
                    (setq buffer-file-type t) )
                (jka-compr-run-real-handler 'write-region
                                            (list (point-min) (point-max)
                                                  filename
                                                  (and append can-append) 'dont))
                (erase-buffer)) )

	    (jka-compr-delete-temp-file temp-file)

	    (and
	     compress-message
	     (message "%s %s...done" compress-message base-name))

	    (cond
	     ((eq visit t)
	      (setq buffer-file-name filename)
	      (set-visited-file-modtime))
	     ((stringp visit)
	      (setq buffer-file-name visit)
	      (let ((buffer-file-name filename))
		(set-visited-file-modtime))))

	    (and (or (eq visit t)
		     (eq visit nil)
		     (stringp visit))
		 (message "Wrote %s" visit-file))

	    ;; ensure `last-coding-system-used' has an appropriate value
	    (setq last-coding-system-used coding-system-used)

	    nil)
	      
	(jka-compr-run-real-handler 'write-region
				    (list start end filename append visit)))))


(defun jka-compr-insert-file-contents (file &optional visit beg end replace)
  (barf-if-buffer-read-only)

  (and (or beg end)
       visit
       (error "Attempt to visit less than an entire file"))

  (let* ((filename (expand-file-name file))
	 (info (jka-compr-get-compression-info filename)))

    (if info

	(let ((uncompress-message (jka-compr-info-uncompress-message info))
	      (uncompress-program (jka-compr-info-uncompress-program info))
	      (uncompress-args (jka-compr-info-uncompress-args info))
	      (base-name (file-name-nondirectory filename))
	      (notfound nil)
	      (local-copy
	       (jka-compr-run-real-handler 'file-local-copy (list filename)))
	      local-file
	      size start
              (coding-system-for-read
	       (or coding-system-for-read
		   ;; If multibyte characters are disabled,
		   ;; don't do that conversion.
		   (and (null enable-multibyte-characters)
			(or (auto-coding-alist-lookup
			     (jka-compr-byte-compiler-base-file-name file))
			    'raw-text))
		   (let ((coding (find-operation-coding-system
				  'insert-file-contents
				  (jka-compr-byte-compiler-base-file-name file))))
		     (and (consp coding) (car coding)))
		   'undecided)) )

	  (setq local-file (or local-copy filename))

	  (and
	   visit
	   (setq buffer-file-name filename))

	  (unwind-protect		; to make sure local-copy gets deleted

	      (progn
		  
		(and
		 uncompress-message
		 (message "%s %s..." uncompress-message base-name))

		(condition-case error-code

		    (progn
		      (if replace
			  (goto-char (point-min)))
		      (setq start (point))
		      (if (or beg end)
			  (jka-compr-partial-uncompress uncompress-program
							(concat uncompress-message
								" " base-name)
							uncompress-args
							local-file
							(or beg 0)
							(if (and beg end)
							    (- end beg)
							  end))
			;; If visiting, bind off buffer-file-name so that
			;; file-locking will not ask whether we should
			;; really edit the buffer.
			(let ((buffer-file-name
			       (if visit nil buffer-file-name)))
			  (jka-compr-call-process uncompress-program
						  (concat uncompress-message
							  " " base-name)
						  local-file
						  t
						  nil
						  uncompress-args)))
		      (setq size (- (point) start))
		      (if replace
			  (let* ((del-beg (point))
				 (del-end (+ del-beg size)))
			    (delete-region del-beg
					   (min del-end (point-max)))))
		      (goto-char start))
		  (error
		   (if (and (eq (car error-code) 'file-error)
			    (eq (nth 3 error-code) local-file))
		       (if visit
			   (setq notfound error-code)
			 (signal 'file-error 
				 (cons "Opening input file"
				       (nthcdr 2 error-code))))
		     (signal (car error-code) (cdr error-code))))))

	    (and
	     local-copy
	     (file-exists-p local-copy)
	     (delete-file local-copy)))

	  (and
	   visit
	   (progn
	     (unlock-buffer)
	     (setq buffer-file-name filename)
	     (set-visited-file-modtime)))
	    
	  (and
	   uncompress-message
	   (message "%s %s...done" uncompress-message base-name))

	  (and
	   visit
	   notfound
	   (signal 'file-error
		   (cons "Opening input file" (nth 2 notfound))))

	  ;; This is done in insert-file-contents after we return.
	  ;; That is a little weird, but better to go along with it now
	  ;; than to change it now.

;;;	  ;; Run the functions that insert-file-contents would.
;;; 	  (let ((p after-insert-file-functions)
;;; 		(insval size))
;;; 	    (while p
;;; 	      (setq insval (funcall (car p) size))
;;; 	      (if insval
;;; 		  (progn
;;; 		    (or (integerp insval)
;;; 			(signal 'wrong-type-argument
;;; 				(list 'integerp insval)))
;;; 		    (setq size insval)))
;;; 	      (setq p (cdr p))))

	  (list filename size))

      (jka-compr-run-real-handler 'insert-file-contents
				  (list file visit beg end replace)))))


(defun jka-compr-file-local-copy (file)
  (let* ((filename (expand-file-name file))
	 (info (jka-compr-get-compression-info filename)))

    (if info

	(let ((uncompress-message (jka-compr-info-uncompress-message info))
	      (uncompress-program (jka-compr-info-uncompress-program info))
	      (uncompress-args (jka-compr-info-uncompress-args info))
	      (base-name (file-name-nondirectory filename))
	      (local-copy
	       (jka-compr-run-real-handler 'file-local-copy (list filename)))
	      (temp-file (jka-compr-make-temp-name t))
	      (temp-buffer (get-buffer-create " *jka-compr-flc-temp*"))
	      (notfound nil)
	      local-file)

	  (setq local-file (or local-copy filename))

	  (unwind-protect

	      (with-current-buffer temp-buffer
		  
		(and
		 uncompress-message
		 (message "%s %s..." uncompress-message base-name))
		  
		;; Here we must read the output of uncompress program
		;; and write it to TEMP-FILE without any code
		;; conversion.  An appropriate code conversion (if
		;; necessary) is done by the later I/O operation
		;; (e.g. load).
		(let ((coding-system-for-read 'no-conversion)
		      (coding-system-for-write 'no-conversion))

		  (jka-compr-call-process uncompress-program
					  (concat uncompress-message
						  " " base-name)
					  local-file
					  t
					  nil
					  uncompress-args)

		  (and
		   uncompress-message
		   (message "%s %s...done" uncompress-message base-name))

		  (write-region
		   (point-min) (point-max) temp-file nil 'dont)))

	    (and
	     local-copy
	     (file-exists-p local-copy)
	     (delete-file local-copy))

	    (kill-buffer temp-buffer))

	  temp-file)
	    
      (jka-compr-run-real-handler 'file-local-copy (list filename)))))


;;; Support for loading compressed files.
(defun jka-compr-load (file &optional noerror nomessage nosuffix)
  "Documented as original."

  (let* ((local-copy (jka-compr-file-local-copy file))
	 (load-file (or local-copy file)))

    (unwind-protect

	(let (inhibit-file-name-operation
	      inhibit-file-name-handlers)
	  (or nomessage
	      (message "Loading %s..." file))

	  (let ((load-force-doc-strings t))
	    (load load-file noerror t t))

	  (or nomessage
	      (message "Loading %s...done." file)))

      (jka-compr-delete-temp-file local-copy))

    t))

(defun jka-compr-byte-compiler-base-file-name (file)
  (let ((info (jka-compr-get-compression-info file)))
    (if (and info (jka-compr-info-strip-extension info))
	(save-match-data
	  (substring file 0 (string-match (jka-compr-info-regexp info) file)))
      file)))

(put 'write-region 'jka-compr 'jka-compr-write-region)
(put 'insert-file-contents 'jka-compr 'jka-compr-insert-file-contents)
(put 'file-local-copy 'jka-compr 'jka-compr-file-local-copy)
(put 'load 'jka-compr 'jka-compr-load)
(put 'byte-compiler-base-file-name 'jka-compr
     'jka-compr-byte-compiler-base-file-name)

(defvar jka-compr-inhibit nil
  "Non-nil means inhibit automatic uncompression temporarily.
Lisp programs can bind this to t to do that.
It is not recommended to set this variable permanently to anything but nil.")

(defun jka-compr-handler (operation &rest args)
  (save-match-data
    (let ((jka-op (get operation 'jka-compr)))
      (if (and jka-op (not jka-compr-inhibit))
	  (apply jka-op args)
	(jka-compr-run-real-handler operation args)))))

;; If we are given an operation that we don't handle,
;; call the Emacs primitive for that operation,
;; and manipulate the inhibit variables
;; to prevent the primitive from calling our handler again.
(defun jka-compr-run-real-handler (operation args)
  (let ((inhibit-file-name-handlers
	 (cons 'jka-compr-handler
	       (and (eq inhibit-file-name-operation operation)
		    inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
    (apply operation args)))

;;;###autoload
(defcustom auto-compression-mode nil
  "Toggle automatic file compression and uncompression.
Setting this variable directly does not take effect;
use either \\[customize] or the function `auto-compression-mode'."
  :set (lambda (symbol value)
	 (auto-compression-mode (or value 0)))
  :initialize 'custom-initialize-default
  :group 'jka-compr
  :version "21.1"
  :type 'boolean
  :require 'jka-compr)

;;;###autoload(defun auto-compression-mode (&optional arg)
;;;###autoload  "\
;;;###autoloadToggle automatic file compression and uncompression.
;;;###autoloadWith prefix argument ARG, turn auto compression on if positive, else off.
;;;###autoloadReturns the new status of auto compression (non-nil means on)."
;;;###autoload  (interactive "P")
;;;###autoload  (if (not (fboundp 'jka-compr-installed-p))
;;;###autoload      (progn
;;;###autoload        (require 'jka-compr)
;;;###autoload        ;; That turned the mode on, so make it initially off.
;;;###autoload        (toggle-auto-compression)))
;;;###autoload  (toggle-auto-compression arg t))

(defun toggle-auto-compression (&optional arg message)
  "Toggle automatic file compression and uncompression.
With prefix argument ARG, turn auto compression on if positive, else off.
Returns the new status of auto compression (non-nil means on).
If the argument MESSAGE is non-nil, it means to print a message
saying whether the mode is now on or off."
  (interactive "P\np")
  (let* ((installed (jka-compr-installed-p))
	 (flag (if (null arg)
		   (not installed)
		 (or (eq arg t) (listp arg) (and (integerp arg) (> arg 0))))))

    (cond
     ((and flag installed) t)		; already installed

     ((and (not flag) (not installed)) nil) ; already not installed

     (flag
      (jka-compr-install))

     (t
      (jka-compr-uninstall)))


    (and message
	 (if flag
	     (message "Automatic file (de)compression is now ON.")
	   (message "Automatic file (de)compression is now OFF.")))

    flag))

(defun jka-compr-build-file-regexp ()
  (concat
   "\\("
   (mapconcat
    'jka-compr-info-regexp
    jka-compr-compression-info-list
    "\\)\\|\\(")
   "\\)"))


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
	(append auto-mode-alist jka-compr-mode-alist-additions)))


(defun jka-compr-uninstall ()
  "Uninstall jka-compr.
This removes the entries in `file-name-handler-alist' and `auto-mode-alist'
and `inhibit-first-line-modes-suffixes' that were added
by `jka-compr-installed'."
  ;; Delete from inhibit-first-line-modes-suffixes
  ;; what jka-compr-install added.
  (mapcar
     (function (lambda (x)
		 (and (jka-compr-info-strip-extension x)
		      (setq inhibit-first-line-modes-suffixes
			    (delete (jka-compr-info-regexp x)
				    inhibit-first-line-modes-suffixes)))))
     jka-compr-compression-info-list)

  (let* ((fnha (cons nil file-name-handler-alist))
	 (last fnha))

    (while (cdr last)
      (if (eq (cdr (car (cdr last))) 'jka-compr-handler)
	  (setcdr last (cdr (cdr last)))
	(setq last (cdr last))))

    (setq file-name-handler-alist (cdr fnha)))

  (let* ((ama (cons nil auto-mode-alist))
	 (last ama)
	 entry)

    (while (cdr last)
      (setq entry (car (cdr last)))
      (if (or (member entry jka-compr-mode-alist-additions)
	      (and (consp (cdr entry))
		   (eq (nth 2 entry) 'jka-compr)))
	  (setcdr last (cdr (cdr last)))
	(setq last (cdr last))))
    
    (setq auto-mode-alist (cdr ama)))

  (let* ((ama (cons nil file-coding-system-alist))
	 (last ama)
	 entry)

    (while (cdr last)
      (setq entry (car (cdr last)))
      (if (member entry jka-compr-added-to-file-coding-system-alist)
	  (setcdr last (cdr (cdr last)))
	(setq last (cdr last))))
    
    (setq file-coding-system-alist (cdr ama))))

      
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


;;; Add the file I/O hook if it does not already exist.
;;; Make sure that jka-compr-file-name-handler-entry is eq to the
;;; entry for jka-compr in file-name-handler-alist.
(and (jka-compr-installed-p)
     (jka-compr-uninstall))

(jka-compr-install)


(provide 'jka-compr)

;; jka-compr.el ends here.
