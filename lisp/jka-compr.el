;;; jka-compr.el - reading/writing/loading compressed files.
;;; Copyright (C) 1993, 1994  Free Software Foundation, Inc.

;; Author: jka@ece.cmu.edu (Jay K. Adams)
;; Keywords: data

;;; Commentary: 

;;; This package implements low-level support for reading, writing,
;;; and loading compressed files.  It hooks into the low-level file
;;; I/O functions (including write-region and insert-file-contents) so
;;; that they automatically compress or uncompress a file if the file
;;; appears to need it (based on the extension of the file name).
;;; Packages like Rmail, VM, GNUS, and Info should be able to work
;;; with compressed files without modification.


;;; INSTRUCTIONS:
;;;
;;; To use jka-compr, simply load this package, and edit as usual.
;;; Its operation should be transparent to the user (except for
;;; messages appearing when a file is being compressed or
;;; uncompressed).
;;;
;;; The variable, jka-compr-compression-info-list can be used to
;;; customize jka-compr to work with other compression programs.
;;; The default value of this variable allows jka-compr to work with
;;; Unix compress and gzip.
;;;
;;; If you are concerned about the stderr output of gzip and other
;;; compression/decompression programs showing up in your buffers, you
;;; should set the discard-error flag in the compression-info-list.
;;; This will cause the stderr of all programs to be discarded.
;;; However, it also causes emacs to call compression/uncompression
;;; programs through a shell (which is specified by jka-compr-shell).
;;; This may be a drag if, on your system, starting up a shell is
;;; slow.
;;;
;;; If you don't want messages about compressing and decompressing
;;; to show up in the echo area, you can set the compress-name and
;;; decompress-name fields of the jka-compr-compression-info-list to
;;; nil.


;;; APPLICATION NOTES:
;;;
;;; crypt++
;;;   jka-compr can coexist with crpyt++ if you take all the decompression
;;;   entries out of the crypt-encoding-list.  Clearly problems will arise if
;;;   you have two programs trying to compress/decompress files.  jka-compr
;;;   will not "work with" crypt++ in the following sense: you won't be able to
;;;   decode encrypted compressed files--that is, files that have been
;;;   compressed then encrypted (in that order).  Theoretically, crypt++ and
;;;   jka-compr could properly handle a file that has been encrypted then
;;;   compressed, but there is little point in trying to compress an encrypted
;;;   file.
;;;


;;; ACKNOWLEDGMENTS
;;; 
;;; jka-compr is a V19 adaptation of jka-compr for V18 of Emacs.  Many people
;;; have made helpful suggestions, reported bugs, and even fixed bugs in 
;;; jka-compr.  I recall the following people as being particularly helpful.
;;;
;;;   Jean-loup Gailly
;;;   David Hughes
;;;   Richard Pieri
;;;   Daniel Quinlan
;;;   Chris P. Ross
;;;   Rick Sladkey
;;;
;;; Andy Norman's ange-ftp was the inspiration for the original jka-compr for
;;; Version 18 of Emacs.
;;;
;;; After I had made progress on the original jka-compr for V18, I learned of a
;;; package written by Kazushi Jam Marukawa, called jam-zcat, that did exactly
;;; what I was trying to do.  I looked over the jam-zcat source code and
;;; probably got some ideas from it.
;;;

;;; Code:

(defvar jka-compr-shell "sh"
  "*Shell to be used for calling compression programs.
The value of this variable only matters if you want to discard the
stderr of a compression/decompression program (see the documentation
for `jka-compr-compression-info-list').")


(defvar jka-compr-use-shell t)


;;; I have this defined so that .Z files are assumed to be in unix
;;; compress format; and .gz files, in gzip format.
(defvar jka-compr-compression-info-list
  ;;[regexp
  ;; compr-message  compr-prog  compr-args
  ;; uncomp-message uncomp-prog uncomp-args
  ;; can-append auto-mode-flag]
  '(["\\.Z\\(~\\|\\.~[0-9]+~\\)?\\'"
     "compressing"    "compress"     ("-c")
     "uncompressing"  "uncompress"   ("-c")
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
invoked.")

(defvar jka-compr-mode-alist-additions
  (list (cons "\\.tgz\\'" 'tar-mode))
  "A list of pairs to add to auto-mode-alist when jka-compr is installed.")

(defvar jka-compr-file-name-handler-entry
  nil
  "The entry in `file-name-handler-alist' used by the jka-compr I/O functions.")

;;; Functions for accessing the return value of jka-get-compression-info
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


(defvar jka-compr-acceptable-retval-list '(0 141))


(defun jka-compr-error (prog args infile message &optional errfile)

  (let ((errbuf (get-buffer-create " *jka-compr-error*"))
	(curbuf (current-buffer)))
    (set-buffer errbuf)
    (widen) (erase-buffer)
    (insert (format "Error while executing \"%s %s < %s\"\n\n"
		     prog
		     (mapconcat 'identity args " ")
		     infile))

     (and errfile
	  (insert-file-contents errfile))

     (set-buffer curbuf)
     (display-buffer errbuf))

  (signal 'compression-error (list "Opening input file" (format "error %s" message) infile)))
			
   
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

      (let ((err-file (jka-compr-make-temp-name)))
	    
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
	 (let ((cbuf (current-buffer)))
	   (set-buffer temp)
	   (write-region (point-min) (point-max) output)
	   (erase-buffer)
	   (set-buffer cbuf)))))


;;; Support for temp files.  Much of this was inspired if not lifted
;;; from ange-ftp.

(defvar jka-compr-temp-name-template
  "/tmp/jka-com"
  "Prefix added to all temp files created by jka-compr.
There should be no more than seven characters after the final `/'")

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
		(temp-file (jka-compr-make-temp-name))
		(base-name (file-name-nondirectory visit-file))
		cbuf temp-buffer)

	    (setq cbuf (current-buffer)
		  temp-buffer (get-buffer-create " *jka-compr-temp*"))
	    (set-buffer temp-buffer)
	    (widen) (erase-buffer)
	    (set-buffer cbuf)

	    (and append
		 (not can-append)
		 (file-exists-p filename)
		 (let* ((local-copy (file-local-copy filename))
			(local-file (or local-copy filename)))

		   (unwind-protect

		       (progn
		      
			 (and
			  uncompress-message
			  (message "%s %s..." uncompress-message base-name))

			 (jka-compr-call-process uncompress-program
						 (concat uncompress-message
							 " " base-name)
						 local-file
						 temp-file
						 temp-buffer
						 uncompress-args)
			 (and
			  uncompress-message
			  (message "%s %s...done" uncompress-message base-name)))
		     
		     (and
		      local-copy
		      (file-exists-p local-copy)
		      (delete-file local-copy)))))

	    (and 
	     compress-message
	     (message "%s %s..." compress-message base-name))

	    (jka-compr-run-real-handler 'write-region
					(list start end temp-file t 'dont))

	    (jka-compr-call-process compress-program
				    (concat compress-message
					    " " base-name)
				    temp-file
				    temp-buffer
				    nil
				    compress-args)

	    (set-buffer temp-buffer)
	    (jka-compr-run-real-handler 'write-region
					(list (point-min) (point-max)
					      filename
					      (and append can-append) 'dont))
	    (erase-buffer)
	    (set-buffer cbuf)

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
	      size start)

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
			(jka-compr-call-process uncompress-program
						(concat uncompress-message
							" " base-name)
						local-file
						t
						nil
						uncompress-args))
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

	  ;; Run the functions that insert-file-contents would.
	  (let ((p after-insert-file-functions)
		(insval size))
	    (while p
	      (setq insval (funcall (car p) size))
	      (if insval
		  (progn
		    (or (integerp insval)
			(signal 'wrong-type-argument
				(list 'integerp insval)))
		    (setq size insval)))
	      (setq p (cdr p))))

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
	      (temp-buffer (get-buffer-create " *jka-compr-temp*"))
	      (notfound nil)
	      (cbuf (current-buffer))
	      local-file)

	  (setq local-file (or local-copy filename))

	  (unwind-protect

	      (progn
		  
		(and
		 uncompress-message
		 (message "%s %s..." uncompress-message base-name))

		(set-buffer temp-buffer)
		  
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
		 (point-min) (point-max) temp-file nil 'dont))

	    (and
	     local-copy
	     (file-exists-p local-copy)
	     (delete-file local-copy))

	    (set-buffer cbuf)
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

	  (load load-file noerror t t)

	  (or nomessage
	      (message "Loading %s...done." file)))

      (jka-compr-delete-temp-file local-copy))

    t))

(put 'write-region 'jka-compr 'jka-compr-write-region)
(put 'insert-file-contents 'jka-compr 'jka-compr-insert-file-contents)
(put 'file-local-copy 'jka-compr 'jka-compr-file-local-copy)
(put 'load 'jka-compr 'jka-compr-load)

(defun jka-compr-handler (operation &rest args)
  (save-match-data
    (let ((jka-op (get operation 'jka-compr)))
      (if jka-op
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

(defun toggle-auto-compression (arg)
  "Toggle automatic file compression and decompression.
With prefix argument ARG, turn auto compression on if positive, else off.
Returns the new status of auto compression (non-nil means on)."
  (interactive "P")
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


    (and (interactive-p)
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
This adds entries to `file-name-handler-alist' and `auto-mode-alist'."

  (setq jka-compr-file-name-handler-entry
	(cons (jka-compr-build-file-regexp) 'jka-compr-handler))

  (setq file-name-handler-alist (cons jka-compr-file-name-handler-entry
				      file-name-handler-alist))

  (mapcar
   (function (lambda (x)
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
that were created by `jka-compr-installed'."

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
    
    (setq auto-mode-alist (cdr ama))))

      
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
