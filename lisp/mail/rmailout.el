;;; rmailout.el --- "RMAIL" mail reader for Emacs: output message to a file.

;; Copyright (C) 1985, 1987, 1993, 1994, 2001 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail

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

;;; Code:

(provide 'rmailout)

(eval-when-compile
  (require 'rmail)
  (require 'rmaildesc))


;;;###autoload
(defcustom rmail-output-file-alist nil
  "*Alist matching regexps to suggested output Rmail files.
This is a list of elements of the form (REGEXP . NAME-EXP).
The suggestion is taken if REGEXP matches anywhere in the message buffer.
NAME-EXP may be a string constant giving the file name to use,
or more generally it may be any kind of expression that returns
a file name as a string."
  :type '(repeat (cons regexp
		       (choice :value ""
			       (string :tag "File Name")
			       sexp)))
  :group 'rmail-output)

(defun rmail-output-read-rmail-file-name ()
  "Read the file name to use for `rmail-output-to-rmail-file'.
Set `rmail-default-rmail-file' to this name as well as returning it."
  (let ((default-file
	  (let (answer tail)
	    (setq tail rmail-output-file-alist)
	    ;; Suggest a file based on a pattern match.
	    (while (and tail (not answer))
	      (save-excursion
		(set-buffer rmail-buffer)
		(goto-char (point-min))
		(if (re-search-forward (car (car tail)) nil t)
		    (setq answer (eval (cdr (car tail)))))
		(setq tail (cdr tail))))
	    ;; If no suggestions, use same file as last time.
	    (expand-file-name (or answer rmail-default-rmail-file)))))
    (let ((read-file
	   (expand-file-name
	    (read-file-name
	     (concat "Output message to Rmail (mbox) file: (default "
		     (file-name-nondirectory default-file)
		     ") ")
	     (file-name-directory default-file)
	     (abbreviate-file-name default-file))
	    (file-name-directory default-file))))
      ;; If the user enters just a directory,
      ;; use the name within that directory chosen by the default.
      (setq rmail-default-rmail-file
	    (if (file-directory-p read-file)
		(expand-file-name (file-name-nondirectory default-file)
				  read-file)
	      read-file)))))

;;; mbox: deprecated
(defun rmail-output-read-file-name ()
  "Read the file name to use for `rmail-output'.
Set `rmail-default-file' to this name as well as returning it."
  (let ((default-file
	  (let (answer tail)
	    (setq tail rmail-output-file-alist)
	    ;; Suggest a file based on a pattern match.
	    (while (and tail (not answer))
	      (save-excursion
		(goto-char (point-min))
		(if (re-search-forward (car (car tail)) nil t)
		    (setq answer (eval (cdr (car tail)))))
		(setq tail (cdr tail))))
	    ;; If no suggestion, use same file as last time.
	    (or answer rmail-default-file))))
    (let ((read-file
	   (expand-file-name
	    (read-file-name
	     (concat "Output message to Unix mail file: (default "
		     (file-name-nondirectory default-file)
		     ") ")
	     (file-name-directory default-file)
	     (abbreviate-file-name default-file))
	    (file-name-directory default-file))))
      (setq rmail-default-file
	    (if (file-directory-p read-file)
		(expand-file-name (file-name-nondirectory default-file)
				  read-file)
	      (expand-file-name
	       (or read-file (file-name-nondirectory default-file))
	       (file-name-directory default-file)))))))

;;; mbox: ready
;;; There are functions elsewhere in Emacs that use this function;
;;; look at them before you change the calling method.
;;;###autoload
(defun rmail-output-to-rmail-file (file-name &optional count stay)
  "Append the current message to an Rmail (mbox) file named FILE-NAME.
If the file does not exist, ask if it should be created.
If file is being visited, the message is appended to the Emacs
buffer visiting that file.
If the file exists and is not an Rmail file, the message is
appended in inbox format, the same way `rmail-output' does it.

The default file name comes from `rmail-default-rmail-file',
which is updated to the name you use in this command.

A prefix argument N says to output N consecutive messages
starting with the current one.  Deleted messages are skipped and don't count.

If optional argument STAY is non-nil, then leave the last filed
mesasge up instead of moving forward to the next non-deleted message."
  (interactive
   (list (rmail-output-read-rmail-file-name)
	 (prefix-numeric-value current-prefix-arg)))
  
  ;; Use the 'rmail-output function to perform the output.
  (rmail-output file-name count nil nil)

  ;; Deal with the next message 
  (if rmail-delete-after-output
      (unless 
          (if (and (= count 0) stay)
              (rmail-delete-message)
            (rmail-delete-forward))
        (setq count 0))
    (if (> count 0)
        (unless 
            (if (not stay) (rmail-next-undeleted-message 1))
          (setq count 0)))))

;;; mbox: deprecated
;;;###autoload
(defcustom rmail-fields-not-to-output nil
  "*Regexp describing fields to exclude when outputting a message to a file."
  :type '(choice (const :tag "None" nil)
		 regexp)
  :group 'rmail-output)

;;; mbox: deprecated
;; Delete from the buffer header fields we don't want output.
;; NOT-RMAIL if t means this buffer does not have the full header
;; and *** EOOH *** that a message in an Rmail file has.
(defun rmail-delete-unwanted-fields (&optional not-rmail)
  (if rmail-fields-not-to-output
      (save-excursion
	(goto-char (point-min))
	;; Find the end of the header.
	(if (and (or not-rmail (search-forward "\n*** EOOH ***\n" nil t))
		 (search-forward "\n\n" nil t))
	    (let ((end (point-marker)))
	      (goto-char (point-min))
	      (while (re-search-forward rmail-fields-not-to-output end t)
		(beginning-of-line)
		(delete-region (point)
			       (progn (forward-line 1) (point)))))))))

;;; mbox: ready
;;; There are functions elsewhere in Emacs that use this function;
;;; look at them before you change the calling method.
;;;###autoload
(defun rmail-output (file-name &optional count noattribute ext)
  "Append an mbox formatted message to the mbox formatted file named
FILE-NAME.  A prefix argument COUNT says to output COUNT consecutive
messages starting with the current one.  Deleted messages are skipped
and don't count.  When called from lisp code, COUNT may be omitted.

The default file name comes from `rmail-default-file',
which is updated to the name you use in this command.

The optional third argument NOATTRIBUTE, if non-nil, says not
to set the `filed' attribute, and not to display a message.

The optional fourth argument EXT is set when called from outside of an
Rmail function, for example by GNUS or Sendmail."
  (interactive
   (list (rmail-output-read-file-name)
	 (prefix-numeric-value current-prefix-arg)))
  (or count (setq count 1))
  (setq file-name
	(expand-file-name file-name
			  (and rmail-default-file
			       (file-name-directory rmail-default-file))))

  ;; Use the Rmail buffer, likely narrowed, as the message source
  ;; unless being called from an external party, such as GNUS or
  ;; Sendmail.
  (unless ext
    (set-buffer rmail-buffer))

  (let ((orig-count count)
	(src-buf (current-buffer))
        (dst-buf (find-buffer-visiting file-name))
        (current-message rmail-current-message)
	(tembuf (get-buffer-create " rmail-output"))
	(original-headers-p
	 (and (not ext) (not (rmail-msg-is-pruned)))))

    ;; Output each message to the destination file.
    (while (> count 0)
      (save-excursion

        ;; Copy the message, including all headers, to the temporary
        ;; buffer.
        (set-buffer tembuf)
        (erase-buffer)
        (insert-buffer-substring src-buf)
        
        ;; Deal with MIME --- tbd.
        ;;(when rmail-enable-mime ...

        ;; Determine whether a buffer is already visiting the output
        ;; file.
        (if dst-buf

            ;; The destination file is being visited.  Update it.
            (progn
              (set-buffer dst-buf)

              ;; Determine if the destination file is an Rmail file.
              (let ((buffer-read-only nil)
                    (dst-current-message (and (boundp 'rmail-current-message)
                                              rmail-current-message)))
                (if dst-current-message

                    ;; The buffer is an Rmail buffer.  Append the message.
                    (progn
                      (widen)
                      (narrow-to-region (point-max) (point-max))
                      (insert-buffer-substring src-buf)
                      (insert "\n")
                      (rmail-process-new-messages)
                      (rmail-show-message dst-current-message))

                  ;; The destination file is not an Rmail file, just
                  ;; insert at the end.
                  (goto-char (point-max))
                  (insert-buffer-substring src-buf))))

          ;; The destination file is not being visited, just write out
          ;; the processed message.
          (write-region (point-min) (point-max) file-name t
                        (if noattribute 'nomsg))))

      ;; Do housekeeping, such as setting the "Filed" attribute, if
      ;; necessary and moving to the next message.
      (or noattribute
          (if (equal major-mode 'rmail-mode)
              (progn
                (rmail-set-attribute "filed" t current-message)
                (setq current-message (1+ current-message)))))

      ;; Determine if Rmail post output operations need to be handled.
      (or ext

          ;; They do.  Move to the next non-deleted message.
          (let ((next-message-p
                 (if rmail-delete-after-output
                     (rmail-delete-forward)
                   (if (> count 1)
                       (rmail-next-undeleted-message 1))))
                (num-appended (- orig-count count)))
            (if (and (> count 1) (not next-message-p))
                (progn 
                  (error
                   (save-excursion
                     (set-buffer src-buf)
                     (format "Only %d message%s appended" num-appended
                             (if (= num-appended 1) "" "s"))))
                  (setq count 0)))))

      ;; Decrement the count for the next iteration.  If an error has
      ;; occurred, then count will be -1, which is every bit as good
      ;; as 0.
      (setq count (1- count)))
    (kill-buffer tembuf)))

;;; mbox: ready
;;;###autoload
(defun rmail-output-body-to-file (file-name)
  "Write this message body to the file FILE-NAME.
FILE-NAME defaults, interactively, from the Subject field of the message."
  (interactive
   (let ((default-file
	   (or (mail-fetch-field "Subject")
	       rmail-default-body-file)))
     (list (setq rmail-default-body-file
		 (read-file-name
		  "Output message body to file: "
		  (and default-file (file-name-directory default-file))
		  default-file
		  nil default-file)))))
  (setq file-name
	(expand-file-name file-name
			  (and rmail-default-body-file
			       (file-name-directory rmail-default-body-file))))
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n\n")
    (and (file-exists-p file-name)
	 (not (y-or-n-p (message "File %s exists; overwrite? " file-name)))
	 (error "Operation aborted"))
    (write-region (point) (point-max) file-name)
    (if (equal major-mode 'rmail-mode)
	(rmail-desc-set-attribute rmail-desc-stored-index t rmail-current-message)))
  (if rmail-delete-after-output
      (rmail-delete-forward)))

;;; rmailout.el ends here
