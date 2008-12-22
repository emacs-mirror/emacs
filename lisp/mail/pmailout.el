;;; pmailout.el --- "PMAIL" mail reader for Emacs: output message to a file

;; Copyright (C) 1985, 1987, 1993, 1994, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'pmail)
(provide 'pmailout)

;;;###autoload
(defcustom pmail-output-file-alist nil
  "*Alist matching regexps to suggested output Pmail files.
This is a list of elements of the form (REGEXP . NAME-EXP).
The suggestion is taken if REGEXP matches anywhere in the message buffer.
NAME-EXP may be a string constant giving the file name to use,
or more generally it may be any kind of expression that returns
a file name as a string."
  :type '(repeat (cons regexp
		       (choice :value ""
			       (string :tag "File Name")
			       sexp)))
  :group 'pmail-output)

(defun pmail-output-read-pmail-file-name ()
  "Read the file name to use for `pmail-output-to-babyl-file'.
Set `pmail-default-pmail-file' to this name as well as returning it."
  (let ((default-file
	  (let (answer tail)
	    (setq tail pmail-output-file-alist)
	    ;; Suggest a file based on a pattern match.
	    (while (and tail (not answer))
	      (save-excursion
		(set-buffer pmail-buffer)
		(goto-char (point-min))
		(if (re-search-forward (car (car tail)) nil t)
		    (setq answer (eval (cdr (car tail)))))
		(setq tail (cdr tail))))
	    ;; If no suggestions, use same file as last time.
	    (expand-file-name (or answer pmail-default-pmail-file)))))
    (let ((read-file
	   (expand-file-name
	    (read-file-name
	     (concat "Output message to Pmail file (default "
		     (file-name-nondirectory default-file)
		     "): ")
	     (file-name-directory default-file)
	     (abbreviate-file-name default-file))
	    (file-name-directory default-file))))
      ;; If the user enters just a directory,
      ;; use the name within that directory chosen by the default.
      (setq pmail-default-pmail-file
	    (if (file-directory-p read-file)
		(expand-file-name (file-name-nondirectory default-file)
				  read-file)
	      read-file)))))

(defun pmail-output-read-file-name ()
  "Read the file name to use for `pmail-output'.
Set `pmail-default-file' to this name as well as returning it."
  (let ((default-file
	  (let (answer tail)
	    (setq tail pmail-output-file-alist)
	    ;; Suggest a file based on a pattern match.
	    (while (and tail (not answer))
	      (save-excursion
		(goto-char (point-min))
		(if (re-search-forward (car (car tail)) nil t)
		    (setq answer (eval (cdr (car tail)))))
		(setq tail (cdr tail))))
	    ;; If no suggestion, use same file as last time.
	    (or answer pmail-default-file))))
    (let ((read-file
	   (expand-file-name
	    (read-file-name
	     (concat "Output message to Unix mail file (default "
		     (file-name-nondirectory default-file)
		     "): ")
	     (file-name-directory default-file)
	     (abbreviate-file-name default-file))
	    (file-name-directory default-file))))
      (setq pmail-default-file
	    (if (file-directory-p read-file)
		(expand-file-name (file-name-nondirectory default-file)
				  read-file)
	      (expand-file-name
	       (or read-file (file-name-nondirectory default-file))
	       (file-name-directory default-file)))))))

(declare-function pmail-update-summary "pmailsum" (&rest ignore))

;;; There are functions elsewhere in Emacs that use this function;
;;; look at them before you change the calling method.
;;;###autoload
(defun pmail-output-to-babyl-file (file-name &optional count stay)
  "Append the current message to a Babyl file named FILE-NAME.
If the file does not exist, ask if it should be created.
If file is being visited, the message is appended to the Emacs
buffer visiting that file.
If the file exists and is not a Babyl file, the message is
appended in inbox format, the same way `pmail-output' does it.

The default file name comes from `pmail-default-pmail-file',
which is updated to the name you use in this command.

A prefix argument COUNT says to output that many consecutive messages,
starting with the current one.  Deleted messages are skipped and don't count.

If the optional argument STAY is non-nil, then leave the last filed
message up instead of moving forward to the next non-deleted message."
  (interactive
   (list (pmail-output-read-pmail-file-name)
	 (prefix-numeric-value current-prefix-arg)))
  (or count (setq count 1))
  (setq file-name
	(expand-file-name file-name
			  (file-name-directory pmail-default-pmail-file)))
  (if (and (file-readable-p file-name) (not (mail-file-babyl-p file-name)))
      (pmail-output file-name count)
    (pmail-maybe-set-message-counters)
    (setq file-name (abbreviate-file-name file-name))
    (or (find-buffer-visiting file-name)
	(file-exists-p file-name)
	(if (yes-or-no-p
	     (concat "\"" file-name "\" does not exist, create it? "))
	    (let ((file-buffer (create-file-buffer file-name)))
	      (save-excursion
		(set-buffer file-buffer)
		(let ((buffer-read-only nil))
		  (insert "BABYL OPTIONS: -*- pmail -*-
Version: 5
Labels:
Note:   This is the header of an pmail file.
Note:   If you are seeing it in pmail,
Note:    it means the file has no messages in it.\n\^_"))
		(let ((require-final-newline nil)
		      (coding-system-for-write
		       (or pmail-file-coding-system
			   'emacs-mule-unix)))
		  (write-region (point-min) (point-max) file-name t 1)))
	      (kill-buffer file-buffer))
	  (error "Output file does not exist")))
    (while (> count 0)
      (let (redelete)
	(unwind-protect
	    (progn
	      (set-buffer pmail-buffer)
	      ;; Temporarily turn off Deleted attribute.
	      ;; Do this outside the save-restriction, since it would
	      ;; shift the place in the buffer where the visible text starts.
	      (if (pmail-message-deleted-p pmail-current-message)
		  (progn (setq redelete t)
			 (pmail-set-attribute pmail-deleted-attr-index nil)))
	      (let ((coding-system-for-write
		     (or pmail-file-coding-system
			 'emacs-mule-unix))
		    cur beg end)
		(pmail-swap-buffers-maybe)
		(setq cur (current-buffer))
		(save-restriction
		  (save-excursion
		    (widen)
		    (setq beg (pmail-msgbeg pmail-current-message)
			  end (pmail-msgend pmail-current-message))
		    ;; Output to a file.
		    (set-buffer (get-buffer-create " pmail-out-temp"))
		    (insert-buffer-substring cur beg end)
		    (if pmail-fields-not-to-output
			(pmail-delete-unwanted-fields))
		    ;; Convert to Babyl format.
		    (pmail-convert-to-babyl-format)
		    (append-to-file (point-min) (point-max) file-name)
		    (set-buffer cur)
		    (kill-buffer (get-buffer " pmail-out-temp")))))
	      (pmail-set-attribute pmail-filed-attr-index t))
	  (if redelete (pmail-set-attribute pmail-deleted-attr-index t))))
      (setq count (1- count))
      (if pmail-delete-after-output
	  (unless (if (and (= count 0) stay)
		      (pmail-delete-message)
		    (pmail-delete-forward))
	    (setq count 0))
	(if (> count 0)
	    (unless (if (not stay)
			(pmail-next-undeleted-message 1))
	      (setq count 0))))))
  (pmail-show-message))

(defalias 'pmail-output-to-pmail-file 'pmail-output-to-babyl-file)

(defun pmail-convert-to-babyl-format ()
  (let ((count 0) start
	(case-fold-search nil)
	(buffer-undo-list t))
    (goto-char (point-min))
    (save-restriction
      (while (not (eobp))
	(setq start (point))
	(unless (looking-at "^From ")
	  (error "Invalid mbox message"))
	(insert "\^L\n0, unseen,,\n*** EOOH ***\n")
	(pmail-nuke-pinhead-header)
	;; If this message has a Content-Length field,
	;; skip to the end of the contents.
	(let* ((header-end (save-excursion
			     (and (re-search-forward "\n\n" nil t)
				  (1- (point)))))
	       (case-fold-search t)
	       (quoted-printable-header-field-end
		(save-excursion
		  (re-search-forward
		   "^content-transfer-encoding:\\(\n?[\t ]\\)*quoted-printable\\(\n?[\t ]\\)*"
		   header-end t)))
	       (base64-header-field-end
		(and
		 ;; Don't decode non-text data.
		 (save-excursion
		   (re-search-forward
		    "^content-type:\\(\n?[\t ]\\)\\(text\\|message\\)/"
		    header-end t))
		 (save-excursion
		   (re-search-forward
		    "^content-transfer-encoding:\\(\n?[\t ]\\)*base64\\(\n?[\t ]\\)*"
		    header-end t))))
	       (size
		;; Get the numeric value from the Content-Length field.
		(save-excursion
		  ;; Back up to end of prev line,
		  ;; in case the Content-Length field comes first.
		  (forward-char -1)
		  (and (search-forward "\ncontent-length: "
				       header-end t)
		       (let ((beg (point))
			     (eol (progn (end-of-line) (point))))
				(string-to-number (buffer-substring beg eol)))))))
	  (and size
	       (if (and (natnump size)
			(<= (+ header-end size) (point-max))
			;; Make sure this would put us at a position
			;; that we could continue from.
			(save-excursion
			  (goto-char (+ header-end size))
			  (skip-chars-forward "\n")
			  (or (eobp)
			      (and (looking-at "BABYL OPTIONS:")
				   (search-forward "\n\^_" nil t))
			      (and (looking-at "\^L")
				   (search-forward "\n\^_" nil t))
			      (let ((case-fold-search t))
				(looking-at pmail-mmdf-delim1))
			      (looking-at "From "))))
		   (goto-char (+ header-end size))
		 (message "Ignoring invalid Content-Length field")
		 (sit-for 1 0 t)))
	  (if (let ((case-fold-search nil))
		(re-search-forward
			(concat "^[\^_]?\\("
				pmail-unix-mail-delimiter
				"\\|"
				pmail-mmdf-delim1 "\\|"
				"^BABYL OPTIONS:\\|"
				"\^L\n[01],\\)") nil t))
	      (goto-char (match-beginning 1))
	    (goto-char (point-max)))
	  (setq count (1+ count))
	  (if quoted-printable-header-field-end
	      (save-excursion
		(unless (mail-unquote-printable-region
			 header-end (point) nil t t)
		  (message "Malformed MIME quoted-printable message"))
		;; Change "quoted-printable" to "8bit",
		;; to reflect the decoding we just did.
		(goto-char quoted-printable-header-field-end)
		(delete-region (point) (search-backward ":"))
		(insert ": 8bit")))
	  (if base64-header-field-end
	      (save-excursion
		(when (condition-case nil
			  (progn
			    (base64-decode-region
			     (1+ header-end)
			     (save-excursion
			       ;; Prevent base64-decode-region
			       ;; from removing newline characters.
			       (skip-chars-backward "\n\t ")
			       (point)))
			    t)
			(error nil))
		  ;; Change "base64" to "8bit", to reflect the
		  ;; decoding we just did.
		  (goto-char base64-header-field-end)
		  (delete-region (point) (search-backward ":"))
		  (insert ": 8bit")))))
	(save-excursion
	  (save-restriction
	    (narrow-to-region start (point))
	    (goto-char (point-min))
	    (while (search-forward "\n\^_" nil t) ; single char
	      (replace-match "\n^_"))))	; 2 chars: "^" and "_"
	;; This is for malformed messages that don't end in newline.
	;; There shouldn't be any, but some users say occasionally
	;; there are some.
	(or (bolp) (newline))
	(insert ?\^_)
	(setq last-coding-system-used nil)
	(or pmail-enable-mime
	    (not pmail-enable-multibyte)
	    (let ((mime-charset
		   (if (and pmail-decode-mime-charset
			    (save-excursion
			      (goto-char start)
			      (search-forward "\n\n" nil t)
			      (let ((case-fold-search t))
				(re-search-backward
				 pmail-mime-charset-pattern
				 start t))))
		       (intern (downcase (match-string 1))))))
	      (pmail-decode-region start (point) mime-charset)))
	(save-excursion
	  (goto-char start)
	  (forward-line 3)
	  (insert "X-Coding-System: "
		  (symbol-name last-coding-system-used)
		  "\n"))
	(narrow-to-region (point) (point-max))
	(and (= 0 (% count 10))
	     (message "Converting to Babyl format...%d" count))))))

;; Delete the "From ..." line, creating various other headers with
;; information from it if they don't already exist.  Now puts the
;; original line into a mail-from: header line for debugging and for
;; use by the pmail-output function.
(defun pmail-nuke-pinhead-header ()
  (save-excursion
    (save-restriction
      (let ((start (point))
  	    (end (progn
		   (condition-case ()
		       (search-forward "\n\n")
		     (error
		      (goto-char (point-max))
		      (insert "\n\n")))
		   (point)))
	    has-from has-date)
	(narrow-to-region start end)
	(let ((case-fold-search t))
	  (goto-char start)
	  (setq has-from (search-forward "\nFrom:" nil t))
	  (goto-char start)
	  (setq has-date (and (search-forward "\nDate:" nil t) (point)))
	  (goto-char start))
	(let ((case-fold-search nil))
	  (if (re-search-forward (concat "^" pmail-unix-mail-delimiter) nil t)
	      (replace-match
		(concat
		  "Mail-from: \\&"
		  ;; Keep and reformat the date if we don't
		  ;;  have a Date: field.
		  (if has-date
		      ""
		    (concat
		     "Date: \\2, \\4 \\3 \\9 \\5 "

		     ;; The timezone could be matched by group 7 or group 10.
		     ;; If neither of them matched, assume EST, since only
		     ;; Easterners would be so sloppy.
		     ;; It's a shame the substitution can't use "\\10".
		     (cond
		      ((/= (match-beginning 7) (match-end 7)) "\\7")
		      ((/= (match-beginning 10) (match-end 10))
		       (buffer-substring (match-beginning 10)
					 (match-end 10)))
		      (t "EST"))
		     "\n"))
		  ;; Keep and reformat the sender if we don't
		  ;; have a From: field.
		  (if has-from
		      ""
		    "From: \\1\n"))
		t)))))))

;;;###autoload
(defcustom pmail-fields-not-to-output nil
  "*Regexp describing fields to exclude when outputting a message to a file."
  :type '(choice (const :tag "None" nil)
		 regexp)
  :group 'pmail-output)

;; Delete from the buffer header fields we don't want output.
;; NOT-PMAIL if t means this buffer does not have the full header
;; and *** EOOH *** that a message in an Pmail file has.
(defun pmail-delete-unwanted-fields (&optional not-pmail)
  (if pmail-fields-not-to-output
      (save-excursion
	(goto-char (point-min))
	;; Find the end of the header.
	(if (and (or not-pmail (search-forward "\n*** EOOH ***\n" nil t))
		 (search-forward "\n\n" nil t))
	    (let ((end (point-marker)))
	      (goto-char (point-min))
	      (while (re-search-forward pmail-fields-not-to-output end t)
		(beginning-of-line)
		(delete-region (point)
			       (progn (forward-line 1) (point)))))))))

;;; There are functions elsewhere in Emacs that use this function;
;;; look at them before you change the calling method.
;;;###autoload
(defun pmail-output (file-name &optional count noattribute from-gnus)
  "Append this message to system-inbox-format mail file named FILE-NAME.
A prefix argument COUNT says to output that many consecutive messages,
starting with the current one.  Deleted messages are skipped and don't count.
When called from lisp code, COUNT may be omitted and defaults to 1.

If the pruned message header is shown on the current message, then
messages will be appended with pruned headers; otherwise, messages
will be appended with their original headers.

The default file name comes from `pmail-default-file',
which is updated to the name you use in this command.

The optional third argument NOATTRIBUTE, if non-nil, says not
to set the `filed' attribute, and not to display a message.

The optional fourth argument FROM-GNUS is set when called from GNUS."
  (interactive
   (list (pmail-output-read-file-name)
	 (prefix-numeric-value current-prefix-arg)))
  (or count (setq count 1))
  (setq file-name
	(expand-file-name file-name
			  (and pmail-default-file
			       (file-name-directory pmail-default-file))))
  (if (and (file-readable-p file-name) (mail-file-babyl-p file-name))
      (pmail-output-to-babyl-file file-name count)
    (set-buffer pmail-buffer)
    (let ((orig-count count)
	  (pmailbuf pmail-buffer)
	  (case-fold-search t)
	  (tembuf (get-buffer-create " pmail-output"))
	  header-beginning
	  mail-from mime-version content-type)
      (while (> count 0)
	;; Preserve the Mail-From and MIME-Version fields
	;; even if they have been pruned.
	(or from-gnus
	    (save-excursion
	      (save-restriction
		(goto-char (if (pmail-buffers-swapped-p)
			       (point-min)
			     (pmail-msgbeg pmail-current-message)))
		(setq header-beginning (point))
		(search-forward "\n\n" nil 'move)
		(narrow-to-region header-beginning (point))
		(setq mail-from (mail-fetch-field "Mail-From"))
		(unless pmail-enable-mime
		  (setq mime-version (mail-fetch-field "MIME-Version")
			content-type (mail-fetch-field "Content-type"))))))
	(save-excursion
	  (set-buffer tembuf)
	  (erase-buffer)
	  (insert-buffer-substring pmailbuf)
	  (save-excursion
	    (goto-char (min (point-min) (- (point-max) 2)))
	    (unless (looking-at "\n\n")
	      (goto-char (point-max))
	      (insert "\n\n")))
	  (when pmail-enable-mime
	    (goto-char (point-min))
	    (forward-line 2)
	    (delete-region (point-min) (point))
	    (search-forward "\n\n")
	    (delete-region (match-beginning 0)
			   (if (search-forward "\n\n")
			       (1- (match-end 0))))
	    (setq buffer-file-coding-system (or pmail-file-coding-system
						'raw-text)))
	  (pmail-delete-unwanted-fields t)
	  (or (bolp) (insert "\n"))
	  (goto-char (point-min))
	  (if mail-from
	      (insert mail-from "\n")
	    (insert "From "
		    (mail-strip-quoted-names
		     (save-excursion
		       (save-restriction
			 (goto-char (point-min))
			 (narrow-to-region
			  (point)
			  (or (search-forward "\n\n" nil)
			      (point-max)))
			 (or (mail-fetch-field "from")
			     (mail-fetch-field "really-from")
			     (mail-fetch-field "sender")
			     "unknown"))))
		    " " (current-time-string) "\n"))
	  (when mime-version
	    (insert "MIME-Version: " mime-version)
	    ;; Some malformed MIME messages set content-type to nil.
	    (when content-type
	      (insert "\nContent-type: " content-type "\n")))
	  ;; ``Quote'' "\nFrom " as "\n>From "
	  ;;  (note that this isn't really quoting, as there is no requirement
	  ;;   that "\n[>]+From " be quoted in the same transparent way.)
	  (let ((case-fold-search nil))
	    (while (search-forward "\nFrom " nil t)
	      (forward-char -5)
	      (insert ?>)))
	  (write-region (point-min) (point-max) file-name t
			(if noattribute 'nomsg)))
	(or noattribute
	    (if (equal major-mode 'pmail-mode)
		(pmail-set-attribute pmail-filed-attr-index t)))
	(setq count (1- count))
	(or from-gnus
	    (let ((next-message-p
		   (if pmail-delete-after-output
		       (pmail-delete-forward)
		     (if (> count 0)
			 (pmail-next-undeleted-message 1))))
		  (num-appended (- orig-count count)))
	      (if (and (> count 0) (not next-message-p))
		  (progn
		    (error "%s"
		     (save-excursion
		       (set-buffer pmailbuf)
		       (format "Only %d message%s appended" num-appended
			       (if (= num-appended 1) "" "s"))))
		    (setq count 0))))))
      (kill-buffer tembuf))))

;;;###autoload
(defun pmail-output-body-to-file (file-name)
  "Write this message body to the file FILE-NAME.
FILE-NAME defaults, interactively, from the Subject field of the message."
  (interactive
   (let ((default-file
	   (or (mail-fetch-field "Subject")
	       pmail-default-body-file)))
     (list (setq pmail-default-body-file
		 (read-file-name
		  "Output message body to file: "
		  (and default-file (file-name-directory default-file))
		  default-file
		  nil default-file)))))
  (setq file-name
	(expand-file-name file-name
			  (and pmail-default-body-file
			       (file-name-directory pmail-default-body-file))))
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n\n")
    (and (file-exists-p file-name)
	 (not (y-or-n-p (format "File %s exists; overwrite? " file-name)))
	 (error "Operation aborted"))
    (write-region (point) (point-max) file-name)
    (if (equal major-mode 'pmail-mode)
	(pmail-set-attribute pmail-stored-attr-index t)))
  (if pmail-delete-after-output
      (pmail-delete-forward)))

;; Local Variables:
;; change-log-default-name: "ChangeLog.pmail"
;; End:

;; arch-tag: 4059abf0-f249-4be4-8e0d-602d370d01d1
;;; pmailout.el ends here
