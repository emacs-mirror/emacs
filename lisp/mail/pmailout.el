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
(defcustom pmail-output-decode-coding nil
  "*If non-nil, do coding system decoding when outputting message as Babyl."
  :type '(choice (const :tag "on" t)
		 (const :tag "off" nil))
  :group 'pmail)

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
	     (concat "Output message to mail file (default "
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

;;;###autoload
(defcustom pmail-fields-not-to-output nil
  "*Regexp describing fields to exclude when outputting a message to a file."
  :type '(choice (const :tag "None" nil)
		 regexp)
  :group 'pmail-output)

;; Delete from the buffer header fields we don't want output.
;; Buffer should be pre-narrowed to the header.
;; PRESERVE is a regexp for fields NEVER to delete.
(defun pmail-delete-unwanted-fields (preserve)
  (if pmail-fields-not-to-output
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward pmail-fields-not-to-output nil t)
	  (beginning-of-line)
	  (unless (looking-at preserve)
	    (delete-region (point)
			   (progn (forward-line 1) (point))))))))

(defun pmail-output-as-babyl (file-name nomsg)
  "Convert the current buffer's text to Babyl and output to FILE-NAME.
It alters the current buffer's text, so it should be a temp buffer."
  (let ((coding-system-for-write
	 'emacs-mule-unix))
    (save-restriction
      (goto-char (point-min))
      (search-forward "\n\n" nil 'move)
      (narrow-to-region (point-min) (point))
      (if pmail-fields-not-to-output
	  (pmail-delete-unwanted-fields nil)))

    ;; Convert to Babyl format.
    (pmail-convert-to-babyl-format)
    ;; Write it into the file.
    (write-region (point-min) (point-max) file-name t nomsg)))

(defun pmail-convert-to-babyl-format ()
  (let ((count 0) (start (point-min))
	(case-fold-search nil)
	(buffer-undo-list t))
    (goto-char (point-min))
    (save-restriction
      (unless (looking-at "^From ")
	(error "Invalid mbox message"))
      (insert "\^L\n0, unseen,,\n*** EOOH ***\n")
      (pmail-nuke-pinhead-header)
      ;; Decode base64 or quoted printable contents, Rmail style.
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
		  header-end t)))))

	(goto-char (point-max))
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
      ;; Transform anything within the message text
      ;; that might appear to be the end of a Babyl-format message.
      (save-excursion
	(save-restriction
	  (narrow-to-region start (point))
	  (goto-char (point-min))
	  (while (search-forward "\n\^_" nil t) ; single char
	    (replace-match "\n^_"))))		; 2 chars: "^" and "_"
      ;; This is for malformed messages that don't end in newline.
      ;; There shouldn't be any, but some users say occasionally
      ;; there are some.
      (or (bolp) (newline))
      (insert ?\^_)
      (setq last-coding-system-used nil)
      ;; Decode coding system, following specs in the message header,
      ;; and record what coding system was decoded.
      (if pmail-output-decode-coding
	  (let ((mime-charset
		 (if (save-excursion
		       (goto-char start)
		       (search-forward "\n\n" nil t)
		       (let ((case-fold-search t))
			 (re-search-backward
			  pmail-mime-charset-pattern
			  start t)))
		     (intern (downcase (match-string 1))))))
	    (pmail-decode-region start (point) mime-charset)))
      (save-excursion
	(goto-char start)
	(forward-line 3)
	(insert "X-Coding-System: "
		(symbol-name last-coding-system-used)
		"\n")))))

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

(defun pmail-output-as-mbox (file-name nomsg)
  "Convert the current buffer's text to mbox Babyl and output to FILE-NAME.
It alters the current buffer's text, so it should be a temp buffer."
  (let ((case-fold-search t)
	mail-from mime-version content-type)

    ;; Preserve the Mail-From and MIME-Version fields
    ;; even if they have been pruned.
    (search-forward "\n\n" nil 'move)
    (narrow-to-region (point-min) (point))

    (pmail-delete-unwanted-fields
     (if pmail-enable-mime "Mail-From"
       "Mail-From\\|MIME-Version\\|Content-type"))

    (widen)

    ;; Make sure message ends with blank line.
    (goto-char (point-max))
    (unless (bolp)
       (insert "\n"))
    (unless (looking-back "\n\n")
      (insert "\n"))

    ;; Generate a From line from other header fields
    ;; if necessary.
    (goto-char (point-min))
    (unless (looking-at "From ")
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

    (let ((coding-system-for-write
	   'raw-text-unix))
      (write-region (point-min) (point-max) file-name t nomsg))))

;;; There are functions elsewhere in Emacs that use this function;
;;; look at them before you change the calling method.
;;;###autoload
(defun pmail-output (file-name &optional count noattribute from-gnus)
  "Append this message to system-inbox-format mail file named FILE-NAME.
A prefix argument COUNT says to output that many consecutive messages,
starting with the current one.  Deleted messages are skipped and don't count.
When called from Lisp code, COUNT may be omitted and defaults to 1.

This outputs the complete message header even the display is pruned.

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
  (set-buffer pmail-buffer)

  ;; Warn about creating new file.
  (or (find-buffer-visiting file-name)
      (file-exists-p file-name)
      (yes-or-no-p
       (concat "\"" file-name "\" does not exist, create it? "))
      (error "Output file does not exist"))

  (let ((orig-count count)
	(case-fold-search t)
	(tembuf (get-buffer-create " pmail-output"))
	(babyl-format
	 (and (file-readable-p file-name) (mail-file-babyl-p file-name))))

    (unwind-protect
	(while (> count 0)
	  (with-current-buffer pmail-buffer
	    (let (cur beg end)
	      (setq beg (pmail-msgbeg pmail-current-message)
		    end (pmail-msgend pmail-current-message))
	      ;; All access to the buffer's local variables is now finished...
	      (save-excursion
		;; ... so it is ok to go to a different buffer.
		(if (pmail-buffers-swapped-p) (set-buffer pmail-view-buffer))
		(setq cur (current-buffer))
		(save-restriction
		  (widen)
		  (with-current-buffer tembuf
		    (insert-buffer-substring cur beg end)
		    ;; Convert the text to one format or another and output.
		    (if babyl-format
			(pmail-output-as-babyl file-name (if noattribute 'nomsg))
		      (pmail-output-as-mbox file-name 
					    (if noattribute 'nomsg))))))))

	  ;; Mark message as "filed".
	  (unless noattribute
	    (pmail-set-attribute pmail-filed-attr-index t))

	  (setq count (1- count))

	  (or from-gnus
	      (let ((next-message-p
		     (if pmail-delete-after-output
			 (pmail-delete-forward)
		       (if (> count 0)
			   (pmail-next-undeleted-message 1))))
		    (num-appended (- orig-count count)))
		(if (and (> count 0) (not next-message-p))
		    (error "Only %d message%s appended" num-appended
			   (if (= num-appended 1) "" "s"))))))
      (kill-buffer tembuf))))

(defun pmail-output-as-seen (file-name &optional count noattribute from-gnus)
  "Append this message to system-inbox-format mail file named FILE-NAME.
A prefix argument COUNT says to output that many consecutive messages,
starting with the current one.  Deleted messages are skipped and don't count.
When called from Lisp code, COUNT may be omitted and defaults to 1.

This outputs the message header as you see it.

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
  (set-buffer pmail-buffer)

  ;; Warn about creating new file.
  (or (find-buffer-visiting file-name)
      (file-exists-p file-name)
      (yes-or-no-p
       (concat "\"" file-name "\" does not exist, create it? "))
      (error "Output file does not exist"))

    (if (and (file-readable-p file-name) (mail-file-babyl-p file-name))
	(error "Cannot output `as seen' to a Babyl file"))

  (let ((orig-count count)
	(case-fold-search t)
	(tembuf (get-buffer-create " pmail-output")))

    (unwind-protect
	(while (> count 0)
	  (let (cur beg end)
	    ;; If operating from whole-mbox buffer, get message bounds.
	    (if (not (pmail-buffers-swapped-p))
		(setq beg (pmail-msgbeg pmail-current-message)
		      end (pmail-msgend pmail-current-message)))
	    ;; All access to the buffer's local variables is now finished...
	    (save-excursion
	      (setq cur (current-buffer))
	      (save-restriction
		(widen)
		;; If operating from the view buffer, get the bounds.
		(unless beg
		  (setq beg (point-min)
			end (point-max)))

		(with-current-buffer tembuf
		  (insert-buffer-substring cur beg end)
		  ;; Convert the text to one format or another and output.
		  (pmail-output-as-mbox file-name 
					(if noattribute 'nomsg))))))

	  ;; Mark message as "filed".
	  (unless noattribute
	    (pmail-set-attribute pmail-filed-attr-index t))

	  (setq count (1- count))

	  (or from-gnus
	      (let ((next-message-p
		     (if pmail-delete-after-output
			 (pmail-delete-forward)
		       (if (> count 0)
			   (pmail-next-undeleted-message 1))))
		    (num-appended (- orig-count count)))
		(if (and (> count 0) (not next-message-p))
		    (error "Only %d message%s appended" num-appended
			   (if (= num-appended 1) "" "s"))))))
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
    (write-region (point) (point-max) file-name))
  (if pmail-delete-after-output
      (pmail-delete-forward)))

;; Local Variables:
;; change-log-default-name: "ChangeLog.pmail"
;; End:

;; arch-tag: 4059abf0-f249-4be4-8e0d-602d370d01d1
;;; pmailout.el ends here
