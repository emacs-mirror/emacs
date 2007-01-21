;;; unrmail.el --- convert Rmail files to mailbox files

;; Copyright (C) 1992, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(defvar command-line-args-left)	;Avoid 'free variable' warning

;;;###autoload
(defun batch-unrmail ()
  "Convert Rmail files to system inbox format.
Specify the input Rmail file names as command line arguments.
For each Rmail file, the corresponding output file name
is made by adding `.mail' at the end.
For example, invoke `emacs -batch -f batch-unrmail RMAIL'."
  ;; command-line-args-left is what is left of the command line (from startup.el)
  (if (not noninteractive)
      (error "`batch-unrmail' is to be used only with -batch"))
  (let ((error nil))
    (while command-line-args-left
      (or (unrmail (car command-line-args-left)
		   (concat (car command-line-args-left) ".mail"))
	  (setq error t))
      (setq command-line-args-left (cdr command-line-args-left)))
    (message "Done")
    (kill-emacs (if error 1 0))))

;;;###autoload
(defun unrmail (file to-file)
  "Convert Rmail file FILE to system inbox format file TO-FILE."
  (interactive "fUnrmail (rmail file): \nFUnrmail into (new mailbox file): ")
  (with-temp-buffer
    ;; Read in the old Rmail file with no decoding.
    (let ((coding-system-for-read 'raw-text))
      (insert-file-contents file))
    ;; But make it multibyte.
    (set-buffer-multibyte t)

    (if (not (looking-at "BABYL OPTIONS"))
	(error "This file is not in Babyl format"))

    ;; Decode the file contents just as Rmail did.
    (let ((modifiedp (buffer-modified-p))
	  (coding-system rmail-file-coding-system)
	  from to)
      (goto-char (point-min))
      (search-forward "\n\^_" nil t)	; Skip BABYL header.
      (setq from (point))
      (goto-char (point-max))
      (search-backward "\n\^_" from 'mv)
      (setq to (point))
      (unless (and coding-system
		   (coding-system-p coding-system))
	(setq coding-system
	      ;; Emacs 21.1 and later writes RMAIL files in emacs-mule, but
	      ;; earlier versions did that with the current buffer's encoding.
	      ;; So we want to favor detection of emacs-mule (whose normal
	      ;; priority is quite low), but still allow detection of other
	      ;; encodings if emacs-mule won't fit.  The call to
	      ;; detect-coding-with-priority below achieves that.
	      (car (detect-coding-with-priority
		    from to
		    '((coding-category-emacs-mule . emacs-mule))))))
      (unless (memq coding-system
		    '(undecided undecided-unix))
	(set-buffer-modified-p t)	; avoid locking when decoding
	(let ((buffer-undo-list t))
	  (decode-coding-region from to coding-system))
	(setq coding-system last-coding-system-used))

      (setq buffer-file-coding-system nil)

      ;; We currently don't use this value, but maybe we should.
      (setq save-buffer-coding-system
	    (or coding-system 'undecided)))

    ;; Default the directory of TO-FILE based on where FILE is.
    (setq to-file (expand-file-name to-file default-directory))
    (condition-case ()
	(delete-file to-file)
      (file-error nil))
    (message "Writing messages to %s..." to-file)
    (goto-char (point-min))

    (let ((temp-buffer (get-buffer-create " unrmail"))
	  (from-buffer (current-buffer)))

      ;; Process the messages one by one.
      (while (search-forward "\^_\^l" nil t)
	(let ((beg (point))
	      (end (save-excursion
		     (if (search-forward "\^_" nil t)
			 (1- (point)) (point-max))))
	      (coding 'raw-text)
	      label-line attrs keywords
	      mail-from reformatted)
	  (with-current-buffer temp-buffer
	    (setq buffer-undo-list t)
	    (erase-buffer)
	    (setq buffer-file-coding-system coding)
	    (insert-buffer-substring from-buffer beg end)
	    (goto-char (point-min))
	    (forward-line 1)
	    ;; Record whether the header is reformatted.
	    (setq reformatted (= (following-char) ?1))

	    ;; Collect the label line, then get the attributes
	    ;; and the keywords from it.
	    (setq label-line
		  (buffer-substring (point)
				    (save-excursion (forward-line 1)
						    (point))))
	    (search-forward ",,")
	    (unless (eolp)
	      (setq keywords
		    (buffer-substring (point)
				      (progn (end-of-line)
					     (1- (point)))))
	      (setq keywords
		    (replace-regexp-in-string ", " "," keywords)))

	    (setq attrs
		  (list
		   (if (string-match ", answered," label-line) ?A ?-)
		   (if (string-match ", deleted," label-line) ?D ?-)
		   (if (string-match ", edited," label-line) ?E ?-)
		   (if (string-match ", filed," label-line) ?F ?-)
		   (if (string-match ", resent," label-line) ?R ?-)
		   (if (string-match ", unseen," label-line) ?\  ?-)
		   (if (string-match ", stored," label-line) ?S ?-)))

	    ;; Delete the special Babyl lines at the start,
	    ;; and the ***EOOH*** line, and the reformatted header if any.
	    (goto-char (point-min))
	    (if reformatted
		(progn
		  (forward-line 2)
		  ;; Delete Summary-Line headers.
		  (let ((case-fold-search t))
		    (while (looking-at "Summary-Line:")
		      (forward-line 1)))
		  (delete-region (point-min) (point))
		  ;; Delete the old reformatted header.
		  (re-search-forward "^[*][*][*] EOOH [*][*][*]\n")
		  (forward-line -1)
		  (let ((start (point)))
		    (search-forward "\n\n")
		    (delete-region start (point))))
	      ;; Not reformatted.  Delete the special
	      ;; lines before the real header.
	      (re-search-forward "^[*][*][*] EOOH [*][*][*]\n")
	      (delete-region (point-min) (point)))

	    ;; Some operations on the message header itself.
	    (goto-char (point-min))
	    (save-restriction
	      (narrow-to-region 
	       (point-min)
	       (save-excursion (search-forward "\n\n" nil 'move) (point)))

	      ;; Fetch or construct what we should use in the `From ' line.
	      (setq mail-from
		    (or (mail-fetch-field "Mail-From")
			(concat "From "
				(mail-strip-quoted-names (or (mail-fetch-field "from")
							     (mail-fetch-field "really-from")
							     (mail-fetch-field "sender")
							     "unknown"))
				" " (current-time-string))))

	      ;; If the message specifies a coding system, use it.
	      (let ((maybe-coding (mail-fetch-field "X-Coding-System")))
		(if maybe-coding
		    (setq coding (intern maybe-coding))))

	      ;; Delete the Mail-From: header field if any.
	      (when (re-search-forward "^Mail-from:" nil t)
		(beginning-of-line)
		(delete-region (point)
			       (progn (forward-line 1) (point)))))

	    (goto-char (point-min))
	    ;; Insert the `From ' line.
	    (insert mail-from "\n")
	    ;; Record the keywords and attributes in our special way.
	    (insert "X-BABYL-V6-ATTRIBUTES: " (apply 'string attrs) "\n")
	    (when keywords
	      (insert "X-BABYL-V6-KEYWORDS: " keywords "\n"))
	    (goto-char (point-min))
	    ;; ``Quote'' "\nFrom " as "\n>From "
	    ;;  (note that this isn't really quoting, as there is no requirement
	    ;;   that "\n[>]+From " be quoted in the same transparent way.)
	    (let ((case-fold-search nil))
	      (while (search-forward "\nFrom " nil t)
		(forward-char -5)
		(insert ?>)))
	    ;; Write it to the output file.
	    (write-region (point-min) (point-max) to-file t
			  'nomsg))))
      (kill-buffer temp-buffer))
    (message "Writing messages to %s...done" to-file)))

(provide 'unrmail)

;;; unrmail.el ends here

;;; arch-tag: 14c6290d-60b2-456f-8909-5c2387de6acb
