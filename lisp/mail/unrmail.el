;;; unrmail.el --- convert Rmail files to mailbox files

;;; Copyright (C) 1992, 2002 Free Software Foundation, Inc.

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
  (let ((message-count 1)
	;; Prevent rmail from making, or switching to, a summary buffer.
	(rmail-display-summary nil)
	(rmail-delete-after-output nil)
	(temp-buffer (get-buffer-create " unrmail")))
    (rmail file)
    ;; Default the directory of TO-FILE based on where FILE is.
    (setq to-file (expand-file-name to-file default-directory))
    (condition-case ()
	(delete-file to-file)
      (file-error nil))
    (message "Writing messages to %s..." to-file)
    (save-restriction
      (widen)
      (while (<= message-count rmail-total-messages)
	(let ((beg (rmail-msgbeg message-count))
	      (end (rmail-msgbeg (1+ message-count)))
	      (from-buffer (current-buffer))
	      (coding (or rmail-file-coding-system 'raw-text))
	      label-line attrs keywords
	      header-beginning mail-from)
	  (save-excursion
	    (goto-char (rmail-msgbeg message-count))
	    (setq header-beginning (point))
	    (search-forward "\n*** EOOH ***\n")
	    (forward-line -1)
	    (search-forward "\n\n")
	    (save-restriction
	      (narrow-to-region header-beginning (point))
	      (setq mail-from
		    (or (mail-fetch-field "Mail-From")
			(concat "From "
				(mail-strip-quoted-names (or (mail-fetch-field "from")
							     (mail-fetch-field "really-from")
							     (mail-fetch-field "sender")
							     "unknown"))
				" " (current-time-string))))))
	  (with-current-buffer temp-buffer
	    (setq buffer-undo-list t)
	    (erase-buffer)
	    (setq buffer-file-coding-system coding)
	    (insert-buffer-substring from-buffer beg end)
	    (goto-char (point-min))
	    (forward-line 1)
	    (setq label-line
		  (buffer-substring (point)
				    (progn (forward-line 1)
					   (point))))
	    (forward-line -1)
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
	    (unrmail-unprune)
	    (goto-char (point-min))
	    (insert mail-from "\n")
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
	    (write-region (point-min) (point-max) to-file t
			  'nomsg)))
	(setq message-count (1+ message-count))))
    (message "Writing messages to %s...done" to-file)))

(defun unrmail-unprune ()
  (let* ((pruned
	  (save-excursion
	    (goto-char (point-min))
	    (forward-line 1)
	    (= (following-char) ?1))))
    (if pruned
	(progn
	  (goto-char (point-min))
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
      ;; Delete everything up to the real header.
      (goto-char (point-min))
      (re-search-forward "^[*][*][*] EOOH [*][*][*]\n")
      (delete-region (point-min) (point)))
    (goto-char (point-min))
    (when (re-search-forward "^Mail-from:")
      (beginning-of-line)
      (delete-region (point)
		     (progn (forward-line 1) (point))))))


(provide 'unrmail)

;;; unrmail.el ends here

;;; arch-tag: 14c6290d-60b2-456f-8909-5c2387de6acb
