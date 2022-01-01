;;; flow-fill.el --- interpret RFC2646 "flowed" text  -*- lexical-binding:t -*-

;; Copyright (C) 2000-2022 Free Software Foundation, Inc.

;; Author: Simon Josefsson <jas@pdc.kth.se>
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This implement decoding of RFC2646 formatted text, including the
;; quoted-depth wins rules.

;; Theory of operation: search for lines ending with SPC, save quote
;; length of line, remove SPC and concatenate line with the following
;; line if quote length of following line matches current line.

;; When no further concatenations are possible, we've found a
;; paragraph and we let `fill-region' fill the long line into several
;; lines with the quote prefix as `fill-prefix'.

;; Todo: implement basic `fill-region'

;;; History:

;; 2000-02-17  posted on ding mailing list
;; 2000-02-19  use `point-at-{b,e}ol' in XEmacs
;; 2000-03-11  no compile warnings for point-at-bol stuff
;; 2000-03-26  committed to gnus cvs
;; 2000-10-23  don't flow "-- " lines, make "quote-depth wins" rule
;;             work when first line is at level 0.
;; 2002-01-12  probably incomplete encoding support
;; 2003-12-08  started working on test harness.

;;; Code:


(defcustom fill-flowed-display-column 'fill-column
  "Column beyond which format=flowed lines are wrapped, when displayed.
This can be a Lisp expression or an integer."
  :version "22.1"
  :group 'mime-display
  :type '(choice (const :tag "Standard `fill-column'" fill-column)
		 (const :tag "Fit Window" (- (window-width) 5))
		 (sexp)
		 (integer)))

(defcustom fill-flowed-encode-column 66
  "Column beyond which format=flowed lines are wrapped, in outgoing messages.
This can be a Lisp expression or an integer.
RFC 2646 suggests 66 characters for readability."
  :version "22.1"
  :group 'mime-display
  :type '(choice (const :tag "Standard fill-column" fill-column)
		 (const :tag "RFC 2646 default (66)" 66)
		 (sexp)
		 (integer)))

;;;###autoload
(defun fill-flowed-encode (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    ;; No point in doing this unless hard newlines is used.
    (when use-hard-newlines
      (let ((start (point-min)) end)
	;; Go through each paragraph, filling it and adding SPC
	;; as the last character on each line.
	(while (setq end (text-property-any start (point-max) 'hard 't))
	  (save-restriction
	    (narrow-to-region start end)
	    (let ((fill-column (eval fill-flowed-encode-column t)))
	      (fill-flowed-fill-buffer))
	    (goto-char (point-min))
	    (while (re-search-forward "\n" nil t)
	      (replace-match " \n" t t))
	    (goto-char (setq start (1+ (point-max)))))))
      t)))

(defun fill-flowed-fill-buffer ()
  (let ((prefix nil)
	(prev-prefix nil)
	(start (point-min)))
    (goto-char (point-min))
    (while (not (eobp))
      (setq prefix (and (looking-at "[> ]+")
			(match-string 0)))
      (if (equal prefix prev-prefix)
	  (forward-line 1)
	(save-restriction
	  (narrow-to-region start (point))
	  (let ((fill-prefix prev-prefix))
	    (fill-region (point-min) (point-max) t 'nosqueeze 'to-eop))
	  (goto-char (point-max)))
	(setq prev-prefix prefix
	      start (point))))
    (save-restriction
      (narrow-to-region start (point))
      (let ((fill-prefix prev-prefix))
	(fill-region (point-min) (point-max) t 'nosqueeze 'to-eop)))))

;;;###autoload
(defun fill-flowed (&optional buffer delete-space)
  "Apply RFC2646 decoding to BUFFER.
If BUFFER is nil, default to the current buffer.

If DELETE-SPACE, delete RFC2646 spaces padding at the end of
lines."
  (with-current-buffer (or buffer (current-buffer))
    (let ((fill-column  (eval fill-flowed-display-column t)))
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((and (looking-at "^>+")
               (eq (char-before (line-end-position)) ?\s))
          (let ((prefix (match-string 0)))
            ;; Insert a space character after the quote signs for more
            ;; pleasant reading of quoted lines.
            (goto-char (match-end 0))
            (unless (looking-at " ")
              (insert " "))
            (while (and (eq (char-before (line-end-position)) ?\s)
                        (not (eobp))
                        (save-excursion
                          (forward-line 1)
                          (looking-at (format "\\(%s ?\\)[^>]" prefix))))
              (end-of-line)
              (when (and (not (eobp))
                         (save-excursion
                           (forward-line 1)
                           (looking-at (format "\\(%s ?\\)[^>]" prefix))))
                ;; Delete the newline and the quote at the start of the
                ;; next line.
                (delete-region (point) (match-end 1))))
                (ignore-errors
		  (let ((fill-prefix (concat prefix " "))
		        adaptive-fill-mode)
		    (fill-region (line-beginning-position)
                                 (line-end-position)
			         'left 'nosqueeze)))))
          (t
          ;; Delete the newline.
          (when (eq (following-char) ?\s)
            (delete-char 1))
          ;; Hack: Don't do the flowing on the signature line.
          (when (and (not (looking-at "-- $"))
                     (eq (char-before (line-end-position)) ?\s))
            (while (and (not (eobp))
                        (eq (char-before (line-end-position)) ?\s))
              (end-of-line)
              (when delete-space
                (delete-char -1))
              (delete-char 1))
            (ignore-errors
		(let ((fill-prefix ""))
		  (fill-region (line-beginning-position)
                               (line-end-position)
			       'left 'nosqueeze))))))
        (forward-line 1)))))

(make-obsolete-variable 'fill-flowed-encode-tests nil "27.1")
(defvar fill-flowed-encode-tests)

(defun fill-flowed-test ()
  (declare (obsolete nil "27.1"))
  (interactive "")
  (user-error (concat "This function is obsolete.  Please see "
                      "test/lisp/mail/flow-fill-tests.el "
                      "in the Emacs source tree")))

(provide 'flow-fill)

;;; flow-fill.el ends here
