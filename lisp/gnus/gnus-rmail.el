;;; gnus-rmail.el --- Saving to rmail/babyl files  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

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

;;

;;; Code:

;;; Functions for saving to babyl/mail files.

(require 'rmail)
(require 'rmailsum)
(require 'nnmail)

(defun gnus-output-to-rmail (filename &optional ask)
  "Append the current article to an Rmail file named FILENAME.
In Emacs 22 this writes Babyl format; in Emacs 23 it writes mbox unless
FILENAME exists and is Babyl format."
  ;; Some of this codes is borrowed from rmailout.el.
  (setq filename (expand-file-name filename))
  ;; FIXME should we really be messing with this defcustom?
  ;; It is not needed for the operation of this function.
  (if (boundp 'rmail-default-rmail-file)
      (setq rmail-default-rmail-file filename) ; 22
    (setq rmail-default-file filename))        ; 23
  (let ((artbuf (current-buffer))
	(tmpbuf (gnus-get-buffer-create " *Gnus-output*"))
        ;; Babyl rmail.el defines this, mbox does not.
        (babyl (fboundp 'rmail-insert-rmail-file-header)))
    (save-excursion
      ;; Note that we ignore the possibility of visiting a Babyl
      ;; format buffer in Emacs 23, since Rmail no longer supports that.
     (or (get-file-buffer filename)
         (progn
           ;; In case someone wants to write to a Babyl file from Emacs 23.
           (when (file-exists-p filename)
             (setq babyl (mail-file-babyl-p filename))
             t))
	  (if (or (not ask)
		  (gnus-yes-or-no-p
		   (concat "\"" filename "\" does not exist, create it? ")))
	      (let ((file-buffer (create-file-buffer filename)))
		(with-current-buffer file-buffer
                  (if (fboundp 'rmail-insert-rmail-file-header)
                      (rmail-insert-rmail-file-header))
		  (let ((require-final-newline nil)
			(coding-system-for-write mm-text-coding-system))
		    (gnus-write-buffer filename)))
		(kill-buffer file-buffer))
	    (error "Output file does not exist")))
      (set-buffer tmpbuf)
      (erase-buffer)
      (insert-buffer-substring artbuf)
      (if babyl
          (gnus-convert-article-to-rmail)
        ;; Non-Babyl case copied from gnus-output-to-mail.
        (goto-char (point-min))
        (if (looking-at "From ")
            (forward-line 1)
          (insert "From nobody " (current-time-string) "\n"))
        (let (case-fold-search)
          (while (re-search-forward "^From " nil t)
            (beginning-of-line)
            (insert ">"))))
      ;; Decide whether to append to a file or to an Emacs buffer.
      (let ((outbuf (get-file-buffer filename)))
	(if (not outbuf)
            (progn
              (unless babyl             ; from gnus-output-to-mail
                (let ((buffer-read-only nil))
                  (goto-char (point-max))
                  (forward-char -2)
                  (unless (looking-at "\n\n")
                    (goto-char (point-max))
                    (unless (bolp)
                      (insert "\n"))
                    (insert "\n"))))
              (let ((file-name-coding-system nnmail-pathname-coding-system))
                (mm-append-to-file (point-min) (point-max) filename)))
	  ;; File has been visited, in buffer OUTBUF.
	  (set-buffer outbuf)
	  (let ((buffer-read-only nil)
		(msg (and (boundp 'rmail-current-message)
			  (symbol-value 'rmail-current-message))))
	    ;; If MSG is non-nil, buffer is in RMAIL mode.
            ;; Compare this with rmail-output-to-rmail-buffer in Emacs 23.
	    (when msg
              (unless babyl
                (rmail-swap-buffers-maybe)
                (rmail-maybe-set-message-counters))
              (widen)
              (unless babyl
		(goto-char (point-max))
		;; Ensure we have a blank line before the next message.
		(unless (bolp)
		  (insert "\n"))
		(insert "\n"))
              (narrow-to-region (point-max) (point-max)))
	    (insert-buffer-substring tmpbuf)
	    (when msg
              (when babyl
                (goto-char (point-min))
                (widen)
                (search-backward "\n\^_")
                (narrow-to-region (point) (point-max)))
	      (rmail-count-new-messages t)
	      (when (rmail-summary-exists)
		(rmail-select-summary
		 (rmail-update-summary)))
	      (rmail-show-message msg))
	    (save-buffer)))))
    (kill-buffer tmpbuf)))

(defun gnus-convert-article-to-rmail ()
  "Convert article in current buffer to Rmail message format."
  (let ((buffer-read-only nil))
    ;; Convert article directly into Babyl format.
    (goto-char (point-min))
    (insert "\^L\n0, unseen,,\n*** EOOH ***\n")
    (while (search-forward "\n\^_" nil t) ;single char
      (replace-match "\n^_" t t))	;2 chars: "^" and "_"
    (goto-char (point-max))
    (insert "\^_")))

;;; gnus-rmail.el ends here
