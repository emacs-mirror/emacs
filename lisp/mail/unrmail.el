;;; unrmail.el --- convert Rmail files to mailbox files.

;;; Copyright (C) 1992 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

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
  (let ((message-count 0)
	;; Prevent rmail from making, or switching to, a summary buffer.
	(rmail-display-summary nil)
	(rmail-delete-after-output nil))
    (rmail file)
    (message "Writing messages to %s..." to-file)
    (while (< message-count rmail-total-messages)
      (rmail-show-message
       (setq message-count (1+ message-count)))
      (rmail-output to-file 1 t))
    (message "Writing messages to %s...done" to-file)))

;;; unrmail.el ends here
