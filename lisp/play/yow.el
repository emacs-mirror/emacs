;;; yow.el --- quote random zippyisms

;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: games

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

;;; Commentary:

;; Important pinheadery for GNU Emacs.
;;
;; See cookie1.el for implementation.  Note --- the `n' argument of yow
;; from the 18.xx implementation is no longer; we only support *random*
;; random access now.

;;; Code:

(require 'cookie1)

(defvar yow-file (concat data-directory "yow.lines")
   "Pertinent pinhead phrases.")

;;;###autoload
(defun yow (&optional interactive)
  "Return or display a random Zippy quotation."
  (interactive "p")
  (let ((yow (cookie
	      yow-file "Am I CONSING yet?..." "I have SEEN the CONSING!!")))
    (cond ((not interactive)
	   yow)
	  ((not (string-match "\n" yow))
	   (delete-windows-on (get-buffer-create "*Help*"))
	   (message "%s" yow))
	  (t
	   (message "Yow!")
	   (with-output-to-temp-buffer "*Help*"
	     (princ yow))))))

(defsubst read-zippyism (prompt &optional require-match)
  "Read a Zippyism from the minibuffer with completion, prompting with PROMPT.
If optional second arg is non-nil, require input to match a completion."
  (read-cookie prompt yow-file
	       "Am I CONSING yet?..." "I have SEEN the CONSING!!"
	       require-match))

; Yowza!! Feed zippy quotes to the doctor. Watch results.
; fun, fun, fun. Entertainment for hours...
;
; written by Kayvan Aghaiepour

;;;###autoload
(defun psychoanalyze-pinhead ()
  "Zippy goes to the analyst."
  (interactive)
  (doctor)				; start the psychotherapy
  (message "")
  (switch-to-buffer "*doctor*")
  (sit-for 0)
  (while (not (input-pending-p))
    (insert-string (yow))
    (sit-for 0)
    (doctor-ret-or-read 1)
    (doctor-ret-or-read 1)))

(provide 'yow)

;;; yow.el ends here
