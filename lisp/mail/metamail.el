;;; metamail.el --- Metamail interface for GNU Emacs

;; Copyright (C) 1993 Masanobu UMEDA

;; Author: Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
;; Version: $Header: /home/gd/gnu/emacs/19.0/lisp/RCS/metamail.el,v 1.1 1993/07/20 03:02:12 rms Exp kwzh $
;; Keywords: mail, news, mime, multimedia

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

;; LCD Archive Entry:
;; metamail|Masanobu UMEDA|umerin@mse.kyutech.ac.jp|
;; Metamail interface for GNU Emacs|
;; $Date: 1993/07/20 03:02:12 $|$Revision: 1.1 $|~/misc/metamail.el.Z|

;; Note: Metamail does not have all options which are compatible with
;; the environment variables.  For that reason, metamail.el has to
;; hack the environment variables.  In addition, there is no way to
;; display all header fields without extra informative body messages
;; which is suppressed by "-q" option.

;; The idea of using metamail to process MIME messages is from
;; gnus-mime.el by Spike <Spike@world.std.com>.

;;; Code:

(defvar metamail-program-name "metamail"
  "*Metamail program name.")

(defvar metamail-environment '("KEYHEADS=*")
  "*Environment variables passed to `metamail'.
It must ba a list of strings that have the format ENVVARNAME=VALUE.")

(defvar metamail-switches '("-m" "emacs" "-x" "-d" "-z")
  "*Switches for `metamail' program.
-z is required to remove zap file.")

(defun metamail-buffer (&optional buffer nodisplay)
  "Process current buffer through `metamail'.
Optional 1st argument BUFFER specifies a buffer to be filled (nil
means current).
Optional 2nd argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted."
  (interactive)
  (metamail-region (point-min) (point-max) buffer nodisplay))

(defun metamail-region (beg end &optional buffer nodisplay)
  "Process current region through 'metamail'.
Optional 1st argument BUFFER specifies a buffer to be filled (nil
means current).
Optional 2nd argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted."
  (interactive "r")
  (let ((curbuf (current-buffer))
	(buffer-read-only nil)
	(metafile (make-temp-name "/tmp/metamail")))
    (save-excursion
      ;; Gee!  Metamail does not output to stdout if input comes from
      ;; stdin.
      (write-region beg end metafile nil 'nomessage)
      (if buffer
	  (set-buffer buffer))
      (setq buffer-read-only nil)
      ;; Clear destination buffer.
      (if (eq curbuf (current-buffer))
	  (delete-region beg end)
	(delete-region (point-min) (point-max)))
      ;; We have to pass the environment variable KEYHEADS to display
      ;; all header fields.  Metamail should have an optional argument
      ;; to pass such information directly.
      (let ((process-environment
	     (append metamail-environment process-environment)))
	(apply (function call-process)
	       metamail-program-name
	       nil
	       t			;Output to current buffer
	       (not nodisplay)		;Force redisplay
	       (append metamail-switches (list metafile))))
      ;; `metamail' may not delete the temporary file!
      (condition-case error
	  (delete-file metafile)
	(error nil))
      )))

;(defun metamail-region (beg end &optional buffer)
;  "Process current region through 'metamail'.
;Optional argument BUFFER specifies a buffer to be filled (nil means current)."
;  (interactive "r")
;  (let ((curbuf (current-buffer))
;	(buffer-read-only nil)
;	(metafile (make-temp-name "/tmp/metamail")))
;    (save-excursion
;      (write-region (point-min) (point-max) metafile nil 'nomessage)
;      (if (eq curbuf
;	      (if buffer (get-buffer buffer) (current-buffer)))
;	  (delete-region (point-min) (point-max)))
;      (apply (function call-process)
;	     metamail-program-name
;	     nil
;	     (or buffer t)		;BUFFER or current buffer
;	     nil			;don't redisplay
;	     (append metamail-switches (list metafile)))
;      )))

(provide 'metamail)

;;; metamail.el ends here
