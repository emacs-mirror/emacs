;;; uudecode.el -- elisp native uudecode

;; Copyright (c) 1998, 1999, 2000 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: uudecode news

;; This file is a part of GNU Emacs.

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

;;     Lots of codes are stolen from mm-decode.el, gnus-uu.el and
;;     base64.el

;; This looks as though it could be made rather more efficient.
;; Encoding could use a lookup table and decoding should presumably
;; use a vector or list buffer for partial results rather than
;; with-current-buffer.  -- fx

;;; Code:

(eval-when-compile (require 'cl))

(eval-and-compile
  (defalias 'uudecode-char-int
    (if (fboundp 'char-int)
	'char-int
      'identity))

  (if (fboundp 'insert-char)
      (defalias 'uudecode-insert-char 'insert-char)
    (defun uudecode-insert-char (char &optional count ignored buffer)
      (if (or (null buffer) (eq buffer (current-buffer)))
	  (insert-char char count)
	(with-current-buffer buffer
	  (insert-char char count))))))

(defcustom uudecode-decoder-program "uudecode"
  "*Non-nil value should be a string that names a uu decoder.
The program should expect to read uu data on its standard
input and write the converted data to its standard output."
  :type 'string
  :group 'gnus-extract)

(defcustom uudecode-decoder-switches nil
  "*List of command line flags passed to `uudecode-decoder-program'."
  :group 'gnus-extract
  :type '(repeat string))

(defconst uudecode-alphabet "\040-\140")

(defconst uudecode-begin-line "^begin[ \t]+[0-7][0-7][0-7][ \t]+\\(.*\\)$")
(defconst uudecode-end-line "^end[ \t]*$")

(defconst uudecode-body-line
  (let ((i 61) (str "^M"))
    (while (> (setq i (1- i)) 0)
      (setq str (concat str "[^a-z]")))
    (concat str ".?$")))

(defvar uudecode-temporary-file-directory
  (cond ((fboundp 'temp-directory) (temp-directory))
	((boundp 'temporary-file-directory) temporary-file-directory)
	("/tmp")))

;;;###autoload
(defun uudecode-decode-region-external (start end &optional file-name)
  "Uudecode region between START and END with external decoder.

If FILE-NAME is non-nil, save the result to FILE-NAME."
  (interactive "r\nP")
  (let ((cbuf (current-buffer)) tempfile firstline work-buffer status)
    (save-excursion
      (goto-char start)
      (when (re-search-forward uudecode-begin-line nil t)
	(forward-line 1)
	(setq firstline (point))
	(cond ((null file-name))
	      ((stringp file-name))
	      (t
	       (setq file-name (read-file-name "File to Name:"
					       nil nil nil
					       (match-string 1)))))
	(setq tempfile (if file-name
			   (expand-file-name file-name)
			 (make-temp-name
			  ;; /tmp/uu...
			  (expand-file-name
			   "uu" uudecode-temporary-file-directory))))
	(let ((cdir default-directory) default-process-coding-system)
	  (unwind-protect
	      (progn
		(set-buffer (setq work-buffer
				  (generate-new-buffer " *uudecode-work*")))
		(buffer-disable-undo work-buffer)
		(insert "begin 600 " (file-name-nondirectory tempfile) "\n")
		(insert-buffer-substring cbuf firstline end)
		(cd (file-name-directory tempfile))
		(apply 'call-process-region
		       (point-min)
		       (point-max)
		       uudecode-decoder-program
		       nil
		       nil
		       nil
		       uudecode-decoder-switches))
	    (cd cdir) (set-buffer cbuf)))
	(if (file-exists-p tempfile)
	    (unless file-name
	      (goto-char start)
	      (delete-region start end)
	      (let (format-alist)
		(insert-file-contents-literally tempfile)))
	  (message "Can not uudecode")))
      (and work-buffer (kill-buffer work-buffer))
      (ignore-errors (or file-name (delete-file tempfile))))))

;;;###autoload

(defun uudecode-decode-region (start end &optional file-name)
  "Uudecode region between START and END.
If FILE-NAME is non-nil, save the result to FILE-NAME."
  (interactive "r\nP")
  (let ((work-buffer nil)
	(done nil)
	(counter 0)
	(remain 0)
	(bits 0)
	(lim 0) inputpos
	(non-data-chars (concat "^" uudecode-alphabet)))
    (unwind-protect
	(save-excursion
	  (goto-char start)
	  (when (re-search-forward uudecode-begin-line nil t)
	    (cond ((null file-name))
		  ((stringp file-name))
		  (t
		   (setq file-name (expand-file-name
				    (read-file-name "File to Name:"
						    nil nil nil
						    (match-string 1))))))
	    (setq work-buffer (generate-new-buffer " *uudecode-work*"))
	    (forward-line 1)
	    (skip-chars-forward non-data-chars end)
	    (while (not done)
	      (setq inputpos (point))
	      (setq remain 0 bits 0 counter 0)
	      (cond
	       ((> (skip-chars-forward uudecode-alphabet end) 0)
		(setq lim (point))
		(setq remain
		      (logand (- (uudecode-char-int (char-after inputpos)) 32)
			      63))
		(setq inputpos (1+ inputpos))
		(if (= remain 0) (setq done t))
		(while (and (< inputpos lim) (> remain 0))
		  (setq bits (+ bits
				(logand
				 (-
				  (uudecode-char-int (char-after inputpos)) 32)
				 63)))
		  (if (/= counter 0) (setq remain (1- remain)))
		  (setq counter (1+ counter)
			inputpos (1+ inputpos))
		  (cond ((= counter 4)
			 (uudecode-insert-char
			  (lsh bits -16) 1 nil work-buffer)
			 (uudecode-insert-char
			  (logand (lsh bits -8) 255) 1 nil work-buffer)
			 (uudecode-insert-char (logand bits 255) 1 nil
					       work-buffer)
			 (setq bits 0 counter 0))
			(t (setq bits (lsh bits 6)))))))
	      (cond
	       (done)
	       ((> 0 remain)
		(error "uucode line ends unexpectly")
		(setq done t))
	       ((and (= (point) end) (not done))
		;;(error "uucode ends unexpectly")
		(setq done t))
	       ((= counter 3)
		(uudecode-insert-char (logand (lsh bits -16) 255) 1 nil
				      work-buffer)
		(uudecode-insert-char (logand (lsh bits -8) 255) 1 nil
				      work-buffer))
	       ((= counter 2)
		(uudecode-insert-char (logand (lsh bits -10) 255) 1 nil
				      work-buffer)))
	      (skip-chars-forward non-data-chars end))
	    (if file-name
		(save-excursion
		  (set-buffer work-buffer)
		  (write-file file-name))
	      (or (markerp end) (setq end (set-marker (make-marker) end)))
	      (goto-char start)
	      (insert-buffer-substring work-buffer)
	      (delete-region (point) end))))
      (and work-buffer (kill-buffer work-buffer)))))

(provide 'uudecode)

;;; uudecode.el ends here
