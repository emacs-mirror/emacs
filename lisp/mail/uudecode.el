;;; uudecode.el --- elisp native uudecode  -*- lexical-binding:t -*-

;; Copyright (C) 1998-2022 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: uudecode news

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

;;; Code:

(defgroup uudecode nil
  "Decoding of uuencoded data."
  :group 'mail
  :group 'news)

(defcustom uudecode-decoder-program "uudecode"
  "Non-nil value should be a string that names a uu decoder.
The program should expect to read uu data on its standard
input and write the converted data to its standard output."
  :type 'string)

(defcustom uudecode-decoder-switches nil
  "List of command line flags passed to `uudecode-decoder-program'."
  :type '(repeat string))

(defcustom uudecode-use-external
  (executable-find uudecode-decoder-program)
  "Use external uudecode program."
  :version "22.1"
  :type 'boolean)

(defconst uudecode-alphabet "\040-\140")

(defconst uudecode-begin-line "^begin[ \t]+[0-7][0-7][0-7][ \t]+\\(.*\\)$")
(defconst uudecode-end-line "^end[ \t]*$")

(defconst uudecode-body-line
  (let ((i 61) (str "^M"))
    (while (> (setq i (1- i)) 0)
      (setq str (concat str "[^a-z]")))
    (concat str ".?$")))

(make-obsolete-variable 'uudecode-temporary-file-directory
                        'temporary-file-directory "28.1")

;;;###autoload
(defun uudecode-decode-region-external (start end &optional file-name)
  "Uudecode region between START and END using external program.
If FILE-NAME is non-nil, save the result to FILE-NAME.  The program
used is specified by `uudecode-decoder-program'."
  (interactive "r\nP")
  (let ((cbuf (current-buffer)) tempfile firstline)
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
			 (make-temp-file "uu")))
	(let ((cdir default-directory)
	      (default-process-coding-system nil))
	  (unwind-protect
	      (with-temp-buffer
		(insert "begin 600 " (file-name-nondirectory tempfile) "\n")
		(insert-buffer-substring cbuf firstline end)
		(cd (file-name-directory tempfile))
		(apply #'call-process-region
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
      (ignore-errors (or file-name (delete-file tempfile))))))

;;;###autoload
(defun uudecode-decode-region-internal (start end &optional file-name)
  "Uudecode region between START and END without using an external program.
If FILE-NAME is non-nil, save the result to FILE-NAME."
  (interactive "r\nP")
  (let ((done nil)
	(counter 0)
	(remain 0)
	(bits 0)
	(lim 0) inputpos result
	(non-data-chars (concat "^" uudecode-alphabet)))
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
	(forward-line 1)
	(skip-chars-forward non-data-chars end)
	(while (not done)
	  (setq inputpos (point))
	  (setq remain 0 bits 0 counter 0)
	  (cond
	   ((> (skip-chars-forward uudecode-alphabet end) 0)
	    (setq lim (point))
	    (setq remain
                  (logand (- (char-after inputpos) 32)
			  63))
	    (setq inputpos (1+ inputpos))
	    (if (= remain 0) (setq done t))
	    (while (and (< inputpos lim) (> remain 0))
	      (setq bits (+ bits
			    (logand
			     (-
                              (char-after inputpos) 32)
			     63)))
	      (if (/= counter 0) (setq remain (1- remain)))
	      (setq counter (1+ counter)
		    inputpos (1+ inputpos))
	      (cond ((= counter 4)
		     (setq result (cons (logand bits 255)
					(cons (logand (ash bits -8) 255)
					      (cons (ash bits -16)
						    result))))
		     (setq bits 0 counter 0))
		    (t (setq bits (ash bits 6)))))))
	  (cond
	   (done)
	   ((> 0 remain)
	    (error "uucode line ends unexpectedly")
	    (setq done t))
	   ((and (= (point) end) (not done))
	    ;;(error "uucode ends unexpectedly")
	    (setq done t))
	   ((= counter 3)
	    (setq result (cons (logand (ash bits -8) 255)
			       (cons (logand (ash bits -16) 255)
				     result))))
	   ((= counter 2)
	    (setq result (cons (logand (ash bits -10) 255)
			       result))))
	  (skip-chars-forward non-data-chars end))
	(if file-name
            (with-temp-file file-name
              (set-buffer-multibyte nil)
              (apply #'insert (nreverse result)))
	  (or (markerp end) (setq end (set-marker (make-marker) end)))
	  (goto-char start)
          (apply #'insert
                 (nreverse
                  (if enable-multibyte-characters
                      (mapcar (lambda (ch)
                                (or (decode-char 'eight-bit ch) ch))
                              result)
                    result)))
	  (delete-region (point) end))))))

;;;###autoload
(defun uudecode-decode-region (start end &optional file-name)
  "Uudecode region between START and END.
If FILE-NAME is non-nil, save the result to FILE-NAME."
  (if uudecode-use-external
      (uudecode-decode-region-external start end file-name)
    (uudecode-decode-region-internal start end file-name)))

(define-obsolete-function-alias 'uudecode-char-int #'identity "28.1")

(provide 'uudecode)

;;; uudecode.el ends here
