;;; md5.el --- MD5 message digest calculation (RFC 1321)

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: mail, processes, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provides the `md5' function for computing the MD5 `message
;; digest'/`fingerprint'/`checksum' for a string or buffer.  This is
;; compatible with the XEmacs version.  We expect to have primitive
;; MD5 support in a future version of Emacs.

;; MD5 is defined in RFC 1321.

;;; Code:

;; Not worth customizing?  Will go away anyhow with primitive support.
(defvar md5-program "md5sum"
  "Name of a program to calculate MD5 (message digest) checksums.
This should read standard input and output the MD5 checksum in the
first 32 bytes of standard output.  `md5sum' is in GNU Textutils.  An
alternative is `md5', present in the SSLeay distribution.")

;;;###autoload
(defun md5 (object &optional start end encoding noerror)
  "Return the MD5 message digest (checksum or fingerprint) of OBJECT.
OBJECT is a buffer or a atring.  Optional arguments START and END
specify a region of the object to use, where the first character is 1
for both buffers and strings.

Optional argument ENCODING specifies a coding system with which to
encode the text for computing the digest.  If omitted, the normal
rules will be used to find a coding system for output to
`md5-program'.  It probably makes most sense to use unibyte data and
`binary' encoding.  Optional argument NOERROR is for XEmacs
compatibility and is ignored.

In this implementation, the program named by `md5-program' is run to
do the calculation.

MD5 is defined in RFC 1321."
  (with-temp-buffer
    (let ((in-buffer (current-buffer))
	  (out-buffer (current-buffer)))
      (if (stringp object)
	  (insert object)
	(setq in-buffer object))
      (goto-char (point-min))
      (unless encoding
	(setq encoding coding-system-for-write))
      (with-current-buffer in-buffer
	(let ((coding-system-for-write encoding))
	  (unless (eq 0 (call-process-region (or start (point-min))
					     (or end (point-max))
					     md5-program nil out-buffer))
	    (error "Running MD5 command %s failed"
		   (cons md5-program md5-program-args)))))
      ;; The meaningful output is the first 32 characters.
      ;; Don't return the newline that follows them!
      (buffer-substring 1 33))))

;;; md5.el ends here
