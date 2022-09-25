;;; rfc6068.el --- support for rfc6068  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

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
;;; Code:

(defun rfc6068-unhexify-string (string &optional inhibit-decode)
  "Unhexify STRING -- e.g. `hello%20there' -> `hello there'.
STRING is assumed to be a percentage-encoded utf-8 string.

If INHIBIT-DECODE is non-nil, return the resulting raw byte
string instead of decoding as utf-8."
  (let ((string
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (insert string)
           (goto-char (point-min))
           (while (re-search-forward "%\\([[:xdigit:]]\\{2\\}\\)" nil t)
             (replace-match (string (string-to-number (match-string 1) 16))
                            t t))
           (buffer-string))))
    (if inhibit-decode
        string
      (decode-coding-string string 'utf-8))))

(defun rfc6068-parse-mailto-url (mailto-url)
  "Parse MAILTO-URL, and return an alist of header-name, header-value pairs.
MAILTO-URL should be a RFC 6068 (mailto) compliant url.  A cons cell w/ a
key of `Body' is a special case and is considered a header for this purpose.
The returned alist is intended for use w/ the `compose-mail' interface.
Note: make sure MAILTO-URL has been \"unhtmlized\" (e.g., &amp; -> &), before
calling this function."
  (let ((case-fold-search t)
	headers-alist)
    (setq mailto-url (string-replace "\n" " " mailto-url))
    (when (string-match "^\\(mailto:\\)\\([^?]+\\)?\\(\\?\\(.*\\)\\)*"
                        mailto-url)
      (let ((address (match-string 2 mailto-url))
            (query (match-string 4 mailto-url)))
	;; Build alist of header name-value pairs.
	(when query
	  (setq headers-alist
		(mapcar
		 (lambda (x)
		   (let* ((pair (split-string x "="))
			  (name (car pair))
			  (value (cadr pair)))
		     ;; Return ("Header-Name" . "header-value").
		     (cons
		      (capitalize (rfc6068-unhexify-string name))
		      (rfc6068-unhexify-string value))))
		 (split-string query "&"))))

	(when address
	  (setq address (rfc6068-unhexify-string address))
	  ;; Deal with multiple 'To' recipients.
	  (if-let ((elem (assoc "To" headers-alist)))
	      (setcdr elem (concat address ", " (cdr elem)))
            (push (cons "To" address) headers-alist)))

	headers-alist))))

(provide 'rfc6068)

;;; rfc6068.el ends here
