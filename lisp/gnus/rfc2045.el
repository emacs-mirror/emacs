;;; rfc2045.el --- Functions for decoding rfc2045 headers

;; Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'ietf-drums)

(defun rfc2045-encode-string (param value)
  "Return and PARAM=VALUE string encoded according to RFC2045."
  (if (or (string-match (concat "[" ietf-drums-no-ws-ctl-token "]") value)
	  (string-match (concat "[" ietf-drums-tspecials "]") value)
	  (string-match "[ \n\t]" value)
	  (not (string-match (concat "[" ietf-drums-text-token "]") value)))
      (concat param "=" (format "%S" value))
    (concat param "=" value)))

(provide 'rfc2045)

;;; rfc2045.el ends here
