;;; rfc2045.el --- Functions for decoding rfc2045 headers

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; RFC 2045 is: "Multipurpose Internet Mail Extensions (MIME) Part
;; One:  Format of Internet Message Bodies".

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

;;; arch-tag: 9ca54127-97bc-432c-b6e2-8c59cadba306
;;; rfc2045.el ends here
