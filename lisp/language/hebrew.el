;;; hebrew.el --- Support for Hebrew

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, Hebrew

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; For Hebrew, the character sets ISO8859-8 is supported.

;;; Code:

(make-coding-system
 'hebrew-iso-8bit 2 ?8
 "ISO 2022 based 8-bit encoding for Hebrew (MIME:ISO-8859-8)"
 '((ascii t) (hebrew-iso8859-8 t) nil nil
   nil ascii-eol ascii-cntl nil nil nil nil nil t))

(define-coding-system-alias 'iso-8859-8 'hebrew-iso-8bit)

(defun setup-hebrew-environment ()
  "Setup multilingual environment (MULE) for Hebrew.
But, please note that right-to-left writing is not yet supported."
  (interactive)
  (setup-8-bit-environment "Hebrew" 'hebrew-iso8859-8 'hebrew-iso-8bit
			   "hebrew"))

(set-language-info-alist
 "Hebrew" '((setup-function . setup-hebrew-environment)
	    (describe-function . describe-hebrew-support)
	    (charset . (hebrew-iso8859-8))
	    (coding-system . (hebrew-iso-8bit))
	    (sample-text . "Hebrew	,Hylem(B")
	    (documentation . "Right-to-left writing is not yet supported.")
	    ))

;;; hebrew.el ends here
