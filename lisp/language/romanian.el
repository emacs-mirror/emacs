;;; romanian.el --- Support for Romanian

;; Copyright (C) 1998 Free Software Foundation.

;; Author:    Dan Nicolaescu <done@ece.arizona.edu>
;; Keywords: multilingual, Romanian

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

;; Romanian ISO 8859-2 environment.

;;; Code:

(defun setup-romanian-environment ()
  "Setup multilingual environment (MULE) for Romanian."
  (interactive)
  (setup-8-bit-environment "Romanian" 'latin-iso8859-2 "latin-2-postfix")
  (load "latin-2"))

(set-language-info-alist
 "Romanian" '((setup-function . setup-romanian-environment)
	    (charset . (ascii latin-iso8859-2))
	    (coding-system . (iso-8859-2))
	    (coding-priority . (iso-8859-2))
	    (sample-text . "Bun,Ac(B ziua, bine a,A~(Bi venit!")
	    (documentation . nil))
 '("European"))

(provide 'romanian)

;; romanian.el ends here

