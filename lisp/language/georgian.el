;;; georgian.el --- language support for Georgian

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: i18n

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

;;; Code:

(set-language-info-alist
 "Georgian" `((coding-system georgian-ps)
	      (coding-priority georgian-ps)
	      (input-method . "georgian")
	      (features code-pages)
	      (documentation . "Support for georgian-ps character set."))
 '("European"))				; fixme: is this appropriate for
					; a non-Latin script?

(provide 'georgian)

;;; georgian.el ends here
