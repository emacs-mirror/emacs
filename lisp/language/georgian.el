;;; georgian.el --- language support for Georgian -*- no-byte-compile: t -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007
;;   Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(set-language-info-alist
 "Georgian" `((coding-system georgian-ps)
	      (coding-priority georgian-ps)
	      (input-method . "georgian")
	      (features code-pages)
	      (nonascii-translation . ,(get 'decode-georgian-ps
					    'translation-table))
	      (documentation . "Support for georgian-ps character set."))
 '("European"))				; fixme: is this appropriate for
					; a non-Latin script?

(provide 'georgian)

;;; arch-tag: 15499fbb-26d4-4a13-9d78-135eef7d32f5
;;; georgian.el ends here
