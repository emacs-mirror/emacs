;;; ps-print-def.el --- Common definitions for ps-print package

;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author:	Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Maintainer:	Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Keywords:	wp, print, PostScript
;; Time-stamp:	<99/07/03 20:16:48 vinicius>
;; Version:	1.0

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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Common definitions for ps-print package.
;;
;; See ps-print.el, ps-mule.el and ps-bdf.el for documentation.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:


;; `ps-multibyte-buffer' definition should be placed in `ps-mule', but
;; `ps-print' and `ps-mule' both use it so it's here.

(defcustom ps-multibyte-buffer nil
  "*Specify the multi-byte buffer handling.

Valid values are:

  nil                     This is the value to use the default settings which
			  is by default for printing buffer with only ASCII
			  and Latin characters.   The default setting can be
			  changed by setting the variable
			  `ps-mule-font-info-database-default' differently.
			  The initial value of this variable is
			  `ps-mule-font-info-database-latin' (see
			  documentation).

  `non-latin-printer'     This is the value to use when you have a Japanese
			  or Korean PostScript printer and want to print
			  buffer with ASCII, Latin-1, Japanese (JISX0208 and
			  JISX0201-Kana) and Korean characters.  At present,
			  it was not tested the Korean characters printing.
			  If you have a korean PostScript printer, please,
			  test it.

  `bdf-font'              This is the value to use when you want to print
			  buffer with BDF fonts.  BDF fonts include both latin
			  and non-latin fonts.  BDF (Bitmap Distribution
			  Format) is a format used for distributing X's font
			  source file.  BDF fonts are included in
			  `intlfonts-1.1' which is a collection of X11 fonts
			  for all characters supported by Emacs.  In order to
			  use this value, be sure to have installed
			  `intlfonts-1.1' and set the variable
			  `bdf-directory-list' appropriately (see ps-bdf.el for
			  documentation of this variable).

  `bdf-font-except-latin' This is like `bdf-font' except that it is used
			  PostScript default fonts to print ASCII and Latin-1
			  characters.  This is convenient when you want or
			  need to use both latin and non-latin characters on
			  the same buffer.  See `ps-font-family',
			  `ps-header-font-family' and `ps-font-info-database'.

Any other value is treated as nil."
  :type '(choice :tag "Multi-Byte Buffer"
		 (const non-latin-printer)     (const bdf-font)
		 (const bdf-font-except-latin) (other :tag "nil" nil))
  :group 'ps-print-font)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ps-print-def)

;;; ps-print-def.el ends here
