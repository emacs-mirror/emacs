;;; dos-vars.el --- MS-Dos specific user options.

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

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

(defgroup dos-fns nil
  "MS-DOS specific functions."
  :group 'environment)

(defcustom msdos-shells '("command.com" "4dos.com" "ndos.com")
  "*List of shells that use `/c' instead of `-c' and a backslashed command."
  :type '(repeat string)
  :group 'dos-fns)

;; Support for printing under MS-DOS, see lpr.el and ps-print.el.
(defcustom dos-printer "PRN"
  "*The name of a local MS-DOS device to which data is sent for printing.
\(Note that PostScript files are sent to `dos-ps-printer', which see.\)

Typical non-default settings would be \"LPT1\" to \"LPT3\" for
parallel printers, or \"COM1\" to \"COM4\" or \"AUX\" for serial
printers.  You can also set it to a name of a file, in which
case the output gets appended to that file.
If you want to discard the printed output, set this to \"NUL\"."
  :type 'file ; could use string but then we lose completion for files.
  :group 'dos-fns)

(defcustom dos-ps-printer "PRN"
  "*Method for printing PostScript files under MS-DOS.

If the value is a string, then it is taken as the name of the
device to which PostScript files are written.  By default it
is the default printer device; typical non-default settings
would be \"LPT1\" to \"LPT3\" for parallel printers, or \"COM1\"
to \"COM4\" or \"AUX\" for serial printers.  You can also set it
to a name of a file, in which case the output gets appended
to that file.  \(Note that `ps-print' package already has
facilities for printing to a file, so you might as well use
them instead of changing the setting of this variable.\)  If
you want to silently discard the printed output, set this to \"NUL\".

If the value is anything but a string, PostScript files will be
piped to the program given by `ps-lpr-command', with switches
given by `ps-lpr-switches', which see."
  :type '(choice file (other :tag "Pipe to ps-lpr-command" pipe))
  :group 'dos-fns)

;;; dos-vars.el ends here
