;;; w32-vars.el --- MS-Windows specific user options

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: Jason Rumney <jasonr@gnu.org>
;; Keywords: internal

;; This file is part of GNU Emacs.

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

;;; Code:

;; Custom group for w32 specific settings
(defgroup w32 nil
  "MS-Windows specific features"
  :group 'environment
  :version "21.3"
  :prefix "w32")

;; mwheel.el should probably be adapted to accept mouse-wheel events
;; then this could go.
(defcustom mouse-wheel-scroll-amount '(5  ((shift) . 1) ((control) . nil))
  "*Amount to scroll windows by when spinning the mouse wheel.
This is actually a cons cell, where the first item is the amount to scroll
on a normal wheel event. The rest is ignored on Windows, see mwheel.el if
you wish to implement modifier keys."
  :group 'w32
  :group 'mouse
  :type '(cons
	  (choice :tag "Normal"
		  (const :tag "Full screen" :value nil)
		  (integer :tag "Specific # of lines")
		  (float :tag "Fraction of window"))
          (repeat
           (cons
            (repeat (choice :tag "modifier" (const alt) (const control) (const hyper)
                            (const meta) (const shift) (const super)))
            (choice :tag "scroll amount"
                    (const :tag "Full screen" :value nil)
                    (integer :tag "Specific # of lines")
                    (float :tag "Fraction of window"))))))

;; Redefine the font selection to use the standard W32 dialog
(defcustom w32-use-w32-font-dialog t
  "*Use the standard font dialog.
If nil, pop up a menu of a fixed set of fonts including fontsets, like
X does.  See `w32-fixed-font-alist' for the font menu definition."
  :type 'boolean
  :group 'w32)

(defcustom w32-list-proportional-fonts nil
  "*Include proportional fonts in the default font dialog."
  :type 'boolean
  :group 'w32)

(defcustom w32-allow-system-shell nil
  "*Disable startup warning when using \"system\" shells."
  :type 'boolean
  :group 'w32)

(defcustom w32-system-shells '("cmd" "cmd.exe" "command" "command.com"
			       "4nt" "4nt.exe" "4dos" "4dos.exe"
			       "ndos" "ndos.exe")
  "*List of strings recognized as Windows NT/9X system shells."
  :type '(repeat string)
  :group 'w32)

;; Want "menu" custom type for this.
(defcustom w32-fixed-font-alist
  '("Font menu"
    ("Misc"
     ;; For these, we specify the pixel height and width.
     ("fixed" "Fixedsys")
     ("")
     ("Terminal 5x4"
      "-*-Terminal-normal-r-*-*-*-45-*-*-c-40-*-oem")
     ("Terminal 6x8"
      "-*-Terminal-normal-r-*-*-*-60-*-*-c-80-*-oem")
     ("Terminal 9x5"
      "-*-Terminal-normal-r-*-*-*-90-*-*-c-50-*-oem")
     ("Terminal 9x7"
      "-*-Terminal-normal-r-*-*-*-90-*-*-c-70-*-oem")
     ("Terminal 9x8"
      "-*-Terminal-normal-r-*-*-*-90-*-*-c-80-*-oem")
     ("Terminal 12x12"
      "-*-Terminal-normal-r-*-*-*-120-*-*-c-120-*-oem")
     ("Terminal 14x10"
      "-*-Terminal-normal-r-*-*-*-135-*-*-c-100-*-oem")
     ("Terminal 6x6 Bold"
      "-*-Terminal-bold-r-*-*-*-60-*-*-c-60-*-oem")
     ("")
     ("Lucida Sans Typewriter.8"
      "-*-Lucida Sans Typewriter-normal-r-*-*-11-*-*-*-c-*-iso8859-1")
     ("Lucida Sans Typewriter.9"
      "-*-Lucida Sans Typewriter-normal-r-*-*-12-*-*-*-c-*-iso8859-1")
     ("Lucida Sans Typewriter.10"
      "-*-Lucida Sans Typewriter-normal-r-*-*-13-*-*-*-c-*-iso8859-1")
     ("Lucida Sans Typewriter.11"
      "-*-Lucida Sans Typewriter-normal-r-*-*-15-*-*-*-c-*-iso8859-1")
     ("Lucida Sans Typewriter.12"
      "-*-Lucida Sans Typewriter-normal-r-*-*-16-*-*-*-c-*-iso8859-1")
     ("Lucida Sans Typewriter.8 Bold"
      "-*-Lucida Sans Typewriter-semibold-r-*-*-11-*-*-*-c-*-iso8859-1")
     ("Lucida Sans Typewriter.9 Bold"
      "-*-Lucida Sans Typewriter-semibold-r-*-*-12-*-*-*-c-*-iso8859-1")
     ("Lucida Sans Typewriter.10 Bold"
      "-*-Lucida Sans Typewriter-semibold-r-*-*-13-*-*-*-c-*-iso8859-1")
     ("Lucida Sans Typewriter.11 Bold"
      "-*-Lucida Sans Typewriter-semibold-r-*-*-15-*-*-*-c-*-iso8859-1")
     ("Lucida Sans Typewriter.12 Bold"
      "-*-Lucida Sans Typewriter-semibold-r-*-*-16-*-*-*-c-*-iso8859-1"))
    ("Courier"
     ("Courier 10x8"
      "-*-Courier-*normal-r-*-*-*-97-*-*-c-80-iso8859-1")
     ("Courier 12x9"
      "-*-Courier-*normal-r-*-*-*-120-*-*-c-90-iso8859-1")
     ("Courier 15x12"
      "-*-Courier-*normal-r-*-*-*-150-*-*-c-120-iso8859-1")
     ;; For these, we specify the point height.
     ("")
     ("8" "-*-Courier New-normal-r-*-*-11-*-*-*-c-*-iso8859-1")
     ("9" "-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-1")
     ("10" "-*-Courier New-normal-r-*-*-13-*-*-*-c-*-iso8859-1")
     ("11" "-*-Courier New-normal-r-*-*-15-*-*-*-c-*-iso8859-1")
     ("12" "-*-Courier New-normal-r-*-*-16-*-*-*-c-*-iso8859-1")
     ("8 bold" "-*-Courier New-bold-r-*-*-11-*-*-*-c-*-iso8859-1")
     ("9 bold" "-*-Courier New-bold-r-*-*-12-*-*-*-c-*-iso8859-1")
     ("10 bold" "-*-Courier New-bold-r-*-*-13-*-*-*-c-*-iso8859-1")
     ("11 bold" "-*-Courier New-bold-r-*-*-15-*-*-*-c-*-iso8859-1")
     ("12 bold" "-*-Courier New-bold-r-*-*-16-*-*-*-c-*-iso8859-1")
     ("8 italic" "-*-Courier New-normal-i-*-*-11-*-*-*-c-*-iso8859-1")
     ("9 italic" "-*-Courier New-normal-i-*-*-12-*-*-*-c-*-iso8859-1")
     ("10 italic" "-*-Courier New-normal-i-*-*-13-*-*-*-c-*-iso8859-1")
     ("11 italic" "-*-Courier New-normal-i-*-*-15-*-*-*-c-*-iso8859-1")
     ("12 italic" "-*-Courier New-normal-i-*-*-16-*-*-*-c-*-iso8859-1")
     ("8 bold italic" "-*-Courier New-bold-i-*-*-11-*-*-*-c-*-iso8859-1")
     ("9 bold italic" "-*-Courier New-bold-i-*-*-12-*-*-*-c-*-iso8859-1")
     ("10 bold italic" "-*-Courier New-bold-i-*-*-13-*-*-*-c-*-iso8859-1")
     ("11 bold italic" "-*-Courier New-bold-i-*-*-15-*-*-*-c-*-iso8859-1")
     ("12 bold italic" "-*-Courier New-bold-i-*-*-16-*-*-*-c-*-iso8859-1")
     ))
    "*Fonts suitable for use in Emacs.
Initially this is a list of some fixed width fonts that most people
will have like Terminal and Courier. These fonts are used in the font
menu if the variable `w32-use-w32-font-dialog' is nil."
    :type '(list
	    (string :tag "Menu Title")
	    (repeat :inline t
	     (list :tag "Submenu"
	      (string :tag "Title")
	      (repeat :inline t
	       (choice :tag ""
		(const :tag "Seperator" (""))
		(list :tag "Font Entry"
		      (string :tag "Menu text")
		      (string :tag "Font"))))))))

(defcustom x-select-enable-clipboard t
  "*Non-nil means cutting and pasting uses the clipboard.
This is in addition to the primary selection."
  :type 'boolean
  :group 'killing)


;;; w32-vars.el ends here
