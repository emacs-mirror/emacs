;;; t-mouse.el --- mouse support within the text terminal

;; Author: Nick Roberts <nickrob@gnu.org>
;; Maintainer: FSF
;; Keywords: mouse gpm linux

;; Copyright (C) 1994, 1995, 1998, 2006, 2007 Free Software Foundation, Inc.

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

;; This package provides access to mouse event as reported by the gpm-Linux
;; package. It tries to reproduce the functionality offered by Emacs under X.
;; The "gpm" server runs under Linux, so this package is rather
;; Linux-dependent.

;; The file, t-mouse was originally written by Alessandro Rubini and Ian T
;; Zimmerman and communicated with Emacs through the client program mev.  Now
;; the interface with gpm is directly through a Unix socket, so this file is
;; reduced to a minor mode macro call.

;; 

;;; Code:

;;;###autoload
(define-minor-mode t-mouse-mode
  "Toggle t-mouse mode to use the mouse in Linux consoles.
With prefix arg, turn t-mouse mode on iff arg is positive.

This allows the use of the mouse when operating on a Linux console, in the
same way as you can use the mouse under X11.
It requires the `mev' program, part of the `gpm' utilities."
  :global t :group 'mouse
  (if window-system
      (error "t-mouse only works in the console on GNU/Linux")
    (if t-mouse-mode
	(progn
	  (unless (fboundp 'term-open-connection)
	    (error "Emacs must be built with Gpm to use this mode"))
	  (unless (term-open-connection)
	    (error "Can't open mouse connection")))
      ;; Turn it off
      (term-close-connection))))

(provide 't-mouse)

;; arch-tag: a63163b3-bfbe-4eb2-ab4f-201cd164b05d
;;; t-mouse.el ends here
