;;; t-mouse.el --- mouse support within the text terminal

;; Author: Nick Roberts <nickrob@gnu.org>
;; Maintainer: FSF
;; Keywords: mouse gpm linux

;; Copyright (C) 1994, 1995, 1998, 2006, 2007, 2008
;;   Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; The file, t-mouse.el was originally written by Alessandro Rubini and Ian T
;; Zimmerman, and Emacs communicated with gpm through a client program called
;; mev.  Now the interface with gpm is directly through a Unix socket, so this
;; file is reduced to a single minor mode macro call.

;; 

;;; Code:

;; Prevent warning when compiling in an Emacs without gpm support.
(declare-function gpm-mouse-start "term.c" ())

;;;###autoload
(define-obsolete-function-alias 't-mouse-mode 'gpm-mouse-mode "23.1")
;;;###autoload
(define-minor-mode gpm-mouse-mode
  "Toggle gpm-mouse mode to use the mouse in GNU/Linux consoles.
With prefix arg, turn gpm-mouse mode on if arg is positive,
otherwise turn it off.

This allows the use of the mouse when operating on a GNU/Linux console,
in the same way as you can use the mouse under X11.
It relies on the `gpm' daemon being activated."
  :global t :group 'mouse
  (let ((activated nil))
    (unwind-protect
        (progn
          (unless (fboundp 'gpm-mouse-start)
            (error "Emacs must be built with Gpm to use this mode"))
          (when gpm-mouse-mode
            (gpm-mouse-start)
            (setq activated t)))
      ;; If the user asked to turn it off do that.
      ;; If something failed to turn it on, try to turn it off as well,
      ;; just in case.
      (when (and (fboundp 'gpm-mouse-stop) (not activated))
        (setq gpm-mouse-mode nil)
        (gpm-mouse-stop)))))

(provide 't-mouse)

;; arch-tag: a63163b3-bfbe-4eb2-ab4f-201cd164b05d
;;; t-mouse.el ends here
