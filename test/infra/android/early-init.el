;;; Suppress deletion of the initial frame by `frame-initialize'.  -*- lexical-binding:t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.



(message "Loading early-init.el...")

(advice-add 'frame-initialize :around
            (lambda (oldfun &rest args)
              (let ((subr (symbol-function 'delete-frame))
                    (terminal-frame terminal-frame))
                (unwind-protect
                    (progn
                      (message "Suppressed deletion of the initial frame.")
                      (fset 'delete-frame #'ignore)
                      (apply oldfun args))
                  (fset 'delete-frame subr)))))
