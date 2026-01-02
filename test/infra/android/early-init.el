;;; Suppress deletion of the initial frame by `frame-initialize'.  -*- lexical-binding:t -*-
;; $Id: early-init.el,v 1.2 2025/03/02 11:18:42 jw Exp $

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

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



(advice-add 'frame-initialize :around
            (lambda (oldfun &rest args)
              (let ((subr (symbol-function 'delete-frame))
                    (terminal-frame terminal-frame))
                (unwind-protect
                    (progn
                      (fset 'delete-frame #'ignore)
                      (apply oldfun args))
                  (fset 'delete-frame subr)))))

;; Local Variables:
;; no-byte-compile: t
;; End:
