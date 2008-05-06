;;; keyswap.el --- swap BS and DEL keys -*- no-byte-compile: t -*-

;; Copyright (C) 1992, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Keywords: terminals

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file has been obsolete since Emacs 22.1.

;; This package is meant to be called by other terminal packages.

;;; Code:

(let ((the-table (make-string 128 0)))
  (let ((i 0))
    (while (< i 128)
      (aset the-table i i)
      (setq i (1+ i))))
  ;; Swap ^H and DEL
  (aset the-table ?\177 ?\^h)
  (aset the-table ?\^h ?\177)
  (setq keyboard-translate-table the-table))

;; arch-tag: 67cf7009-e23e-421c-9648-078e7277297c
;;; keyswap.el ends here
