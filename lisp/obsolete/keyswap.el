;;; keyswap.el --- swap BS and DEL keys -*- no-byte-compile: t -*-

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Keywords: terminals

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

;;; arch-tag: 67cf7009-e23e-421c-9648-078e7277297c
;;; keyswap.el ends here
