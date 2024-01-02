;;; ps-def.el --- Emacs definitions for ps-print -*- lexical-binding: t -*-

;; Copyright (C) 2007-2024 Free Software Foundation, Inc.

;; Author: Vinicius Jose Latorre <viniciusjl.gnu@gmail.com>
;;	Kenichi Handa <handa@gnu.org> (multi-byte characters)
;; Keywords: wp, print, PostScript
;; URL: https://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre
;; Package: ps-print
;; Obsolete-since: 29.1

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; See ps-print.el for documentation.

;;; Code:


;; Emacs Definitions

(defun ps-mark-active-p ()
  (declare (obsolete mark-active "29.1"))
  mark-active)

(defun ps-face-foreground-name (face)
  (declare (obsolete face-foreground "29.1"))
  (face-foreground face nil t))

(defun ps-face-background-name (face)
  (declare (obsolete face-background "29.1"))
  (face-background face nil t))

(define-obsolete-function-alias 'ps-frame-parameter #'frame-parameter "28.1")
(define-obsolete-function-alias 'ps-color-device #'display-color-p "29.1")
(define-obsolete-function-alias 'ps-color-values #'color-values "28.1")

(provide 'ps-def)

;;; ps-def.el ends here
