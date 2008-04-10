;;; cdl.el --- Common Data Language (CDL) utility functions for GNU Emacs

;; Copyright (C) 1993, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: ATAE@spva.physics.imperial.ac.uk (Ata Etemadi)
;; Maintainer: FSF
;; Keywords: data

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

;;; Code:

(defun cdl-get-file (filename)
  "Run file through ncdump and insert result into buffer after point."
  (interactive "fCDF file: ")
  (message "ncdump in progress...")
  (let ((start (point)))
    (call-process  "ncdump" nil t nil (expand-file-name filename))
    (goto-char start))
  (message "ncdump in progress...done"))

(defun cdl-put-region (filename start end)
  "Run region through ncgen and write results into a file."
  (interactive "FNew CDF file: \nr")
  (message "ncgen in progress...")
  (call-process-region start end "ncgen"
		       nil nil nil "-o" (expand-file-name filename))
  (message "ncgen in progress...done"))

(provide 'cdl)

;; arch-tag: b8e95a6e-2387-4077-ad9a-af54b09b8615
;;; cdl.el ends here
