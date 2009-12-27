;;; cit-dist.el ---
;;
;; Copyright (C) 2009 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cit-dist.el,v 1.2 2009-12-27 03:37:04 zappo Exp $
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Create a distribution, unpack it, and compile it somewhere.

;;; Code:

(defun cit-make-dist ()
  "Create a distribution, and test that it exists."
  ;; 6.a) Create the distribution
  (ede-make-dist)
  (cit-wait-for-compilation)

  ;; Get the version number, then check for that file to exist.
  (when (not (file-exists-p "CEDET_Integ_Test_Project-1.0.tar.gz"))
    (error "Failed to create expected .tar.gz file."))

  ;; 6.b) update the version number
  (cit-update-version)

  ;; 6.c) make a new dist.  Verify version number.
  (ede-make-dist)
  (cit-wait-for-compilation)

  ;; Get the version number, then check for that file to exist.
  (when (not (file-exists-p "CEDET_Integ_Test_Project-2.1.tar.gz"))
    (error "Failed to create expected .tar.gz file."))

  ;; @TODO - test extraction and build somewhere else.
  ;; 6.d)

  )


(defun cit-update-version ()
  "Update the version number of the project.  Verify code changes."
  ;; 6.b) update the version number.
  (ede-update-version "2.1")

  ;; Check it.
  (let ((ver (oref (ede-toplevel) :version)))
    (when (not (string= "2.1" ver))
      (error "Version number did not update correctly.")))

  ;; Force a rebuild with the new version number
  (cit-compile-and-wait " Makefile")

  )

(provide 'cit-dist)

;;; cit-dist.el ends here
