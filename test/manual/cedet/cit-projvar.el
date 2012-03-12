;;; cit-projvar.el --- Test project local variables
;;
;; Copyright (C) 2012 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
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
;; Test `ede-set' and that it will correctly set variables in project buffers, but not other
;; buffers.

(defvar cit-test-moose nil)
(defvar cit-test-list nil)

;;; Code:
(defun cit-proj-variables ()
  "Test project local variables."
  ;; Pre-load a file from the project
  (find-file (cit-file "include/foo.hpp"))

  ;; Make some misc file we created and added to a project local.
  (find-file (cit-file "src/main.cpp"))

  ;; Add some "unique" variables to the EDE project
  (ede-set 'cit-test-moose "MOOSE")
  (ede-set 'cit-test-list '(1 2 3))

  ;; Make sure the variables get immediately set to this buffer.
  (cit-projvar-desired-vars-test)

  ;; Make sure they are set in an open, but not current buffer.
  (set-buffer "foo.hpp")
  (cit-projvar-desired-vars-test)

  ;; Make sure they are set when we open a file.
  (kill-buffer (current-buffer))
  (find-file (cit-file "include/foo.hpp"))
  (cit-projvar-desired-vars-test)

  ;; Make sure they are NOT set is some file not belonging to our project
  (find-file (cit-file "../NOT_A_FILE"))
  (cit-projvar-desired-vars-test t)

  ;; Lets flip back to main.cpp - lets set a variable even MORE local
  (find-file (cit-file "src/main.cpp"))
  (ede-set 'cit-test-moose "moose" (ede-current-project))

  (when (not (and (stringp cit-test-moose)
		  (string= cit-test-moose "moose")))
    (error "ede-set failed to create a subproject string variable."))

  ;; I hope it didn't set things over here in foo.hpp
  (find-file (cit-file "include/foo.hpp"))
  (when (not (and (stringp cit-test-moose)
		  (string= cit-test-moose "MOOSE")))
    (error "ede-set failed to restrict a subproject string variable to the subproject."))

  )

(defun cit-projvar-desired-vars-test (&optional notset)
  "Test that the created variables are all property set.
If optional NOTSET, then make sure that they DONT have the desired values."

  (if notset

      (when (or cit-test-moose cit-test-list)
	(error "ede-set set variables where they don't belong."))

    ;; Test positively
    (when (not (and (stringp cit-test-moose)
		    (string= cit-test-moose "MOOSE")))
      (error "ede-set failed to create a string variable."))

    (when (not (and (listp cit-test-list)
		    (equal cit-test-list '(1 2 3))))
      (error "ede-set failed to create a list variable."))

    ))

(provide 'cit-projvar)

;;; cit-projvar.el ends here
