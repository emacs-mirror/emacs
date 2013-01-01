;;; cit-cpproot.el --- Test ede/cpp-root project features
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
;; The ede/cpp-root project type has features designed to overlay on top
;; of any project that is built out of a significant portion of C++
;; code.
;;
;; In particular, ede/cpp-root projects support preprocessor symbols,
;; and files of preprocessor symbols whos features need to be tested.

;;; Code:
(require 'cedet-android)
(require 'ede/android)

;;; Setup
(defvar cit-integ-target-cpproot
  (expand-file-name "edeproj_ede_cpproot" cedet-integ-base)
  "Root of the EDE project integration tests for the ede/cpp-root project.")

(defvar cit-integ-cpproot-subdir "integ_src/cpproot/"
  "Directory of files to copy into the tmp project dir.")

(defvar cit-integ-cpproot-srcdir
  (expand-file-name
   cit-integ-cpproot-subdir
   (file-name-directory (locate-library "cit-cpproot")))
  "The source directory dor the CPP root sources.")

(defvar cit-src-cpproot-main-tags
  (list
   (semantic-tag-new-include "sppmacros.h" nil)
   (semantic-tag-new-include "test.h" nil)
   (semantic-tag-new-function
    "main" "int"
    (list (semantic-tag-new-variable "argc" "int")
	  (semantic-tag-new-variable "argv" "char"
				     nil
				     :pointer 1
				     :dereference 1)))
   (semantic-tag-new-function "feature1" "int" nil)
   (semantic-tag-new-function "feature2" "int" nil)
   (semantic-tag-new-function "feature3" "int" nil)
   (semantic-tag-new-function "generic_feature" "int" nil)
   )
  "List of tags we need to be able to to find in main.cpp")


(defun cit-ede-cpproot-test ()
  "Test EDE cpproot based Project."

  ;; Create directory for the project
  (cit-make-dir cit-integ-target-cpproot)

  ;; Copy source files into the cpproot directory
  (copy-directory cit-integ-cpproot-srcdir cit-integ-target-cpproot t t)

  ;; Create the ede-cpp-root-project class directly.
  (ede-cpp-root-project
   "TESTCPPROOT"
   :file (expand-file-name "main.cpp" cit-integ-target-cpproot)
   :spp-table '( ("FEATURE3" . "1")
		 ("RANDOM" . "random") )
   :spp-files '( "sppmacros.h" ))

  ;; Load up main
  (find-file (cit-file-cpproot "main.cpp"))

  ;; Validate found tags based on project provided macros.
  (cit-srecode-verify-tags (semantic-fetch-tags)
			   cit-src-cpproot-main-tags)

  )


(defun cit-file-cpproot (filename)
  "Return a testing filename.
Append FILENAME to the target directory."
  (expand-file-name filename cit-integ-target-cpproot))

(provide 'cit-cpproot)

;;; cit-android.el ends here
