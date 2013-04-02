;;; cit-javaroot.el --- Integration tests for java root
;;
;; Copyright (C) 2013 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Integration test for the ede/java-root project type.

(require 'semantic/db-javap)

;;; Code:
(defvar cit-integ-target-javaroot
  (expand-file-name "edeproj_ede_javaroot" cedet-integ-base)
  "Root of the EDE project integration tests for the ede/java-root project.")

(defvar cit-integ-javaroot-subdir "integ_src/javaroot/"
  "Directory of files to copy into the tmp project dir.")

(defvar cit-integ-javaroot-sys-subdir "integ_src/fauxsyslib/"
  "Directory of files to copy into the tmp project dir.")

(defvar cit-integ-javaroot-srcdir
  (expand-file-name
   cit-integ-javaroot-subdir
   (file-name-directory (locate-library "cit-javaroot")))
  "The source directory for the JAVA root sources.")

(defvar cit-integ-javaroot-sys-subdir "integ_src/fauxsyslib/"
  "Directory of files to copy into the tmp project dir.")

(defvar cit-integ-javaroot-sys-srcdir
  (expand-file-name
   cit-integ-javaroot-sys-subdir
   (file-name-directory (locate-library "cit-javaroot")))
  "The source directory dor the JAVA root sources.")

(defvar cit-src-javaroot-main-tags
  (list
   (semantic-tag-new-include "TestLib" nil)
   (semantic-tag-new-include "test.TestTest" nil)
   (semantic-tag-new-include "testproj.TestInJar" nil)
   (semantic-tag-new-include "syslib.TestSysJar" nil)
   (semantic-tag-new-include "java.io.InputStream" nil)

   (semantic-tag-new-type
    "TestMain" "class"
    (list
     (semantic-tag-new-function
      "main" "void"
      (list (semantic-tag-new-variable "args" "String" nil
				       :dereference 1))
      :typemodifiers '("public" "static"))
      )
    nil ;; parents
    :typemodifiers '("public"))
   )
  "List of tags we need to be able to to find in main.java")

(defvar cit-javaroot-depfiles
  (list
   (list 'semanticdb-table (expand-file-name "TestLib.java" cit-integ-target-javaroot))
   (list 'semanticdb-table (expand-file-name "test/TestTest.java" cit-integ-target-javaroot))
   (list 'semanticdb-table-jar-file "testproj/TestInJar.class")
   (list 'semanticdb-table-jar-file "syslib/TestSysJar.class")
   (list 'semanticdb-table-jar-file "java/io/InputStream.class")
   ;;(expand-file-name "TestLib.h" cit-integ-javaroot-sys-srcdir)
   )
  "List of expected path names to include files found in TestMain.java")

(defun cit-ede-javaroot-test ()
  "Test the ede-java-root project type."
  ;; Create directory for the project
  (cit-make-dir cit-integ-target-javaroot)

  ;; Copy source files into the javaroot directory
  (condition-case nil
      ;; Emacs 24.2
	(copy-directory (file-name-as-directory cit-integ-javaroot-srcdir) cit-integ-target-javaroot t t t)

    ;; Emacs 23
    (error
     (copy-directory (file-name-as-directory cit-integ-javaroot-srcdir) cit-integ-target-javaroot t t)))

  ;; Create the ede-java-root-project class directly.
  (ede-java-root-project
   "TESTJAVAROOT"
   :file (expand-file-name "TestMain.java" cit-integ-target-javaroot)
   :localclasspath '( "/projjar/TestProj.jar" )
   :classpath (list (expand-file-name "TestSys.jar" cit-integ-javaroot-sys-srcdir))
   )

  ;; Load up main
  (find-file (cit-file-javaroot "TestMain.java"))

  ;; Did the parse work at all???
  (when (not (semantic-fetch-tags))
    (semantic-c-describe-environment)
    (error "TestMain.java failed to parse."))

  ;; Validate found tags based on project provided macros.
  (cit-srecode-verify-tags (semantic-fetch-tags) cit-src-javaroot-main-tags)

  ;; Test out the include paths by checking the discovered file names for the includes.
  (let ((itag (semantic-find-tags-included (current-buffer)))
	(expected cit-javaroot-depfiles))
    (while (and itag expected)

      (let* ((db (semanticdb-find-table-for-include (car itag)))
	     (fn (if db (if (slot-exists-p db 'filename)
			    (oref db :filename)
			  (semanticdb-full-filename db))
		   "<no db>")))

	(when (or (not (object-of-class-p db (nth 0 (car expected))))
		  (not (string= fn (nth 1 (car expected)))))

	  (error "Tag: %s found in db %s fname %s, expected db %s fname %s"
		 (semantic-format-tag-name (car itag))
		 (if db (object-class db) "<no DB>")
		 fn
		 (nth 0 (car expected))
		 (nth 1 (car expected)))
	  ))

      (setq itag (cdr itag)
	    expected (cdr expected)))
    (when (or itag expected)
      (error "Number of found include tags does not match number of expected tags.")))
  )

(defun cit-file-javaroot (filename)
  "Return a testing filename.
Append FILENAME to the target directory."
  (expand-file-name filename cit-integ-target-javaroot))

(provide 'cit-javaroot)

;;; cit-javaroot.el ends here
