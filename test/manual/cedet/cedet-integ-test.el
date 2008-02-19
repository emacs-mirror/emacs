;;; cedet-integ-test.el --- CEDET full integration tests.

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;; This file provides a top level integration test harness for
;; the various CEDET tools to do a simple stand-alone test.
;;
;; The below listed parts DO NOT happen in this order as various
;; tools have to work together to build up the project.
;;
;; Parts:
;;
;; 1) Create an EDE project in /tmp
;;    a build directory tree
;;    b make a toplevel project
;;    c Make a src and include directory
;;    d Put C++ src files into the correct directory.
;;    e Tell EDE where they are.
;;    f create a build file.
;;    g build the sources
;;
;; 2) Build sources using SRecode.
;;    a Fill in the constructed C files with classes and methods.
;;    b Test various templates
;;    c Use a template to build some C++ templates
;;    d SRecode to load the new template and construct some sources.
;;
;; 3) Semantic to parse stuff
;;    a Parse the sources
;;    b Use srecode to make more sources
;;    c test the incremental parsers.
;;    d test the completion engine.
;;    e Save semanticdb tables.  Are the files there?
;;
;; 4) Delete the project
;;    a Make sure the semanticdb cleans up the dead cache files.
;;    b Make sure EDE clears this project from it's project cache.

(defvar cedet-integ-target "/tmp/CEDET_INTEG"
  "Root of the integration tests.")

;;; Code:
(defun cedet-integ-test ()
  "Run the full CEDET integration test."
  (interactive)
  ;; 1 a) build directories
  ;;
  (cit-make-dir cedet-integ-target)
  ;; 1 c) make src and include directories
  (cit-make-dir (cit-file "src"))
  (cit-make-dir (cit-file "include"))
  ;;
  ;; 1 b) make a toplevel project
  ;;
  (find-file (expand-file-name "README" cedet-integ-target))
  (ede-new "Make" "CEDET Integ Test Project")
  ;; 1 d) Put C++ src into the right directories.
  ;; 2 a) Create sources with SRecode
  ;;
  (cit-srecode-fill)

  )

(defun cit-make-dir (dir)
  "Make directory DIR if it doesn't exist."
  (when (not (file-exists-p dir))
    (make-directory dir)))

(defun cit-file (filename)
  "Return a testing filename.
Append FILENAME to the target directory."
  (expand-file-name filename cedet-integ-target))

(defvar cit-header-tags
  (list
   (semantic-tag-new-type
    "foo" "class"
    (list
     (semantic-tag "public" 'label)
     (semantic-tag-new-function
      "foo" "" (list (semantic-tag-new-variable "f" "int"))
      :constructor t)
     (semantic-tag-new-function
      "foo" "" nil :destructor t )
     (semantic-tag-new-function
      "doSomethingPublic" "void"
      (list (semantic-tag-new-variable "ctxt" "int")
	    (semantic-tag-new-variable "thing" "char *")))
     (semantic-tag-new-function
      "setField1" "void"
      (list (semantic-tag-new-variable "f" "int"))
      )
     (semantic-tag-new-function
      "getField1" "int" nil )
     (semantic-tag "protected" 'label)
     (semantic-tag-new-function
      "doSomethingProtected" "void"
      (list (semantic-tag-new-variable "ctxt" "int")
	    (semantic-tag-new-variable "thing" "char *")))
     (semantic-tag "private" 'label)
     (semantic-tag-new-variable
      "Field1" "int")
     )
    nil)
   )
  "Tags to be inserted into a header file.")

(defvar cit-src-tags
  (list
   (semantic-tag-new-include "foo.hh" nil)
   (semantic-tag-new-function
    "doSomethingPublic" "void"
    (list (semantic-tag-new-variable "ctxt" "int")
	  (semantic-tag-new-variable "thing" "char *"))
    :parent "foo")
   (semantic-tag-new-function
    "setField1" "void"
    (list (semantic-tag-new-variable "f" "int"))
    :parent "foo")
   (semantic-tag-new-function
    "getField1" "int" nil
    :parent "foo")
   (semantic-tag-new-function
    "doSomethingProtected" "void"
    (list (semantic-tag-new-variable "ctxt" "int")
	  (semantic-tag-new-variable "thing" "char *"))
    :parent "foo")
   )
  "Tags to be inserted into a source file.")

(defvar cit-main-tags
  (list
   (semantic-tag-new-include "foo.hh" nil)
   (semantic-tag-new-function
    "main" "int"
    (list (semantic-tag-new-variable "argc" "int")
	  (semantic-tag-new-variable "argv" "char**")))
   )
  "Tags to be inserted into main.")

(defun cit-srecode-fill ()
  "Fill up a base set of files with some base tags."
  ;; 2 b) Test various templates.

  (cit-srecode-fill-with-stuff "include/foo.hh" cit-header-tags)
  (ede-new "Make" "Includes")
  ;; 1 e) Tell EDE where the srcs are
  (ede-new-target "Includes" "miscelaneous" "n")
  (ede-add-file "Includes")

  (cit-srecode-fill-with-stuff "src/foo.cpp" cit-src-tags)
  (ede-new "Make" "Src")
  ;; 1 e) Tell EDE where the srcs are
  (ede-new-target "Prog" "program" "n")
  (ede-add-file "Prog")

  (cit-srecode-fill-with-stuff "src/main.cpp" cit-main-tags)
  ;; 1 e) Tell EDE where the srcs are
  (ede-add-file "Prog")

  (let ((p (ede-current-project)))
    (oset p :variables '( ( "CPPFLAGS" . "-I../include") ))
    (ede-commit-project p)
    )

  (find-file "../Project.ede")
  ;; 1 f) Create a build file.
  (ede-proj-regenerate)
  ;; 1 g) build the sources.
  (compile "make")
  )

(defun cit-srecode-fill-with-stuff (filename tags)
  "Fill up FILENAME with some TAGS.
Argument FILENAME is the file to fill up.
Argument TAGS is the list of tags to insert into FILENAME."
  (let ((post-empty-tags nil)
	)

    ;;
    ;; Fill up foo.h, header file with class in it.
    ;;
    (find-file (cit-file filename))
    (srecode-load-tables-for-mode major-mode)
    (erase-buffer)
    (srecode-insert "file:empty")

    ;; 3 a) Parse the sources
    (setq post-empty-tags (semantic-fetch-tags))

    ;;
    ;; Add in our tags
    ;;
    (dolist (tag tags)

      (sit-for 0)
      ;; 3 b) Srecode to make more sources
      ;; 3 c) Test incremental parsers (by side-effect)
      (let ((e (srecode-semantic-insert-tag tag)))

	(goto-char e)

	)
      )

    (save-buffer)

    ;; Make sure the tags we have are the same as the tags we tried
    ;; to insert.
    (cit-srecode-verify-tags (semantic-fetch-tags)
			     tags
			     post-empty-tags)


    ))

(defun cit-srecode-verify-tags (actual expected &optional extra)
  "Make sure the ACTUAL tags found in a buffer match those in EXPECTED.
EXTRA tags might also be in the list, so don't fail if any tags in EXTRA
are found, but don't error if they are not their."
  (while actual

    (if (semantic-tag-similar-p (car actual) (car expected))

	(let ((mem1 (semantic-tag-components (car actual)))
	      (mem2 (semantic-tag-components (car expected))))

	  (cit-srecode-verify-tags mem1 mem2)

	  (setq expected (cdr expected)))

      ;; ELSE - it might be in a list of extra tags???

      (when (semantic-tag-similar-p (car actual) (car extra))

	;; Don't check members.  These should be simple cases for now.
	(setq extra (cdr extra))

	)

      )


    (setq actual (cdr actual))))


(provide 'cedet-integ-test)
;;; cedet-integ-test.el ends here
