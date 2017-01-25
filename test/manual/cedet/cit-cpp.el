;;; cit-cpp.el --- C++ specific things for our integ test.

;; Copyright (C) 2008, 2009, 2010, 2012 Eric M. Ludlam

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
;; C++ specific code for the cedet integration tests.
;; Add some C code too!

;;; Code:

(defvar cit-header-cpp-tags
  (list
   (semantic-tag-new-type
    "foo" "class"
    (list
     (semantic-tag "public" 'label)
     (semantic-tag-new-function
      "foo" '("foo" type (:type "class"))
      (list (semantic-tag-new-variable "f" "int"))
      :constructor-flag t
      :code " Field1 = f; ")
     (semantic-tag-new-function
      "foo" "void" nil :destructor-flag t )
     (semantic-tag-new-function
      "doSomethingPublic" "void"
      (list (semantic-tag-new-variable "ctxt" "int")
	    (semantic-tag-new-variable "thing" "char"
				       nil
				       :pointer 1))
      :prototype-flag t)
     (semantic-tag-new-function
      "setField1" "void"
      (list (semantic-tag-new-variable "f" "int"))
      :prototype-flag t)
     (semantic-tag-new-function
      "getField1" "int" nil
      :prototype-flag t)
     (semantic-tag "protected" 'label)
     (semantic-tag-new-function
      "doSomethingProtected" "void"
      (list (semantic-tag-new-variable "ctxt" "int")
	    (semantic-tag-new-variable "thing" "char"
				       nil
				       :pointer 1))
      :prototype-flag t)
     (semantic-tag "private" 'label)
     (semantic-tag-new-variable
      "Field1" "int")
     )
    nil)
   )
  "Tags to be inserted into a header file.")

(defvar cit-src-cpp-tags
  (list
   (semantic-tag-new-include "stdio.h" nil)
   (semantic-tag-new-include "foo.hpp" nil)
   (semantic-tag-new-function
    "doSomethingPublic" "void"
    (list (semantic-tag-new-variable "ctxt" "int")
	  (semantic-tag-new-variable "thing" "char"
				     nil
				     :pointer 1))
    :parent "foo"
    :code "   setField1(1);
   if(getField1() == 1) {
      // Call doSomethingProtected <== multi-hit for grep, not for global.
      doSomethingProtected(ctxt,thing);
   }\n")
   (semantic-tag-new-function
    "setField1" "void"
    (list (semantic-tag-new-variable "f" "int"))
    :parent "foo"
    :code "   Field1 = f;\n")
   (semantic-tag-new-function
    "getField1" "int" nil
    :parent "foo"
    :code "   return Field1;\n")
   (semantic-tag-new-function
    "doSomethingProtected" "void"
    (list (semantic-tag-new-variable "ctxt" "int")
	  (semantic-tag-new-variable "thing" "char"
				     nil
				     :pointer 1))
    :parent "foo"
    :code "   printf(\"%s\\n\",thing);\n")
   )
  "Tags to be inserted into a source file.")

(defvar cit-plain-c-tags
  (list
   (semantic-tag-new-include "string.h" nil)
   (semantic-tag-new-function
    "main" "int"
    (list (semantic-tag-new-variable "argc" "int")
	  (semantic-tag-new-variable "argv" "char"
				     nil
				     :pointer 2 ))
    :code "   int myInt = 0;
   char *myStr = strdup(\"MOOSE\");
")
   )
  "Tags to be inserted into plain.c.")

(defvar cit-main-cpp-tags
  (list
   (semantic-tag-new-include "foo.hpp" nil)
   (semantic-tag-new-include "string.h" nil)
   (semantic-tag-new-function
    "main" "int"
    (list (semantic-tag-new-variable "argc" "int")
	  (semantic-tag-new-variable "argv" "char"
				     nil
				     :pointer 2 ))
    :code "   foo myFoo(2);
   char *myStr = strdup(\"MOOSE\");
   myFoo.doSomethingPublic(1,myStr);
")
   )
  "Tags to be inserted into main.")

(defvar cit-symref-operations
  '( "doSomethingProtected" "renameSomething" )
  "A set of symref operations based on the tags defined here.
The symref test (in cit-symref.el) will use these operations
to test they symbol reference system in C++.")

(defun cit-srecode-fill-cpp (make-type)
  "Fill up a base set of files with some base tags.
MAKE-TYPE is the type of make process to use."

  ;; 2 b) Test various templates.

  (cit-srecode-fill-with-stuff "include/foo.hpp" cit-header-cpp-tags)
  (ede-new make-type "Includes")
  ;; 1 e) Tell EDE where the srcs are
  (ede-new-target "Includes" "miscellaneous" "n")
  (ede-add-file "Includes")

  (cit-srecode-fill-with-stuff "src/foo.cpp" cit-src-cpp-tags)
  (ede-new make-type "Src")
  ;; 1 e) Tell EDE where the srcs are
  (ede-new-target "Prog" "program" "n")
  (ede-add-file "Prog")

  (cit-srecode-fill-with-stuff "src/main.cpp" cit-main-cpp-tags)
  ;; 1 e) Tell EDE where the srcs are
  (ede-add-file "Prog")

  ;; Have some plain C code too to test the C code generator
  (cit-srecode-fill-with-stuff "src/plain.c" cit-plain-c-tags)
  (ede-new-target "Plain" "program" "n")
  (ede-add-file "Plain")

  (let ((p (ede-current-project)))
    (if (string= make-type "Automake")
	(oset p :variables '( ( "AM_CPPFLAGS" . "-I../include") ))
      (oset p :variables '( ( "CPPFLAGS" . "-I../include") )))
    (ede-commit-project p)
    )

  ;; 1 g) build the sources.
  (cit-compile-and-wait)

  ;; 1 g.1) Run the compiled program.  (Test for MOOSE output.)
  (find-file (cit-file "src/main.cpp"))
  (cit-run-target "./Prog")
  )

(defun cit-remove-add-to-project-cpp ()
  "Remve foo.cpp from the current project.  Add in a new generated file."

  (find-file (cit-file "src/foo.cpp"))
  ;; Whack the file
  (ede-remove-file t)
  (kill-buffer (current-buffer))
  (delete-file (cit-file "src/foo.cpp"))
  (delete-file (cit-file "src/foo.o"))

  ;; Make a new one
  (cit-srecode-fill-with-stuff "src/bar.cpp" cit-src-cpp-tags)
  (ede-add-file "Prog")

  ;; 1 g) build the sources.
  ;; Direct compile to test that make fails properly.
  (compile ede-make-command)

  (cit-wait-for-compilation)
  (cit-check-compilation-for-error t) ;; That should have errored.

  (cit-compile-and-wait)
  )

(defun cit-remove-and-do-shared-lib (make-type)
  "Remove bar.cpp from the current project.
Create a new shared lib with bar.cpp in it.
Argument MAKE-TYPE is the type of make project to create."
  (find-file (cit-file "src/bar.cpp"))
  ;; Whack the file
  (ede-remove-file t)
  (kill-buffer (current-buffer))
  (delete-file (cit-file "src/bar.cpp"))
  (delete-file (cit-file "src/bar.o"))

  ;; Create a new shared lib target, and add bar.cpp to it.
  (find-file (cit-file "lib/bar.cpp"))
  (cit-srecode-fill-with-stuff "lib/bar.cpp" cit-src-cpp-tags)
  (ede-new make-type "Libs")
  (ede-new-target "testlib" "sharedobject" "n")
  (ede-add-file "testlib")
  (let ((mt ede-object))
    (oset mt :compiler 'ede-g++-libtool-shared-compiler)
    (oset mt :linker 'ede-g++-linker-libtool)
    )


  ;; 1 g) build the sources.
  ;; Direct compile to test that make fails properly.
  (compile ede-make-command)

  (cit-wait-for-compilation)
  (cit-check-compilation-for-error t) ;; that should have errored.

  (let ((p (ede-current-project)))
    (if (string= make-type "Automake")
	(oset p :variables '( ( "AM_CPPFLAGS" . "-I../include") ))
      (oset p :variables '( ( "CPPFLAGS" . "-I../include") )))
    (ede-commit-project p)
    )

  ;; Flip back over to main, and add our new testlib.

  (let* ((buffer (find-file (cit-file "src/main.cpp")))
	 (mt ede-object))
    (if (string= make-type "Automake")
	(progn
	  (oset mt :ldflags '("-L../lib"))
	  (oset mt :ldlibs '("testlib")))
      ;; FIX THIS
      (oset mt :ldflags '("../lib/bar.o"));;HACK for libtool!
      )
    (cit-compile-and-wait)

    ;; Use the local libs version also to make sure it works.
    (pop-to-buffer buffer))
  (let ((mt ede-object))
    (if (string= make-type "Automake")
	(progn
	  (oset mt :ldlibs-local '("../lib/libtestlib.la"))
	  (oset mt :ldflags nil)
	  (oset mt :ldlibs nil))
      ;; FIX THIS
      (oset mt :ldflags '("../lib/bar.o"));;HACK for libtool!
      ))
  (cit-compile-and-wait)
  )

(provide 'cit-cpp)
;;; cit-cpp.el ends here
