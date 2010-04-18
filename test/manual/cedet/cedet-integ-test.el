;;; cedet-integ-test.el --- CEDET full integration tests.

;; Copyright (C) 2008, 2009, 2010 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>

(eval-and-compile
  ;; Other package depend on this value at compile time via inversion.

  (defvar cit-version "1.0"
    "Current version of Semantic.")

  )
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
;; Note: Not all entries below are actually tested.  Make in comments the
;; bit a new piece of code implements.
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
;;    g.1 Run a program build by EDE
;;    e remove files from a project.
;;    f shared libraries from EDE.
;;
;; 2) Build sources using SRecode.
;;    a Fill in the constructed C files with classes and methods.
;;    b Test various templates
;;    c Use a template to build some C++ templates
;;    d SRecode to load the new template and construct some sources.
;;    e SRecode map update for local templates.
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
;;
;; 5) Use COGRE to build sources with SRecode
;;    a Create a COGRE graph.
;;    b Generate C++ code from the graph.
;;    c Compile the sources.
;;
;; 6) Symref
;;    a Attempt to use symref to create a results list.
;;    b open/close entries in symref
;;    c Use 'rename' command to rename and rebuild.
;;    d See if GNU Global is installed
;;    e repeat above.
;;
;; 7) Create a distribution file.
;;    a Call "make dist"
;;    b update the version number
;;    c make a new dist.  Verify version number.
;;    d In a fresh dir, unpack the dist.
;;    e Compile that dist.

(require 'semantic)
(require 'ede)
(require 'data-debug)
(require 'ede-make)
(require 'cogre)

(eval-and-compile
  (defvar cedet-integ-base "/tmp/CEDET_INTEG"
    "Root of multiple project integration tests.")
  )

(require 'cit-cpp)
(require 'cit-symref)
(require 'cit-uml)
(require 'cit-srec)
(require 'cit-el)
(require 'cit-texi)
(require 'cit-gnustep)
(require 'cit-dist)

(defvar cedet-integ-target (expand-file-name "edeproj" cedet-integ-base)
  "Root of the EDE project integration tests.")

;;; Code:
(defun cedet-integ-test-Make ()
  "Run the full CEDET integration test using a Make style project."
  (cedet-integ-test-proj "Make"))

(defun cedet-integ-test-Automake ()
  "Run the full CEDET integration test using a Automake style project."
  (let ((ede-pconf-create-file-query nil))
    (cedet-integ-test-proj "Automake")))

(defun cedet-integ-test-proj (&optional make-type)
  "Run the full CEDET integration test.
Optional argument MAKE-TYPE is the style of EDE project to test."
  (interactive)

  ;; Input check
  (if (not (member make-type '("Make" "Automake")))
      (error "Invalid make-type for test: %S" make-type))
  (message "Running integration test of style %S" make-type)

  ;; Setup EDE to be turned on.
  (let ((ede-auto-add-method 'never))
    (global-ede-mode 1)

    ;; 1 a) build directories
    ;;
    (cit-make-dir cedet-integ-base)
    (cit-make-dir cedet-integ-target)
    ;; 1 c) make src and include directories
    (cit-make-dir (cit-file "src"))
    (cit-make-dir (cit-file "lib"))
    (cit-make-dir (cit-file "include"))
    (cit-make-dir (cit-file "uml"))
    ;;
    ;; 1 b) make a toplevel project
    ;;
    (find-file (expand-file-name "README" cedet-integ-target))
    (ede-new make-type "CEDET_Integ_Test_Project")
    ;; 1 d) Put C++ src into the right directories.
    ;; 2 a) Create sources with SRecode
    ;;
    (cit-srecode-fill-cpp make-type)

    ;; 5 a,b,c) UML code generation test
    (cit-fill-uml make-type)

    ;; 1 e) remove files from a project
    (cit-remove-add-to-project-cpp)

    ;; 1 f) remove files from a project
    (when (string= make-type "Automake")  ;; Only works w/ Automake
      (cit-remove-and-do-shared-lib make-type))

    ;; 6) Test symref/refactoring tools
    (cit-symref)

    ;; 2 e) srecode map manipulation
    (cit-srecode-map-test)

    ;; Do some more with Emacs Lisp.
    (cit-srecode-fill-el make-type)

    ;; Do some texinfo documentation.
    (cit-srecode-fill-texi)

    ;; Create a distribution
    (find-file (expand-file-name "README" cedet-integ-target))
    (cit-make-dist)

    (cit-finish-message "PASSED" make-type)
    ))

(defun cedet-integ-test-GNUStep ()
  "Run the CEDET integration test using GNUStep style project."
  (interactive)

  ;; Do a EDE GNUstep-Make Project
  (make-directory (concat cedet-integ-target "_ede_GSMake") t)
  (find-file (expand-file-name "README" (concat cedet-integ-target "_ede_GSMake"))) ;; only to change dir
  (let ((ede-auto-add-method 'always))
    (cit-ede-step-test))

  (cit-finish-message "PASSED" "GNUStep")
  )

(defun cit-finish-message (message style)
  "Display a MESSAGE that some test is now finished.
Argument STYLE is the type of build done."
  (let ((b (set-buffer (get-buffer-create "*PASSED*"))))
    (erase-buffer)
    (insert "\n\n  PASSED!\n\n  Make Style: ")
    (insert (format "%S" style) "\n")
    (insert "\n\nWaiting 5 seconds before exiting with positive exit status.\n")
    (switch-to-buffer b)
    ;; Now wait.
    (sit-for 5)
    ;; 1 means GOOD to the shell script, since any other emacs exit
    ;; mechanism will be 0. (ie - click on the X in the corner.)
    (kill-emacs 1)
    ))

(defun cit-make-dir (dir)
  "Make directory DIR if it doesn't exist."
  (when (not (file-exists-p dir))
    (make-directory dir)))

(defun cit-file (filename)
  "Return a testing filename.
Append FILENAME to the target directory."
  (expand-file-name filename cedet-integ-target))

(defun cit-srecode-fill-with-stuff (filename tags &rest
					     empty-dict-entries)
  "Fill up FILENAME with some TAGS.
Argument FILENAME is the file to fill up.
Argument TAGS is the list of tags to insert into FILENAME.
EMPTY-DICT-ENTRIES are dictionary entries for the EMPTY fill macro."
  (let ((post-empty-tags nil)
	)

    ;;
    ;; Fill up foo.h, header file with class in it.
    ;;
    (find-file (cit-file filename))
    (srecode-load-tables-for-mode major-mode)
    (condition-case nil
	;; Protect against a font-lock bug.
	(erase-buffer)
      (error nil))
    (apply 'srecode-insert "file:empty" empty-dict-entries)

    (save-excursion
      (goto-char (point-max))
      (insert "\n\n"))

    ;; 3 a) Parse the sources
    (setq post-empty-tags (semantic-fetch-tags))

    (sit-for 0)
    ;;
    ;; Add in our tags
    ;;
    (dolist (tag tags)

      ;; 3 b) Srecode to make more sources
      ;; 3 c) Test incremental parsers (by side-effect)
      (let ((e (srecode-semantic-insert-tag tag))
	    (code (semantic-tag-get-attribute tag :code)))

	(when code (insert code))

	(goto-char e)
	(sit-for 0)
	)
      )

    (save-buffer)

    ;; Make sure the tags we have are the same as the tags we tried
    ;; to insert.
    (cit-srecode-verify-tags (semantic-fetch-tags)
			     tags
			     post-empty-tags)


    ))

(defclass cit-tag-verify-error-debug ()
  ((actual :initarg :actual
	   :documentation
	   "The actual value found in the buffer.")
   (expected :initarg :expected
	     :documentation
	     "The expected value found in the buffer.")
   )
  "Debugging object for cit tag verifier.")

(defun cit-srecode-verify-tags (actual expected &optional extra)
  "Make sure the ACTUAL tags found in a buffer match those in EXPECTED.
EXTRA tags might also be in the list, so don't fail if any tags in EXTRA
are found, but don't error if they are not their."
  (while actual

    (let ((T1 (car actual))
	  (T2 (car expected)))

      (cond
       ((semantic-tag-similar-p T1 T2 :default-value)

	(let ((mem1 (semantic-tag-components T1))
	      (mem2 (semantic-tag-components T2)))

	  (when (and (or mem1 mem2)
		     (semantic-tag-p (car mem1)))
	    (cit-srecode-verify-tags mem1 mem2))

	  (setq expected (cdr expected)))
	)

	;;it might be in a list of extra tags???
       ((semantic-tag-similar-p T1 (car extra) :default-value)

	;; Don't check members.  These should be simple cases for now.
	(setq extra (cdr extra))
	)

       (t ;; Not the same
	(data-debug-new-buffer "*Test Failure*")
	(data-debug-insert-thing
	 (cit-tag-verify-error-debug "Dbg" :actual T1 :expected T2)
	 ">" "")

	(error "Tag %s does not match %s"
	       (semantic-format-tag-name T1)
	       (semantic-format-tag-name T2))
	)
       ))

    (setq actual (cdr actual))
    ))

(defun cit-compile-and-wait (&optional ARGS)
  "Compile our current project, but wait for it to finish.
Optional ARGS are additional arguments to add to the compile command,
such as 'clean'."
  (let ((bufftokill (find-file (cit-file "Project.ede"))))
    ;; 1 f) Create a build file.
    (ede-proj-regenerate)
    ;; 1 g) build the sources.
    (compile (concat ede-make-command (or ARGS "")))

    (cit-wait-for-compilation)
    (cit-check-compilation-for-error)

    (kill-buffer bufftokill)
    ))

(defun cit-wait-for-compilation ()
  "Wait for a compilation to finish."
  (while compilation-in-progress
    (accept-process-output)
    ;; If sit for indicates that input is waiting, then
    ;; read and discard whatever it is that is going on.
    (when (not (sit-for 1))
      (read-event nil nil .1)
      ))
  )

(defun cit-check-compilation-for-error (&optional inverse)
  "Error if the compilation buffer has errors in it.
If optional INVERSE is non-nil, then throw an error if the
compilation succeeded."
  (save-excursion
    (set-buffer "*compilation*")
    (goto-char (point-max))

    (if (re-search-backward "Compilation exited abnormally " nil t)
	(when (not inverse)
	  (error "Compilation failed!"))
      (when inverse
	(error "Compilation succeeded erroneously!"))
      )))

(defun cit-run-target (command)
  "Run the program (or whatever) that is associated w/ the current target.
Use COMMAND to run the program."
  (let ((target ede-object)
	(cnt 0))
    ;; Run the target.
    (project-run-target target command)
    ;; Did it produce errors or anything?
    (save-excursion
      (set-buffer (ede-shell-buffer target))
      (goto-char (point-min))
      ;; Wait for prompt.
      (unwind-protect
	  (while (not (re-search-forward "MOOSE" nil t))
	    (setq cnt (1+ cnt))
	    (when (> cnt 10) (error "Program output not detected"))
	    (sit-for .1))
	;; Show program output
	(sit-for .2)
	)
      )))

(provide 'cedet-integ-test)
;;; cedet-integ-test.el ends here
