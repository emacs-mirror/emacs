;;; cit-globalref.el --- Test GNU Global being used for finding references
;;
;; Copyright (C) 2013, 2014 Eric M. Ludlam
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
;; Using srcs by Barry OReilly, test out GNU Global running against
;; the src, then use the symref tool to find a deep tag (A::B::MyFcn)
;; where the namespaces appear all over.  Can we find the right symbol
;; without loading all the crazy code?

;;; Code:

(defvar cit-integ-target-globalref
  (expand-file-name "edeproj_ede_globalref" cedet-integ-base)
  "Root of the EDE project integration tests for the ede/java-root project.")

(defvar cit-integ-globalref-subdir "integ_src/globalref/"
  "Directory of files to copy into the tmp project dir.")

(defvar cit-integ-globalref-srcdir
  (expand-file-name
   cit-integ-globalref-subdir
   (file-name-directory (locate-library "cit-globalref")))
  "The source directory for the JAVA root sources.")



(defun cit-globalref-test ()
  "Test the use of GNU Global and minimal file loading."
  ;; Make sure we can use GNU Global.
  (if (not (cedet-gnu-global-version-check t))
      (error "WARNING: Failed cedet-gnu-global-version-check "))

  ;; Create directory for the project
  (cit-make-dir cit-integ-target-globalref)

  ;; Copy source files into the globalref directory
  (condition-case nil
      ;; Emacs 24.2
	(copy-directory (file-name-as-directory cit-integ-globalref-srcdir) cit-integ-target-globalref t t t)

    ;; Emacs 23
    (error
     (copy-directory (file-name-as-directory cit-integ-globalref-srcdir) cit-integ-target-globalref t t)))

  ;; Create the ede-java-root-project class directly.
  (ede-cpp-root-project
   "TESTGLOBALREF"
   :file (expand-file-name "README" cit-integ-target-globalref)
   :include-path '( "/inc" )
   )

  ;; Bootstrap GNU Global in the test directory
  (cedet-gnu-global-create/update-database
   (file-name-as-directory cit-integ-target-globalref))

  ;; Enable GNU Global Database
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)

  ;; Setup file loading/logging in C files.
  (add-hook 'c-mode-common-hook 'cit-globalref-cmode-hook)

  ;; Hook into the system.
  (find-file (cit-file-globalref "inc/Util.hh"))

  ;; Quick check - basics working???
  (when (not (member "inc/Util.hh" cit-globalref-file-log))
    (error "Basic logging failed.  Skipping globalref test."))

  (setq cit-globalref-file-log nil) ;; Reset

  ;; Do a generic semanticdb search for the symbol w/out prototypes.
  (let ((ans (semanticdb-find-tags-by-name "myUtilFunc"))
	)
    (when (not (= (semanticdb-find-result-length ans) 1))
      (error "Global should have returned only 1 tag for myUtilFunc"))
    ;;(message "Answer: %S" (semanticdb-fast-strip-find-results ans))

    (when (not (= (length cit-globalref-file-log) 1))
      (error "Global should have only loaded 1 file for myUtilFunc"))

    (setq cit-globalref-file-log nil) ;; Reset
    (semantic-symref-cleanup-recent-buffers-fcn) ;; Simulate end of command
    )

  ;; Now do a full search for the impl with proto-impl toggle.
  (goto-char (point-min))
  (re-search-forward "myUtilFun")
  (semantic-analyze-proto-impl-toggle)

  (when (not (string= (buffer-name) "Util.cc"))
    (message "Jumped to: [%S]" (buffer-name))
    (error "proto-impl-toggle failed to find Util.cc"))

  (message "Jump to myUtilFun success.")

  ;; The only file it needed to find
  (when (not (equal cit-globalref-file-log '("test/ManagerTest.cc" "src/Manager.cc" "src/Util.cc")))
    (message "Too many/wrong files searched, should have only found src/Util.cc, test/ManagerTest.cc, and src/Manager.cc")
    (error "Found %S" cit-globalref-file-log))

  ;; Debug reporting.
  (message "Files loaded in globalref test: %S"
	   cit-globalref-file-log)

  (setq cit-globalref-file-log nil) ;; Reset
  (semantic-symref-cleanup-recent-buffers-fcn) ;; Simulate end of command

  ;; Now make sure some of the buffers weren't left hanging around.
  (when (get-file-buffer "test/ManagerTest.cc")
    (error "Buffer left open for test/ManagerTest.cc"))

  ;;; Now Check for the impl for a symbol not in a namespace.
  ;; Now do a full search for the impl with proto-impl toggle.
  (find-file (cit-file-globalref "inc/Util.hh"))
  (goto-char (point-min))
  (re-search-forward "myUtilFuncNoNS")
  (semantic-analyze-proto-impl-toggle)

  (when (not (string= (buffer-name) "Util.cc"))
    (message "Jumped to: [%S]" (buffer-name))
    (error "proto-impl-toggle failed to find Util.cc"))

  (message "Jump to myUtilFunNoNS success.")

  ;; The only file it needed to find
  (when (not (equal cit-globalref-file-log nil)) ;; '("src/Util.cc")))
    (message "Too many files searched, should have found files in buffers.")
    (error "Found %S" cit-globalref-file-log))

  ;; Debug reporting.
  (message "Files loaded in globalref no NS test: %S"
	   cit-globalref-file-log)

  (setq cit-globalref-file-log nil) ;; Reset
  (semantic-symref-cleanup-recent-buffers-fcn) ;; Simulate end of command

  ;; Now make sure some of the buffers weren't left hanging around.
  (when (get-file-buffer "test/Util.cc")
    (error "Buffer left open for test/Util.cc"))

  )

(defvar cit-globalref-file-log nil
  "Log of all the C files pulled into Emacs.")

(defun cit-globalref-cmode-hook ()
  "Hook to run in C mode.
Logs all the C files initailized in Emacs."
  (let ((fname
	 (condition-case nil
	     (if (string-match cit-integ-target-globalref (buffer-file-name))
		 (file-relative-name (buffer-file-name)
				     (file-name-as-directory
				      cit-integ-target-globalref))
	       (buffer-file-name))
	   ;; Use condition case so I can debug from here.
	   (error (buffer-file-name)))))
    (push fname cit-globalref-file-log)))

(defun cit-file-globalref (filename)
  "Return a testing filename.
Append FILENAME to the target directory."
  (expand-file-name filename cit-integ-target-globalref))

(provide 'cit-globalref)

;;; cit-globalref.el ends here
