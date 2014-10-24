;;; cit-global.el ---
;;
;; Copyright (C) 2010, 2013, 2014 Eric M. Ludlam
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
;; Test GNU Global functionality against the current project.
;;
;; 1) Create a database
;; 2) Test the find-file feature
;; 3) Test the tag lookup feature.
;; 4) Test the symbol references lookup via symref

(require 'cit-symref)

;;; Code:

(defvar cit-externaldb-files-to-find
  '(("foo.hpp" . "include/foo.hpp")
    ;;("foo.cpp" . "src/foo.cpp")
    ("umltest.cpp" . "uml/umltest.cpp")
    ;("elfoo.el" . "src/elfoo.el")
    ;("foodoc.texi" . "src/foodoc.texi")
    ;("DNE.cpp" . "doesnotexist.cpp")
    )
  "List of file names to lookup and their locations.")

(defvar cit-external-db-tool-list
  '( (global ;; 0 tool name (used by symref)
      cedet-global ;; 1 library support for tool
      cedet-gnu-global-version-check ;; 2 version check
      cedet-gnu-global-create/update-database ;; 3 create a db
      ede-locate-global ;; 4 ede locate tool name
      semantic/db-global ;; 5 database src file
      semanticdb-enable-gnu-global-in-buffer ;; 6 enable db in a buffer
      semanticdb-table-global ;; 7 the database type
      ( "GTAGS" "GPATH" "GSYMS" "GRTAGS" ) ;; 8 files created
      )
     (idutils ;; 0 tool name (used by symref)
      cedet-idutils ;; 1 library support for tool
      cedet-idutils-version-check ;; 2 version check
      cedet-idutils-create/update-database ;; 3 create a db
      ede-locate-idutils ;; 4 ede locate tool name
      nil ;; 5 database src file
      nil ;; 6 enable db in a buffer
      nil ;; 7 the database type
      ( "ID" ) ;; 8 files created
      )
     (cscope ;; 0 tool name (used by symref)
      cedet-cscope ;; 1 library support for tool
      cedet-cscope-version-check ;; 2 version check
      cedet-cscope-create/update-database ;; 3 create a db
      ede-locate-cscope ;; 4 ede locate tool name
      nil ;; 5 database src file
      nil ;; 6 enable db in a buffer
      nil ;; 7 the database type
      ( "cscope.out" "cscope.files") ;; 8 files created
      )

     )
  "Different external DB tools to test.")

(defun cit-externaldb-test ()
  "Test the external database tools."
  (dolist (TOOL cit-external-db-tool-list)
    ;; Pull in the tool libraries needed
    (require (nth 1 TOOL))
    (when (nth 5 TOOL) (require (nth 5 TOOL)))

    ;; Check the tool
    (if (not (funcall (nth 2 TOOL) t))
	(progn
	  (message "Skipping %s test -- tool not installed." (nth 0 TOOL))
	  (sit-for 1))

      (catch 'abort-test-for-good-reason
	;; Call to test this instance.
	(cit-gnu-externaldb-test-one (nth 0 TOOL)
				     (nth 3 TOOL)
				     (nth 4 TOOL)
				     (nth 6 TOOL)
				     (nth 7 TOOL)
				     (nth 8 TOOL)
				     ))
      )))

(defun cit-externaldb-cleanup (cleanupfiles)
  "Clean up the files created by a database."
  ;; Delete the files created by external tool.
  (dolist (F cleanupfiles)
    (if (file-exists-p F)
	(progn
	  (message "DB Cleanup:  delete-file %s" F)
	  (delete-file F))
      (message "DB Cleanup: File not found to delete: %s" F))))

(defun cit-gnu-externaldb-test-one (symrefsym
				    createfcn
				    edelocatesym
				    semanticdbenablefcn
				    semanticdbclass
				    cleanupfiles)
  "Test external database tooling integration if it is available."
  (let ((bufftokill (find-file (cit-file "Project.ede"))))
    ;; 1) Create
    ;; We are at the root of the created CIT project.  Lets create a
    ;; database.
    (condition-case findcrash
	(funcall createfcn default-directory)
      (error
       (if (and (eq (car findcrash) 'error)
		(string-match "^Output:" (cadr findcrash)))
	   (progn
	     (message " !! Database create external program crashed !!  Aborting test for %S\n"
		      symrefsym)
	     (message "%S" (cadr findcrash))
	     (cit-externaldb-cleanup cleanupfiles)
	     (throw 'abort-test-for-good-reason t))
	 ;; ELSE, throw the actual error.
	 (message "An error occured creating the database.\n %S" findcrash))))

    ;; 2) force ede's find file to use external tool
    (require 'ede/locate)
    (let* ((ede-locate-setup-options (list edelocatesym))
	   (base default-directory)
	   (fname nil))

      ;; Change the locate tool active on this project.
      (ede-enable-locate-on-project)

      (dolist (F cit-externaldb-files-to-find)
	(let* ((raw (ede-expand-filename (ede-current-project) (car F)))
	       (expect (cdr F))
	       (result (when (stringp raw)
			 (file-relative-name raw default-directory))))

	  (when (not (string= result expect))
	    (error "%s: Expected %s; Found %s" symrefsym expect result))
	  ))

      (let ((fail (ede-expand-filename (ede-current-project) "doesnotexist.cpp")))
	(if fail
	    (error "%s TEST: Found a file that shouldn't exist." symrefsym)))
      )

    ;; After removing the old locate system, restore the old one.
    (ede-enable-locate-on-project)

    ;; 3) Look up tags with a external database
    (if semanticdbenablefcn
	(save-excursion
	  (let ((killme (find-file (cit-file "src/main.cpp"))))
	    (funcall semanticdbenablefcn)

	    (let ((res (semanticdb-find-tags-by-name "doSomethingPublic")))

	      ;; There is only one database result because we never enabled
	      ;; semanticdb minor mode.
	      (if (or (not res)
		      (not (object-of-class-p (car (car res)) semanticdbclass)))
		  (error "Did not find %s results table." symrefsym))

	      (when (/= (semanticdb-find-result-length res) 1)
		(error "%s should find 1 tag, found %d"
		       symrefsym
		       (semanticdb-find-result-length res)))

	      (dolist (tag (semanticdb-strip-find-results res 'name))
		(if (not (semantic--tag-get-property tag :filename))
		    (error "Tag %s does not point to a specific file."
			   (semantic-tag-name tag))))

	      (kill-buffer killme))))
      ;; else, message
      (message "Skipping %s database test : No database implemented." symrefsym))

    ;; 4) Symref symbol lookup via our external tool
    (setq semantic-symref-tool 'detect)
    (let ((detectedtool (semantic-symref-detect-symref-tool)))
      (when (not (eq detectedtool symrefsym))
	(error "Symref doesn't recognize %s backend.  Found %s instead"
	       symrefsym detectedtool)))

    ;; Do the tests again.
    (cit-symref-quick-find-test)

    (cit-externaldb-cleanup cleanupfiles)

    (kill-buffer bufftokill))
  )

(provide 'cit-externaldb)

;;; cit-externaldb.el ends here
