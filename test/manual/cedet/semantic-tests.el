;;; semantic-tests.el --- Miscellaneous Semantic tests.  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2026 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Originally, there are many test functions scattered among the
;; Semantic source files.  This file consolidates them.

;;; Code:

(require 'data-debug)

;;; From semantic-complete

(require 'semantic/complete)

(defun semantic-complete-test ()
  "Test completion mechanisms."
  (interactive)
  (message "%S"
   (semantic-format-tag-prototype
    (semantic-complete-read-tag-project "Symbol: "))))

;;; From semanticdb-ebrowse

(require 'semantic/db-ebrowse)

(defun semanticdb-ebrowse-run-tests ()
  "Run some tests of the semanticdb-ebrowse system.
All systems are different.  Ask questions along the way."
  (interactive)
  (let ((doload nil))
    (when (y-or-n-p "Create a system database to test with?")
      (call-interactively 'semanticdb-create-ebrowse-database)
      (setq doload t))
    ;;  Should we load in caches
    (when (if doload
	      (y-or-n-p "New database created.  Reload system databases? ")
	    (y-or-n-p "Load in all system databases? "))
      (semanticdb-load-ebrowse-caches)))
  ;; Ok, databases were created.  Let's try some searching.
  (when (not (or (eq major-mode 'c-mode)
		 (eq major-mode 'c++-mode)))
    (error "Please make your default buffer be a C or C++ file, then
run the test again")))

(defun semanticdb-ebrowse-dump ()
  "Find the first loaded ebrowse table, and dump out the contents."
  (interactive)
  (let ((db semanticdb-database-list)
	;; (ab nil)
	)
    (while db
      (when (semanticdb-project-database-ebrowse-p (car db))
	;; (setq ab
	      (data-debug-new-buffer "*EBROWSE Database*") ;;)
	(data-debug-insert-thing (car db) "*" "")
	(setq db nil)
	)
      (setq db (cdr db)))))

;;; From semanticdb-global:

(require 'semantic/db-global)

(defvar semanticdb-test-gnu-global-startfile "~/src/global-5.7.3/global/global.c"
  "File to use for testing.")

(defun semanticdb-test-gnu-global (searchfor &optional standardfile)
  "Test the GNU Global semanticdb.
Argument SEARCHFOR is the text to search for.
If optional arg STANDARDFILE is non-nil, use a standard file with
global enabled."
  (interactive "sSearch For Tag: \nP")

  (require 'data-debug)
  (save-excursion
    (when standardfile
      (save-match-data
	(set-buffer (find-file-noselect semanticdb-test-gnu-global-startfile))))

    (condition-case err
	(semanticdb-enable-gnu-global-in-buffer)
      (error (if standardfile
		 (error err)
	       (save-match-data
		 (set-buffer (find-file-noselect semanticdb-test-gnu-global-startfile)))
	       (semanticdb-enable-gnu-global-in-buffer))))

    (let* ((db (semanticdb-project-database-global)) ;; "global"
	   (tab (semanticdb-file-table db (buffer-file-name)))
	   (result (semanticdb-deep-find-tags-for-completion-method tab searchfor))
	   )
      (data-debug-new-buffer "*SemanticDB Gnu Global Result*")
      (data-debug-insert-thing result "?" ""))))

;;; From semantic-format

(require 'semantic/format)

(defun semantic-test-all-format-tag-functions (&optional arg)
  "Test all outputs from `semantic-format-tag-functions'.
Output is generated from the function under `point'.
Optional argument ARG specifies not to use color."
  (interactive "P")
  (semantic-fetch-tags)
  (let* ((tag (semantic-current-tag))
	 (par (semantic-current-tag-parent))
	 (fns semantic-format-tag-functions))
    (with-output-to-temp-buffer "*format-tag*"
      (princ "Tag->format function tests:")
      (while fns
	(princ "\n")
	(princ (car fns))
	(princ ":\n ")
	(let ((s (funcall (car fns) tag par (not arg))))
	  (with-current-buffer "*format-tag*"
	    (goto-char (point-max))
	    (insert s)))
	(setq fns (cdr fns))))
      ))

;;; From semantic-fw:

(require 'semantic/fw)

(defun semantic-test-throw-on-input ()
  "Test that throw on input will work."
  (interactive)
  (semantic-throw-on-input 'done-die)
  (message "Exit Code: %s"
	   (semantic-exit-on-input 'testing
	     (let ((inhibit-quit nil)
		   (message-log-max nil))
	       (while t
		 (message "Looping ... press a key to test")
		 (semantic-throw-on-input 'test-inner-loop))
	       'exit)))
  (when (input-pending-p)
    (if (fboundp 'read-event)
	(read-event)
      (read-char))))

;;; From semantic-idle:

(require 'semantic/idle)

(defun semantic-idle-pnf-test ()
  "Test `semantic-idle-scheduler-work-parse-neighboring-files' and time it."
  (interactive)
  (let ((start (current-time))
	(_junk (semantic-idle-scheduler-work-parse-neighboring-files)))
    (message "Work took %.2f seconds." (semantic-elapsed-time start nil))))

;;; From semantic-lex:

(require 'semantic/lex)

(defun semantic-lex-test-full-depth (arg)
  "Test the semantic lexer in the current buffer parsing through lists.
Usually the lexer parses.
If universal argument ARG, then try the whole buffer."
  (interactive "P")
  (let* ((start (current-time))
	 (result (semantic-lex
		  (if arg (point-min) (point))
		  (point-max)
		  100)))
    (message "Elapsed Time: %.2f seconds."
	     (semantic-elapsed-time start nil))
    (pop-to-buffer "*Lexer Output*")
    (require 'pp)
    (erase-buffer)
    (insert (pp-to-string result))
    (goto-char (point-min))))

(defun semantic-lex-test-region (beg end)
  "Test the semantic lexer in the current buffer.
Analyze the area between BEG and END."
  (interactive "r")
  (let ((result (semantic-lex beg end)))
    (pop-to-buffer "*Lexer Output*")
    (require 'pp)
    (erase-buffer)
    (insert (pp-to-string result))
    (goto-char (point-min))))

;;; From semantic-lex-spp:

(require 'semantic/lex-spp)

(defun semantic-lex-spp-write-test ()
  "Test the semantic tag writer against the current buffer."
  (interactive)
  (with-output-to-temp-buffer "*SPP Write Test*"
    (semantic-lex-spp-table-write-slot-value
     (semantic-lex-spp-save-table))))

(defvar cedet-utest-directory) ;From test/manual/cedet/cedet-utests.el?

(defun semantic-lex-spp-write-utest ()
  "Unit test using the test spp file to test the slot write fcn."
  (interactive)
  (save-excursion
    (let ((buff (find-file-noselect
		 (expand-file-name "tests/testsppreplace.c"
				   cedet-utest-directory))))
      (set-buffer buff)
      (semantic-lex-spp-write-test)
      (kill-buffer buff)
      (when (not (called-interactively-p 'interactive))
        (kill-buffer "*SPP Write Test*"))
      )))

;;; From semantic-tag-write:

;;; TESTING.

(require 'semantic/tag-write)

(defun semantic-tag-write-test ()
  "Test the semantic tag writer against the tag under point."
  (interactive)
  (with-output-to-temp-buffer "*Tag Write Test*"
    (semantic-tag-write-one-tag (semantic-current-tag))))

(defun semantic-tag-write-list-test ()
  "Test the semantic tag writer against the tag under point."
  (interactive)
  (with-output-to-temp-buffer "*Tag Write Test*"
    (semantic-tag-write-tag-list (semantic-fetch-tags))))

;;; From semantic-symref-filter:

(require 'semantic/symref/filter)

(defun semantic-symref-test-count-hits-in-tag ()
  "Lookup in the current tag the symbol under point.
Then count all the other references to the same symbol within the
tag that contains point, and return that."
  (interactive)
  (let* ((ctxt (semantic-analyze-current-context))
	 (target (car (reverse (oref ctxt prefix))))
	 (tag (semantic-current-tag))
	 (start (current-time))
	 (Lcount 0))
    (when (semantic-tag-p target)
      (semantic-symref-hits-in-region
       target (lambda (_start _end _prefix) (setq Lcount (1+ Lcount)))
       (semantic-tag-start tag)
       (semantic-tag-end tag))
      (when (called-interactively-p 'interactive)
	(message "Found %d occurrences of %s in %.2f seconds"
		 Lcount (semantic-tag-name target)
		 (semantic-elapsed-time start nil)))
      Lcount)))

;;; semantic-tests.el ends here
