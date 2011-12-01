;;; semantic/utest-c.el --- C based parsing tests.

;; Copyright (C) 2008, 2009, 2010, 2011 Eric M. Ludlam

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
;; Run some C based parsing tests.

(defvar semantic-utest-c-comparisons
  '( ("testsppreplace.c" . "testsppreplaced.c")
     )
  "List of files to parse and compare against eachother.")

(defvar semantic-utest-c-conditionals
  '( "testsppcond.cpp" )
  "List of files for testing conditionals.")

;;; Code:
;;;###autoload
(defun semantic-utest-c ()
  "Run parsing test for C from the test directory."
  (interactive)
  (semantic-utest-c-compare)
  (semantic-utest-c-conditionals)
  ;; Passed?
  (message "PASSED!")
  )

(defun semantic-utest-c-compare ()
  "Run parsing test for C which compares two files.
The first file is full of SPP macros.
The second file is full of raw code that the macros should
expand to."
  (dolist (fp semantic-utest-c-comparisons)
    (let* ((sem (or (locate-library "cedet/semantic/utest-c")
		    (error "Cannot locate library 'cedet/semantic/utest-c'.")))
	   (sdir (file-name-directory sem))
	   (filename1 (expand-file-name (concat "tests/" (car fp)) sdir))
	   (filename2 (expand-file-name (concat "tests/" (cdr fp)) sdir))
	   (semantic-lex-c-nested-namespace-ignore-second nil)
	   (tags-actual
	    (save-excursion
	      (unless (file-exists-p filename1)
		(error "Cannot load %s." filename1))
	      (set-buffer (find-file-noselect filename1))
	      (semantic-clear-toplevel-cache)
	      (semantic-fetch-tags)))
	   (tags-expected
	    (save-excursion
	      (unless (file-exists-p filename2)
		(error "Cannot load %s." filename2))
	      (set-buffer (find-file-noselect filename2))
	      (semantic-clear-toplevel-cache)
	      (semantic-fetch-tags))))
      ;; Now that we have the tags, compare them for SPP accuracy.
      (dolist (tag tags-actual)
	(if (and (semantic-tag-of-class-p tag 'variable)
		 (semantic-tag-variable-constant-p tag))
	    nil				; skip the macros.
	  (if (semantic-tag-similar-with-subtags-p tag (car tags-expected))
	      (setq tags-expected (cdr tags-expected))
	    (with-mode-local c-mode
	      (error "Found: >> %s << Expected: >>  %s <<"
		     (semantic-format-tag-prototype tag nil t)
		     (semantic-format-tag-prototype (car tags-expected) nil t)
		     )))
	  ))
      )))

(defun semantic-utest-c-conditionals ()
  "Run parsing test for C which is full of conditional statements.
Functions parsed with FAIL in the name will fail the tests, while
those with PASS in the name will pass."
  (if (featurep 'xemacs)
      (message "\nNOTICE: XEmacs 21 doesn't support a recent enough version of hideif to run C contional tests.\n")

    (dolist (fp semantic-utest-c-conditionals)
      (let* ((sem (or (locate-library "cedet/semantic/utest-c")
		      (error "Cannot locate library 'cedet/semantic/utest-c'.")))
	     (sdir (file-name-directory sem))
	     (filename (expand-file-name (concat "tests/" fp) sdir))
	     (semantic-lex-c-nested-namespace-ignore-second nil)
	     (tags-actual
	      (save-excursion
		(unless (file-exists-p filename)
		  (error "Cannot load %s." filename))
		(set-buffer (find-file-noselect (expand-file-name (concat "tests/" fp) sdir)))
		(semantic-clear-toplevel-cache)
		(semantic-fetch-tags)))
	     )
	(dolist (tag tags-actual)
	  (let ((name (semantic-tag-name tag)))
	    (cond ((string-match "fail" name)
		   (error "Found: >> %s << which should not have been found"
			  name))
		  ((string-match "pass" name)
		   nil) ;; that's ok
		  ((and (semantic-tag-of-class-p tag 'variable)
			(semantic-tag-variable-constant-p tag))
		   nil) ;; Our macro definitions
		  (t
		   (error "Found: >> %s << which is not expected" name)))
	    )))))
  )

(provide 'cedet/semantic/utest-c)
;;; semantic/utest-c.el ends here
