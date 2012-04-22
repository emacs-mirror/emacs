;;; cedet/semantic/fmt-utest.el --- Parsing / Formatting tests
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
;; Unit tests for the formatting feature.
;;
;; Using test code from the tests source directory, parse the source
;; file.  After parsing, read the comments for each signature, and
;; make sure that the semantic-tag-format-* functions in question
;; created the desired output.

(require 'semantic)
(require 'semantic/format)

;;; Code:

(defvar semantic-fmt-utest-file-list
  '("tests/test-fmt.cpp"
    "tests/test-fmt.el"
    )
  "List of files to run unit tests in.")

(defvar semantic-fmt-utest-error-log-list nil
  "Log errors during testing in this variable.")

;;;###autoload
(defun semantic-fmt-utest ()
  "Visit all file entries, and run formatting test.
Files to visit are in `semantic-fmt-utest-file-list'."
  (interactive)

  (save-current-buffer

    (let ((fl semantic-fmt-utest-file-list)
	  (semantic-fmt-utest-error-log-list nil)
	  )

      (cedet-utest-log-setup "PARSE/FORMAT")

      (set-buffer (semantic-find-file-noselect
		   (locate-library "cedet/semantic/fmt-utest.el")))

      (dolist (FILE fl)

	(save-current-buffer
	  ;; Make sure we have the files we think we have.
	  (when (not (file-exists-p FILE))
	    (error "Cannot find unit test file: %s" FILE))

	  ;; Run the tests.
	  (let ((fb (find-buffer-visiting FILE))
		(b (semantic-find-file-noselect FILE))
		(num 0)
		(tags nil))

	    (save-current-buffer
	      (set-buffer b)
	      (when (not (semantic-active-p))
		(error "Cannot open %s for format tests" FILE))

	      ;; This will force a reparse, removing any chance of semanticdb cache
	      ;; using stale data.
	      (semantic-clear-toplevel-cache)
	      ;; Force the reparse
	      (setq tags (semantic-fetch-tags))
	      (setq num (length tags))

	      (semantic-fmt-utest-log "  ** Starting tests in %s"
				      (buffer-name))

	      (save-excursion
		(while tags
		  (let* ((T (car tags))
			 (start (semantic-tag-end T))
			 (end (if (cdr tags)
				  (semantic-tag-start (car (cdr tags)))
				(point-max)))
			 (TESTS nil)
			 )
		    (goto-char start)
		    ;; Scan the space between tags for all test condition matches.
		    (while (re-search-forward "## \\([a-z-]+\\) \"\\([^\n\"]+\\)\"$" end t)
		      (push (cons (match-string 1) (match-string 2)) TESTS))
		    (setq TESTS (nreverse TESTS))

		    (dolist (TST TESTS)
		      (let* ( ;; For each test, convert CAR into a semantic-format-tag* fcn
			     (sym (intern (concat "semantic-format-tag-" (car TST))))
			     ;; Convert the desired result from a string syntax to a string.
			     (desired (cdr TST))
			     ;; What does the fmt function do?
			     (actual (funcall sym T))
			     )
			(unless (string= desired actual)

			  (add-to-list 'semantic-fmt-utest-error-log-list
				       (list (buffer-name) (semantic-tag-name T)
					     sym)
				       )

			  (semantic-fmt-utest-log
			   "    Failed %s/%s.  Desired: %S Actual %S"
			   (semantic-tag-name T) sym
			   desired actual))))
		    )
		  (setq tags (cdr tags)))


		(semantic-fmt-utest-log "  ** Completed %d tests in %s\n"
					num (buffer-name))
		))

	    ;; If it wasn't already in memory, whack it.
	    (when (and b (not fb))
	      (kill-buffer b)))
	  ))

      (cedet-utest-log-shutdown
       "PARSE/FORMAT"
       (when semantic-fmt-utest-error-log-list
	 (format "%s Failures found."
		 (length semantic-fmt-utest-error-log-list))))
      (when semantic-fmt-utest-error-log-list
	(error "Failures found during parse/format unit tests"))
      )))


(defun semantic-fmt-utest-start-log ()
  "Start up a testlog for a run."
  ;; Redo w/ CEDET utest framework.
  (cedet-utest-log-start "semantic: format tests"))

(defun semantic-fmt-utest-log (&rest args)
  "Log some test results.
Pass ARGS to format to create the log message."
  ;; Forward to CEDET utest framework.
  (apply 'cedet-utest-log args))

(provide 'cedet/semantic/fmt-utest)

;;; semantic-fmt-utest.el ends here
