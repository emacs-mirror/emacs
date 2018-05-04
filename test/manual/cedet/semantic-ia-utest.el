;;; semantic-ia-utest.el --- Analyzer unit tests

;; Copyright (C) 2008-2018 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;;
;; Use marked-up files in the test directory and run the analyzer
;; on them.  Make sure the answers are correct.
;;
;; Each file has cursor keys in them of the form:
;;   // -#- ("ans1" "ans2" )
;; where # is 1, 2, 3, etc, and some sort of answer list.

;;; Code:
(require 'semantic)
(require 'semantic/analyze)
(require 'semantic/analyze/refs)
(require 'semantic/symref)
(require 'semantic/symref/filter)

(load-file "cedet-utests.el")

(defvar semantic-ia-utest-file-list
  '(
    "tests/testdoublens.cpp"
    "tests/testsubclass.cpp"
    "tests/testtypedefs.cpp"
    "tests/testfriends.cpp"
    "tests/testnsp.cpp"
    "tests/testsppcomplete.c"
    "tests/testvarnames.c"
    "tests/testjavacomp.java"
    )
  "List of files with analyzer completion test points.")

(defvar semantic-ia-utest-error-log-list nil
  "List of errors occurring during a run.")

;;;###autoload
(defun semantic-ia-utest (&optional arg)
  "Run the semantic ia unit test against stored sources.
Argument ARG specifies which set of tests to run.
 1 - ia utests
 2 - regs utests
 3 - symrefs utests
 4 - symref count utests"
  (interactive "P")
  (save-excursion

    (let ((fl semantic-ia-utest-file-list)
	  (semantic-ia-utest-error-log-list nil)
	  )

      (cedet-utest-log-setup "ANALYZER")

      (set-buffer (semantic-find-file-noselect
		   (or (locate-library "semantic-ia-utest.el")
		       "semantic-ia-utest.el")))

      (while fl

	;; Make sure we have the files we think we have.
	(when (not (file-exists-p (car fl)))
	  (error "Cannot find unit test file: %s" (car fl)))

	;; Run the tests.
	(let ((fb (find-buffer-visiting (car fl)))
	      (b (semantic-find-file-noselect (car fl) t)))

	  ;; Run the test on it.
	  (save-excursion
	    (set-buffer b)

	    ;; This line will also force the include, scope, and typecache.
	    (semantic-clear-toplevel-cache)
	    ;; Force tags to be parsed.
	    (semantic-fetch-tags)

	    (semantic-ia-utest-log "  ** Starting tests in %s"
				   (buffer-name))

	    (when (or (not arg) (= arg 1))
	      (semantic-ia-utest-buffer))

	    (when (or (not arg) (= arg 2))
	      (set-buffer b)
	      (semantic-ia-utest-buffer-refs))

	    (when (or (not arg) (= arg 3))
	      (set-buffer b)
	      (semantic-sr-utest-buffer-refs))

	    (when (or (not arg) (= arg 4))
	      (set-buffer b)
	      (semantic-src-utest-buffer-refs))

	    (semantic-ia-utest-log "  ** Completed tests in %s\n"
				   (buffer-name))
	    )

	  ;; If it wasn't already in memory, whack it.
	  (when (not fb)
	    (kill-buffer b))
	  )
	(setq fl (cdr fl)))

      (cedet-utest-log-shutdown
       "ANALYZER"
       (when semantic-ia-utest-error-log-list
	 (format "%s Failures found."
		 (length semantic-ia-utest-error-log-list))))
      (when semantic-ia-utest-error-log-list
	(error "Failures found during analyzer unit tests"))
      ))
  )

(defun semantic-ia-utest-buffer ()
  "Run analyzer completion unit-test pass in the current buffer."

  (let* ((idx 1)
	 (regex-p nil)
	 (regex-a nil)
	 (p nil)
	 (a nil)
	 (pass nil)
	 (fail nil)
	 (actual nil)
	 (desired nil)
	 ;; Exclude unpredictable system files in the
	 ;; header include list.
	 (semanticdb-find-default-throttle
	  (remq 'system semanticdb-find-default-throttle))
	 )
    ;; Keep looking for test points until we run out.
    (while (save-excursion
	     (setq regex-p (concat "//\\s-*-" (number-to-string idx) "-" )
		   regex-a (concat "//\\s-*#" (number-to-string idx) "#" ))
	     (goto-char (point-min))
	     (save-match-data
	       (when (re-search-forward regex-p nil t)
		 (setq p (match-beginning 0))))
	     (save-match-data
	       (when (re-search-forward regex-a nil t)
		 (setq a (match-end 0))))
	     (and p a))

      (save-excursion

	(goto-char p)

	(let* ((ctxt (semantic-analyze-current-context))
	       (acomp
		(condition-case nil
		    (semantic-analyze-possible-completions ctxt)
		  (error nil))))
	  (setq actual (mapcar 'semantic-tag-name acomp)))

	(goto-char a)

	(let ((bss (buffer-substring-no-properties (point) (point-at-eol))))
	  (condition-case nil
	      (setq desired (read bss))
	    (error (setq desired (format "  FAILED TO PARSE: %S"
					 bss)))))

	(if (equal actual desired)
	    (setq pass (cons idx pass))
	  (setq fail (cons idx fail))
	  (semantic-ia-utest-log
	   "    Failed %d.  Desired: %S Actual %S"
	   idx desired actual)
	  (add-to-list 'semantic-ia-utest-error-log-list
		       (list (buffer-name) idx desired actual)
		       )

	  )
	)

      (setq p nil a nil)
      (setq idx (1+ idx)))

    (if fail
	(progn
	  (semantic-ia-utest-log
	   "    Unit tests (completions) failed tests %S"
	   (reverse fail))
	  )
      (semantic-ia-utest-log "    Unit tests (completions) passed (%d total)"
			     (- idx 1)))

    ))

(defun semantic-ia-utest-buffer-refs ()
  "Run an analyze-refs unit-test pass in the current buffer."

  (let* ((idx 1)
	 (regex-p nil)
	 (p nil)
	 (pass nil)
	 (fail nil)
	 ;; Exclude unpredictable system files in the
	 ;; header include list.
	 (semanticdb-find-default-throttle
	  (remq 'system semanticdb-find-default-throttle))
	 )
    ;; Keep looking for test points until we run out.
    (while (save-excursion
	     (setq regex-p (concat "//\\s-*\\^" (number-to-string idx) "^" )
		   )
	     (goto-char (point-min))
	     (save-match-data
	       (when (re-search-forward regex-p nil t)
		 (setq p (match-beginning 0))))
	     p)

      (save-excursion

	(goto-char p)
	(forward-char -1)

	(let* ((ct (semantic-current-tag))
	       (refs (semantic-analyze-tag-references ct))
	       (impl (semantic-analyze-refs-impl refs t))
	       (proto (semantic-analyze-refs-proto refs t))
	       (pf nil)
	       )
	  (setq
	   pf
	   (catch 'failed
	     (if (and impl proto (car impl) (car proto))
		 (let (ct2 ref2 impl2 proto2
			   newstart)
		   (cond
		    ((semantic-equivalent-tag-p (car impl) ct)
		     ;; We are on an IMPL.  Go To the proto, and find matches.
		     (semantic-go-to-tag (car proto))
		     (setq newstart (car proto))
		     )
		    ((semantic-equivalent-tag-p (car proto) ct)
		     ;; We are on a PROTO.  Go to the imple, and find matches
		     (semantic-go-to-tag (car impl))
		     (setq newstart (car impl))
		     )
		    (t
		     ;; No matches is a fail.
		     (throw 'failed t)
		     ))
		   ;; Get the new tag, does it match?
		   (setq ct2 (semantic-current-tag))

		   ;; Does it match?
		   (when (not (semantic-equivalent-tag-p ct2 newstart))
		     (throw 'failed t))

		   ;; Can we double-jump?
		   (setq ref2 (semantic-analyze-tag-references ct)
			 impl2 (semantic-analyze-refs-impl ref2 t)
			 proto2 (semantic-analyze-refs-proto ref2 t))

		   (when (or (not (and impl2 proto2))
			     (not
			      (and (semantic-equivalent-tag-p
				    (car impl) (car impl2))
				   (semantic-equivalent-tag-p
				    (car proto) (car proto2)))))
		     (throw 'failed t))
		   )

	       ;; Else, no matches at all, so another fail.
	       (throw 'failed t)
	       )))

	   (if (not pf)
	      ;; We passed
	      (setq pass (cons idx pass))
	    ;; We failed.
	    (setq fail (cons idx fail))
	    (semantic-ia-utest-log
	     "    Failed %d.  For %s (Num impls %d) (Num protos %d)"
	     idx (if ct (semantic-tag-name ct) "<No tag found>")
	     (length impl) (length proto))
	    (add-to-list 'semantic-ia-utest-error-log-list
			 (list (buffer-name) idx)
			 )
	    ))

	(setq p nil)
	(setq idx (1+ idx))

	))

    (if fail
	(progn
	  (semantic-ia-utest-log
	   "    Unit tests (refs) failed tests")
	  )
      (semantic-ia-utest-log "    Unit tests (refs) passed (%d total)"
			     (- idx 1)))

    ))

(defun semantic-sr-utest-buffer-refs ()
  "Run a symref unit-test pass in the current buffer."

  ;; This line will also force the include, scope, and typecache.
  (semantic-clear-toplevel-cache)
  ;; Force tags to be parsed.
  (semantic-fetch-tags)

  (let* ((idx 1)
	 (tag nil)
	 (regex-p nil)
	 (desired nil)
	 (actual-result nil)
	 (actual nil)
	 (pass nil)
	 (fail nil)
	 (symref-tool-used nil)
	 ;; Exclude unpredictable system files in the
	 ;; header include list.
	 (semanticdb-find-default-throttle
	  (remq 'system semanticdb-find-default-throttle))
	 )
    ;; Keep looking for test points until we run out.
    (while (save-excursion
	     (setq regex-p (concat "//\\s-*\\%" (number-to-string idx) "%" )
		   )
	     (goto-char (point-min))
	     (save-match-data
	       (when (re-search-forward regex-p nil t)
		 (setq tag (semantic-current-tag))
		 (goto-char (match-end 0))
		 (setq desired (read (buffer-substring (point) (point-at-eol))))
		 ))
	     tag)

      (setq actual-result (semantic-symref-find-references-by-name
			   (semantic-tag-name tag) 'target
			   'symref-tool-used))

      (if (not actual-result)
	  (progn
	    (setq fail (cons idx fail))
	    (semantic-ia-utest-log
	     "  Failed FNames %d: No results." idx)
	    (semantic-ia-utest-log
	     "  Failed Tool: %s" (object-name symref-tool-used))

	    (add-to-list 'semantic-ia-utest-error-log-list
			 (list (buffer-name) idx)
			 )
	    )

	(setq actual (list (sort (mapcar
				  'file-name-nondirectory
				  (semantic-symref-result-get-files actual-result))
				 'string<)
			   (sort
			    (mapcar
			     'semantic-format-tag-canonical-name
			     (semantic-symref-result-get-tags actual-result))
			    'string<)))


	(if (equal desired actual)
	    ;; We passed
	    (setq pass (cons idx pass))
	  ;; We failed.
	  (setq fail (cons idx fail))
	  (when (not (equal (car actual) (car desired)))
	    (semantic-ia-utest-log
	     "  Failed FNames %d: Actual: %S Desired: %S"
	     idx (car actual) (car desired))
	    (semantic-ia-utest-log
	     "  Failed Tool: %s" (object-name symref-tool-used))
	    )
	  (when (not (equal (car (cdr actual)) (car (cdr desired))))
	    (semantic-ia-utest-log
	     "  Failed TNames %d: Actual: %S Desired: %S"
	     idx (car (cdr actual)) (car (cdr desired)))
	    (semantic-ia-utest-log
	     "  Failed Tool: %s" (object-name symref-tool-used))
	    )
	  (add-to-list 'semantic-ia-utest-error-log-list
		       (list (buffer-name) idx)
		       )
	  ))

      (setq idx (1+ idx))
      (setq tag nil))

    (if fail
	(progn
	  (semantic-ia-utest-log
	   "    Unit tests (symrefs) failed tests")
	  )
      (semantic-ia-utest-log "    Unit tests (symrefs) passed (%d total)"
			     (- idx 1)))

    ))

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
       target (lambda (start end prefix) (setq Lcount (1+ Lcount)))
       (semantic-tag-start tag)
       (semantic-tag-end tag))
      (when (interactive-p)
	(message "Found %d occurrences of %s in %.2f seconds"
		 Lcount (semantic-tag-name target)
		 (semantic-elapsed-time start nil)))
      Lcount)))

(defun semantic-src-utest-buffer-refs ()
  "Run a sym-ref counting unit-test pass in the current buffer."

  ;; This line will also force the include, scope, and typecache.
  (semantic-clear-toplevel-cache)
  ;; Force tags to be parsed.
  (semantic-fetch-tags)

  (let* ((idx 1)
	 (start nil)
	 (regex-p nil)
	 (desired nil)
	 (actual nil)
	 (pass nil)
	 (fail nil)
	 ;; Exclude unpredictable system files in the
	 ;; header include list.
	 (semanticdb-find-default-throttle
	  (remq 'system semanticdb-find-default-throttle))
	 )
    ;; Keep looking for test points until we run out.
    (while (save-excursion
	     (setq regex-p (concat "//\\s-*@"
				   (number-to-string idx)
				   "@\\s-+\\(\\w+\\)" ))
	     (goto-char (point-min))
	     (save-match-data
	       (when (re-search-forward regex-p nil t)
		 (goto-char (match-beginning 1))
		 (setq desired (read (buffer-substring (point) (point-at-eol))))
		 (setq start (match-beginning 0))
		 (goto-char start)
		 (setq actual (semantic-symref-test-count-hits-in-tag))
		 start)))

      (if (not actual)
	  (progn
	    (setq fail (cons idx fail))
	    (semantic-ia-utest-log
	     "  Failed symref count %d: No results." idx)

	    (add-to-list 'semantic-ia-utest-error-log-list
			 (list (buffer-name) idx)
			 )
	    )

	(if (equal desired actual)
	    ;; We passed
	    (setq pass (cons idx pass))
	  ;; We failed.
	  (setq fail (cons idx fail))
	  (when (not (equal actual desired))
	    (semantic-ia-utest-log
	     "  Failed symref count %d: Actual: %S Desired: %S"
	     idx actual desired)
	    )

	  (add-to-list 'semantic-ia-utest-error-log-list
		       (list (buffer-name) idx)
		       )
	  ))

      (setq idx (1+ idx))
      )

    (if fail
	(progn
	  (semantic-ia-utest-log
	   "    Unit tests (symrefs counter) failed tests")
	  )
      (semantic-ia-utest-log "    Unit tests (symrefs counter) passed (%d total)"
			     (- idx 1)))

    ))

(defun semantic-ia-utest-start-log ()
  "Start up a testlog for a run."
  ;; Redo w/ CEDET utest framework.
  (cedet-utest-log-start "semantic: analyzer tests"))

(defun semantic-ia-utest-log (&rest args)
  "Log some test results.
Pass ARGS to format to create the log message."
  ;; Forward to CEDET utest framework.
  (apply 'cedet-utest-log args))

(provide 'semantic-ia-utest)

;;; semantic-ia-utest.el ends here
