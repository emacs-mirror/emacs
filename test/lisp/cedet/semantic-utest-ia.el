;;; semantic-utest-ia.el --- Analyzer unit tests  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2024 Free Software Foundation, Inc.

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
;;
;; Use marked-up files in the test directory and run the analyzer
;; on them.  Make sure the answers are correct.
;;
;; Each file has cursor keys in them of the form:
;;   // -#- ("ans1" "ans2" )
;; where # is 1, 2, 3, etc, and some sort of answer list.
;; (Replace // with contents of comment-start for the language being tested.)

;;; Code:
(require 'ert)
(require 'ert-x)
(require 'semantic)
(require 'semantic/analyze)
(require 'semantic/analyze/refs)
(require 'semantic/symref)
(require 'semantic/symref/filter)

(ert-deftest semantic-utest-ia-doublens.cpp ()
  (let ((tst (ert-resource-file "testdoublens.cpp")))
    (should-not (semantic-ia-utest tst))))

(ert-deftest semantic-utest-ia-subclass.cpp ()
  (let ((tst (ert-resource-file "testsubclass.cpp")))
    (should-not (semantic-ia-utest tst))))

(ert-deftest semantic-utest-ia-typedefs.cpp ()
  (let ((tst (ert-resource-file "testtypedefs.cpp")))
    (should-not (semantic-ia-utest tst))))

(ert-deftest semantic-utest-ia-struct.cpp ()
  (let ((tst (ert-resource-file "teststruct.cpp")))
    (should-not (semantic-ia-utest tst))))

;;(ert-deftest semantic-utest-ia-union.cpp ()
;;  (let ((tst (ert-resource-file "testunion.cpp")))
;;    (should-not (semantic-ia-utest tst))))

(ert-deftest semantic-utest-ia-templates.cpp ()
  (let ((tst (ert-resource-file "testtemplates.cpp")))
    (should-not (semantic-ia-utest tst))))

;;(ert-deftest semantic-utest-ia-friends.cpp ()
;;  (let ((tst (ert-resource-file "testfriends.cpp")))
;;    (should-not (semantic-ia-utest tst))))

(ert-deftest semantic-utest-ia-using.cpp ()
  (let ((tst (ert-resource-file "testusing.cpp")))
    (should-not (semantic-ia-utest tst))))

(ert-deftest semantic-utest-ia-nsp.cpp ()
  (skip-unless (executable-find "g++"))
  (let ((tst (ert-resource-file "testnsp.cpp")))
    (should-not (semantic-ia-utest tst))))

(ert-deftest semantic-utest-ia-localvars.cpp ()
  (let ((tst (ert-resource-file "testlocalvars.cpp")))
    (should-not (semantic-ia-utest tst))))

(ert-deftest semantic-utest-ia-namespace.cpp ()
  (skip-unless (executable-find "g++"))
  (let ((tst (ert-resource-file "testnsp.cpp")))
    (should-not (semantic-ia-utest tst))))

(ert-deftest semantic-utest-ia-sppcomplete.c ()
  (let ((tst (ert-resource-file "testsppcomplete.c")))
    (should-not (semantic-ia-utest tst))))

(ert-deftest semantic-utest-ia-varnames.c ()
  (let ((tst (ert-resource-file "testvarnames.c")))
    (should-not (semantic-ia-utest tst))))

(ert-deftest semantic-utest-ia-javacomp.java ()
  (let ((tst (ert-resource-file "testjavacomp.java")))
    (should-not (semantic-ia-utest tst))))

(ert-deftest semantic-utest-ia-varnames.java ()
  (let ((tst (ert-resource-file "testvarnames.java")))
    (should-not (semantic-ia-utest tst))))

;;(ert-deftest semantic-utest-ia-f90.f90 ()
;;  (let ((tst (ert-resource-file "testf90.f90")))
;;    (should-not (semantic-ia-utest tst))))

(ert-deftest semantic-utest-ia-wisent.wy ()
  (let ((tst (ert-resource-file "testwisent.wy")))
    (should-not (semantic-ia-utest tst))))

(ert-deftest semantic-utest-ia-texi ()
  (let ((tst (ert-resource-file "test.texi")))
    (should-not (semantic-ia-utest tst))))

(ert-deftest semantic-utest-ia-make ()
  (let ((tst (ert-resource-file "test.mk")))
    (should-not (semantic-ia-utest tst))))

(ert-deftest semantic-utest-ia-srecoder ()
  (let ((tst (ert-resource-file "test.srt")))
    (should-not (semantic-ia-utest tst))))

;;; Core testing utility
(defun semantic-ia-utest (testfile)
  "Run the semantic ia unit test against stored sources."
  (semantic-mode 1)
  (let ((b (semantic-find-file-noselect testfile t)))

    ;; Run the test on it.
    (with-current-buffer b

      ;; This line will also force the include, scope, and typecache.
      (semantic-clear-toplevel-cache)
      ;; Force tags to be parsed.
      (semantic-fetch-tags)

      (prog1
          (or (semantic-ia-utest-buffer)
              (semantic-ia-utest-buffer-refs)
              (semantic-sr-utest-buffer-refs)
              (semantic-src-utest-buffer-refs))

        (kill-buffer b)
        ))))

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
	     (setq regex-p (concat "\\(" comment-start-skip "\\)\\s-*-"
				   (number-to-string idx) "-" )
		   regex-a (concat "\\(" comment-start-skip "\\)\\s-*#"
				   (number-to-string idx) "#" ))
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
	(skip-chars-backward " ") ;; some languages need a space.

	(let* ((ctxt (semantic-analyze-current-context))
               ;; TODO - fix the NOTFOUND case to be nil and not an error when finding
               ;; completions, then remove the below debug-on-error setting.
               (debug-on-error nil)
	       (acomp
		(condition-case _err
		    (semantic-analyze-possible-completions ctxt)
                  ((error user-error) nil))
                ))
	  (setq actual (mapcar 'semantic-format-tag-name acomp)))

	(goto-char a)

        (let ((bss (buffer-substring-no-properties (point) (pos-eol))))
	  (condition-case nil
	      (setq desired (read bss))
	    (error (setq desired (format "  FAILED TO PARSE: %S"
					 bss)))))

	(setq actual (sort actual 'string<))
	(setq desired (sort desired 'string<))

	(if (equal actual desired)
            (prog1
	        (setq pass (cons idx pass))
              ;;(message "PASS: %S" actual)
              )
	  (setq fail (cons
                      (list
                       (format "Failed %d.  Desired: %S Actual %S"
	                       idx desired actual)
                       )
                      fail)))

        (setq p nil a nil)
        (setq idx (1+ idx)))
      )

    (when fail
      (cons "COMPLETION SUBTEST" (reverse fail)))
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
	     (setq regex-p (concat "\\(" comment-start-skip
				   "\\)\\s-*\\^" (number-to-string idx) "^" )
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
	    (setq fail (cons
                        (list
	                 (message "Test id %d.  For %s (Num impls %d) (Num protos %d)"
	                          idx (if ct (semantic-tag-name ct) "<No tag found>")
	                          (length impl) (length proto))
                         )
                        fail))
	    ))
	(setq p nil)
	(setq idx (1+ idx))))
    (when fail
      (cons "ANALYZER REF COUNTING SUBTEST" fail))))

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
	     (setq regex-p (concat "\\(" comment-start-skip "\\)\\s-*\\%"
				   (number-to-string idx) "%" )
		   )
	     (goto-char (point-min))
	     (save-match-data
	       (when (re-search-forward regex-p nil t)
		 (setq tag (semantic-current-tag))
		 (goto-char (match-end 0))
                 (setq desired (read (buffer-substring (point) (pos-eol))))
		 ))
	     tag)

      (setq actual-result (semantic-symref-find-references-by-name
			   (semantic-format-tag-name tag) 'target
			   'symref-tool-used))

      (if (not actual-result)
	  (progn
	    (setq fail (cons idx fail))
	    (message "Failed Tool: %s" (eieio-object-name symref-tool-used))
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
	  (setq fail
                (cons (list
	               (when (not (equal (car actual) (car desired)))
                         (list
	                  (format "Actual: %S Desired: %S"
	                          (car actual) (car desired))
	                  (format "Failed Tool: %s" (eieio-object-name symref-tool-used))
	                  ))
	               (when (not (equal (car (cdr actual)) (car (cdr desired))))
	                 (list (format
	                        "Actual: %S Desired: %S"
	                        (car (cdr actual)) (car (cdr desired)))
	                       (format
	                        "Failed Tool: %s" (eieio-object-name symref-tool-used)))))
                      fail))
	  ))

      (setq idx (1+ idx))
      (setq tag nil))

    (when fail
      (cons "SYMREF SUBTEST" fail))))

(defun semantic-symref-test-count-hits-in-tag ()
  "Lookup in the current tag the symbol under point.
Then count all the other references to the same symbol within the
tag that contains point, and return that."
  (interactive)
  (let* ((ctxt (semantic-analyze-current-context))
	 (target (car (reverse (oref ctxt prefix))))
	 (tag (semantic-current-tag))
	 (Lcount 0))
    (when (semantic-tag-p target)
      (semantic-symref-hits-in-region
       target (lambda (_start _end _prefix) (setq Lcount (1+ Lcount)))
       (semantic-tag-start tag)
       (semantic-tag-end tag))
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
	     (setq regex-p (concat "\\(" comment-start-skip "\\)\\s-*@"
				   (number-to-string idx)
				   "@\\s-+\\w+" ))
	     (goto-char (point-min))
	     (save-match-data
	       (when (re-search-forward regex-p nil t)
		 (goto-char (match-end 0))
		 (skip-syntax-backward "w")
                 (setq desired (read (buffer-substring (point) (pos-eol))))
		 (setq start (match-beginning 0))
		 (goto-char start)
		 (setq actual (semantic-symref-test-count-hits-in-tag))
		 start)))

      (if (not actual)
	  (progn
	    (setq fail (cons
                        (list
	                 (format
	                  "Symref id %d: No results." idx))
                        fail))

	    )

	(if (equal desired actual)
	    ;; We passed
	    (setq pass (cons idx pass))
	  ;; We failed.
	  (setq fail (cons (list
	                    (when (not (equal actual desired))
	                      (format
	                       "Symref id %d: Actual: %S Desired: %S"
	                       idx actual desired)
	                      )
                            )
                           fail))
	  ))

      (setq idx (1+ idx))
      )

    (when fail
      (cons "SYMREF COUNTING SUBTEST" fail))))

(provide 'semantic-ia-utest)

;;; semantic-utest-ia.el ends here
