;;; srecode-tests.el --- Some tests for CEDET's srecode

;; Copyright (C) 2008-2020 Free Software Foundation, Inc.

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

;; Extracted from srecode-fields.el and srecode-document.el in the
;; CEDET distribution.

;;; Code:

;;; From srecode-fields:

(require 'srecode/fields)

(defvar srecode-field-utest-text
  "This is a test buffer.

It is filled with some text."
  "Text for tests.")

(defun srecode-field-utest ()
  "Test the srecode field manager."
  (interactive)
  (srecode-field-utest-impl))

(defun srecode-field-utest-impl ()
  "Implementation of the SRecode field utest."
  (save-excursion
    (find-file "/tmp/srecode-field-test.txt")

    (erase-buffer)
    (goto-char (point-min))
    (insert srecode-field-utest-text)
    (set-buffer-modified-p nil)

    ;; Test basic field generation.
    (let ((srecode-field-archive nil)
	  (f nil))

      (end-of-line)
      (forward-word -1)

      (setq f (srecode-field "Test"
			     :name "TEST"
			     :start 6
			     :end 8))

      (when (or (not (slot-boundp f 'overlay)) (not (oref f overlay)))
	(error "Field test: Overlay info not created for field"))

      (when (and (overlayp (oref f overlay))
		 (not (overlay-get (oref f overlay) 'srecode-init-only)))
	(error "Field creation overlay is not tagged w/ init flag"))

      (srecode-overlaid-activate f)

      (when (or (not (overlayp (oref f overlay)))
		(overlay-get (oref f overlay) 'srecode-init-only))
	(error "New field overlay not created during activation"))

      (when (not (= (length srecode-field-archive) 1))
	(error "Field test: Incorrect number of elements in the field archive"))
      (when (not (eq f (car srecode-field-archive)))
	(error "Field test: Field did not auto-add itself to the field archive"))

      (when (not (overlay-get (oref f overlay) 'keymap))
	(error "Field test: Overlay keymap not set"))

      (when (not (string= "is" (srecode-overlaid-text f)))
	(error "Field test: Expected field text 'is', not %s"
	       (srecode-overlaid-text f)))

      ;; Test deletion.
      (srecode-delete f)

      (when (slot-boundp f 'overlay)
	(error "Field test: Overlay not deleted after object delete"))
      )

    ;; Test basic region construction.
    (let* ((srecode-field-archive nil)
	   (reg nil)
	   (fields
	    (list
	     (srecode-field "Test1" :name "TEST-1" :start 5 :end 10)
	     (srecode-field "Test2" :name "TEST-2" :start 15 :end 20)
	     (srecode-field "Test3" :name "TEST-3" :start 25 :end 30)

	     (srecode-field "Test4" :name "TEST-4" :start 35 :end 35))
	    ))

      (when (not (= (length srecode-field-archive) 4))
	(error "Region Test: Found %d fields.  Expected 4"
	       (length srecode-field-archive)))

      (setq reg (srecode-template-inserted-region "REG"
						  :start 4
						  :end 40))

      (srecode-overlaid-activate reg)

      ;; Make sure it was cleared.
      (when srecode-field-archive
	(error "Region Test: Did not clear field archive"))

      ;; Auto-positioning.
      (when (not (eq (point) 5))
	(error "Region Test: Did not reposition on first field"))

      ;; Active region
      (when (not (eq (srecode-active-template-region) reg))
	(error "Region Test: Active region not set"))

      ;; Various sizes
      (mapc (lambda (T)
	      (if (string= (object-name-string T) "Test4")
		  (progn
		    (when (not (srecode-empty-region-p T))
		      (error "Field %s is not empty"
			     (object-name T)))
		    )
		(when (not (= (srecode-region-size T) 5))
		  (error "Calculated size of %s was not 5"
			 (object-name T)))))
	    fields)

      ;; Make sure things stay up after a 'command'.
      (srecode-field-post-command)
      (when (not (eq (srecode-active-template-region) reg))
	(error "Region Test: Active region did not stay up"))

      ;; Test field movement.
      (when (not (eq (srecode-overlaid-at-point 'srecode-field)
		     (nth 0 fields)))
	(error "Region Test: Field %s not under point"
	       (object-name (nth 0 fields))))

      (srecode-field-next)

      (when (not (eq (srecode-overlaid-at-point 'srecode-field)
		     (nth 1 fields)))
	(error "Region Test: Field %s not under point"
	       (object-name (nth 1 fields))))

      (srecode-field-prev)

      (when (not (eq (srecode-overlaid-at-point 'srecode-field)
		     (nth 0 fields)))
	(error "Region Test: Field %s not under point"
	       (object-name (nth 0 fields))))

      ;; Move cursor out of the region and have everything cleaned up.
      (goto-char 42)
      (srecode-field-post-command)
      (when (srecode-active-template-region)
	(error "Region Test: Active region did not clear on move out"))

      (mapc (lambda (T)
	      (when (slot-boundp T 'overlay)
		(error "Overlay did not clear off of field %s"
		       (object-name T))))
	    fields)

      ;; End of LET
      )

    ;; Test variable linkage.
    (let* ((srecode-field-archive nil)
	   (f1 (srecode-field "Test1" :name "TEST" :start 6 :end 8))
	   (f2 (srecode-field "Test2" :name "TEST" :start 28 :end 30))
	   (f3 (srecode-field "Test3" :name "NOTTEST" :start 35 :end 40))
	   (reg (srecode-template-inserted-region "REG" :start 4 :end 40))
	   )
      (srecode-overlaid-activate reg)

      (when (not (string= (srecode-overlaid-text f1)
			  (srecode-overlaid-text f2)))
	(error "Linkage Test: Init strings are not ="))
      (when (string= (srecode-overlaid-text f1)
		     (srecode-overlaid-text f3))
	(error "Linkage Test: Init string on dissimilar fields is now the same"))

      (goto-char 7)
      (insert "a")

      (when (not (string= (srecode-overlaid-text f1)
			  (srecode-overlaid-text f2)))
	(error "Linkage Test: mid-insert strings are not ="))
      (when (string= (srecode-overlaid-text f1)
		     (srecode-overlaid-text f3))
	(error "Linkage Test: mid-insert string on dissimilar fields is now the same"))

      (goto-char 9)
      (insert "t")

      (when (not (string= (srecode-overlaid-text f1) "iast"))
	(error "Linkage Test: tail-insert failed to captured added char"))
      (when (not (string= (srecode-overlaid-text f1)
			  (srecode-overlaid-text f2)))
	(error "Linkage Test: tail-insert strings are not ="))
      (when (string= (srecode-overlaid-text f1)
		     (srecode-overlaid-text f3))
	(error "Linkage Test: tail-insert string on dissimilar fields is now the same"))

      (goto-char 6)
      (insert "b")

      (when (not (string= (srecode-overlaid-text f1) "biast"))
	(error "Linkage Test: tail-insert failed to captured added char"))
      (when (not (string= (srecode-overlaid-text f1)
			  (srecode-overlaid-text f2)))
	(error "Linkage Test: tail-insert strings are not ="))
      (when (string= (srecode-overlaid-text f1)
		     (srecode-overlaid-text f3))
	(error "Linkage Test: tail-insert string on dissimilar fields is now the same"))

      ;; Cleanup
      (srecode-delete reg)
      )

    (set-buffer-modified-p nil)

    (message "   All field tests passed.")
    ))

;;; From srecode-document:

(require 'srecode/document)

(defun srecode-document-function-comment-extract-test ()
  "Test old comment extraction.
Dump out the extracted dictionary."
  (interactive)

  (srecode-load-tables-for-mode major-mode)
  (srecode-load-tables-for-mode major-mode 'document)

  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))

  (let* ((temp (srecode-template-get-table (srecode-table)
					   "function-comment"
					   "declaration"
					   'document))
	 (fcn-in (semantic-current-tag)))

    (if (not temp)
	(error "No templates for function comments"))

    ;; Try to figure out the tag we want to use.
    (when (or (not fcn-in)
	      (not (semantic-tag-of-class-p fcn-in 'function)))
      (error "No tag of class 'function to insert comment for"))

    (let ((lextok (semantic-documentation-comment-preceding-tag fcn-in 'lex))
	  )

      (when (not lextok)
	(error "No comment to attempt an extraction"))

      (let ((s (semantic-lex-token-start lextok))
	    (e (semantic-lex-token-end lextok))
	    (extract nil))

	(pulse-momentary-highlight-region s e)

	;; Extract text from the existing comment.
	(setq extract (srecode-extract temp s e))

	(with-output-to-temp-buffer "*SRECODE DUMP*"
	  (princ "EXTRACTED DICTIONARY FOR ")
	  (princ (semantic-tag-name fcn-in))
	  (princ "\n--------------------------------------------\n")
	  (srecode-dump extract))))))

;;; srecode-tests.el ends here
