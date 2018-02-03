;;; biditest.el --- test bidi reordering in GNU Emacs display engine.

;; Copyright (C) 2013-2018 Free Software Foundation, Inc.

;; Author: Eli Zaretskii
;; Maintainer: emacs-devel@gnu.org
;; Package: emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Produce a specially-formatted text file from BidiCharacterTest.txt
;; file that is part of the Unicode Standard's UCD package.  The file
;; shows the expected results of reordering according to the UBA.  The
;; file is supposed to be visited in Emacs, and the resulting display
;; compared with the expected one.

;;; Code:

(defun biditest-generate-testfile (input-file output-file)
  "Generate a bidi test file OUTPUT-FILE from data in INPUT-FILE.

INPUT-FILE should be in the format of the BidiCharacterTest.txt file
available from the Unicode site, as part of the UCD database, see
http://www.unicode.org/Public/UCD/latest/ucd/BidiCharacterTest.txt.

The resulting file should be viewed with `inhibit-bidi-mirroring' set to t."
  (let ((output-buf (get-buffer-create "*biditest-output*"))
	(lnum 1)
	tbuf)
    (with-temp-buffer
      (message "Generating output in %s ..." output-file)
      (setq tbuf (current-buffer))
      (insert-file-contents input-file)
      (goto-char (point-min))
      (while (not (eobp))
	(when (looking-at "^\\([0-9A-F ]+\\);\\([012]\\);\\([01]\\);\\([0-9x ]+\\);\\([0-9 ]+\\)$")
	  (let ((codes (match-string 1))
		(default-paragraph (match-string 2))
		(resolved-paragraph (match-string 3))
		;; FIXME: Should compare LEVELS with what the display
		;; engine actually produced.
		(levels (match-string 4))
		(indices (match-string 5)))
	    (setq codes (split-string codes " ")
		  indices (split-string indices " "))
	    (switch-to-buffer output-buf)
	    (insert (format "Test on line %d:\n\n" lnum))
	    ;; Force paragraph direction to what the UCD test
	    ;; specifies.
	    (insert (cond
		     ((string= default-paragraph "0") ;L2R
		      #x200e)
		     ((string= default-paragraph "1") ;R2L
		      #x200f)
		     (t "")))	; dynamic
	    ;; Insert the characters
	    (mapc (lambda (code)
		    (insert (string-to-number code 16)))
		  codes)
	    (insert "\n\n")
	    ;; Insert the expected results
	    (insert "Expected result:\n\n")
	    ;; We want the expected results displayed exactly as
	    ;; specified in the test file, without any reordering, so
	    ;; we override the directional properties of all of the
	    ;; characters in the expected result by prepending
	    ;; LRO/RLO.
	    (cond ((string= resolved-paragraph "0")
		   (insert #x200e #x202d))
		  ((string= resolved-paragraph "1")
		   (insert #x200f #x202e)
		   ;; We need to reverse the list of indices for R2L
		   ;; paragraphs, so that their logical order on
		   ;; display matches user expectations.
		   (setq indices (nreverse indices))))
	    (mapc (lambda (index)
		    (insert (string-to-number
			     (nth (string-to-number index 10) codes)
			     16)))
		  indices)
	    (insert #x202c)	; end the embedding
	    (insert "\n\n"))
	  (switch-to-buffer tbuf))
	(forward-line 1)
	(setq lnum (1+ lnum)))
      (switch-to-buffer output-buf)
      (let ((coding-system-for-write 'utf-8-unix))
	(write-file output-file))
      (message "Generating output in %s ... done" output-file))))

(defun biditest-create-test ()
  "Create a test file for testing the Emacs bidirectional display.

The resulting file should be viewed with `inhibit-bidi-mirroring' set to t."
  (biditest-generate-testfile (pop command-line-args-left)
			      (or (pop command-line-args-left)
				  "biditest.txt")))

;; A handy function for displaying the resolved bidi levels.
(defun bidi-levels ()
  "Display the resolved bidirectional levels of characters on current line.

The results can be compared with the levels stated in the
BidiCharacterTest.txt file."
  (interactive)
  (message "%s" (bidi-resolved-levels)))

(define-key global-map [f8] 'bidi-levels)
