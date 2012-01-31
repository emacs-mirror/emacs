;;; redisplay-testsuite.el --- Test suite for redisplay.

;; Copyright (C) 2009, 2010, 2011, 2012 Free Software Foundation, Inc.

;; Author: Chong Yidong <cyd@stupidchicken.com>
;; Keywords:       internal
;; Human-Keywords: internal

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Type M-x test-redisplay RET to generate the test buffer.

;;; Code:

(defun test-insert-overlay (text &rest props)
  (let ((opoint (point))
	overlay)
    (insert text)
    (setq overlay (make-overlay opoint (point)))
    (while props
      (overlay-put overlay (car props) (cadr props))
      (setq props (cddr props)))))

(defun test-redisplay-1 ()
  (insert "Test 1: Displaying adjacent and overlapping overlays:\n\n")
  (insert "  Expected: gnu emacs\n")
  (insert "  Results:  ")
  (test-insert-overlay "n" 'before-string "g" 'after-string  "u ")
  (test-insert-overlay "ma" 'before-string "e" 'after-string  "cs")
  (insert "\n\n")
  (insert "  Expected: gnu emacs\n")
  (insert "  Results:  ")
  (test-insert-overlay "u" 'before-string "gn")
  (test-insert-overlay "ma" 'before-string " e" 'after-string  "cs")
  (insert "\n\n")
  (insert "  Expected: gnu emacs\n")
  (insert "  Results:  ")
  (test-insert-overlay "XXX" 'display "u "
		       'before-string "gn" 'after-string  "em")
  (test-insert-overlay "a" 'after-string  "cs")
  (insert "\n\n")
  (insert "  Expected: gnu emacs\n")
  (insert "  Results:  ")
  (test-insert-overlay "u " 'before-string "gn" 'after-string  "em")
  (test-insert-overlay "XXX" 'display "a" 'after-string  "cs")
  (insert "\n\n"))

(defun test-redisplay-2 ()
  (insert "Test 2: Mouse highlighting.  Move your mouse over the letters XXX:\n\n")
  (insert "  Expected: "
	  (propertize "xxxXXXxxx" 'face 'highlight)
	  "...---...\n  Test:     ")
  (test-insert-overlay "XXX" 'before-string "xxx" 'after-string  "xxx"
		       'mouse-face 'highlight )
  (test-insert-overlay "---" 'before-string "..." 'after-string  "...")
  (insert "\n\n  Expected: "
	  (propertize "xxxXXX" 'face 'highlight)
	  "...---...\n  Test:     ")
  (test-insert-overlay "XXX" 'before-string "xxx" 'mouse-face 'highlight)
  (test-insert-overlay "---" 'before-string "..." 'after-string  "...")
  (insert "\n\n  Expected: "
	  (propertize "XXX" 'face 'highlight)
	  "...---...\n  Test:     ")
  (test-insert-overlay "..." 'display "XXX" 'mouse-face 'highlight)
  (test-insert-overlay "---" 'before-string "..." 'after-string  "...")
  (insert "\n\n  Expected: "
	  (propertize "XXXxxx" 'face 'highlight)
	  "...\n  Test:     ")
  (test-insert-overlay "..." 'display "XXX" 'after-string "xxx"
		       'mouse-face 'highlight)
  (test-insert-overlay "error" 'display "...")
  (insert "\n\n  Expected: "
	  "---..."
	  (propertize "xxxXXX" 'face 'highlight)
	  "\n  Test:     ")
  (test-insert-overlay "xxx" 'display "---" 'after-string "...")
  (test-insert-overlay "error" 'before-string "xxx" 'display "XXX"
		       'mouse-face 'highlight)
  (insert "\n\n  Expected: "
	  "...---..."
	  (propertize "xxxXXXxxx" 'face 'highlight)
	  "\n  Test:     ")
  (test-insert-overlay "---" 'before-string "..." 'after-string  "...")
  (test-insert-overlay "XXX" 'before-string "xxx" 'after-string  "xxx"
		       'mouse-face 'highlight)
  (insert "\n\n  Expected: "
	  "..."
	  (propertize "XXX" 'face 'highlight)
	  "...\n  Test:     ")
  (test-insert-overlay "---"
		       'display (propertize "XXX" 'mouse-face 'highlight)
		       'before-string "..."
		       'after-string  "...")
  (insert "\n\n  Expected: "
	  (propertize "XXX\n" 'face 'highlight)
	  "\n  Test:     ")
  (test-insert-overlay "XXX\n" 'mouse-face 'highlight)
  (insert "\n"))

(defun test-redisplay ()
  (interactive)
  (pop-to-buffer (generate-new-buffer "*Redisplay Test*"))
  (test-redisplay-1)
  (test-redisplay-2)
  (goto-char (point-min)))

;; arch-tag: fcee53c8-024f-403d-9154-61ae3ce0bfb8
