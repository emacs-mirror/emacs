;;; cit-symref.el ---
;;
;; Copyright (C) 2010 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cit-symref.el,v 1.2 2010-06-13 01:12:27 zappo Exp $
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
;; Test the Symref refactoring tool against this made up project.
;;
;; We can't test much in symref in the utest setup because they can't
;; do the refactoring steps, so we have to do them here instead.

;;; Code:


(defun cit-symref ()
  "Test the symref tool."
  (save-excursion
    ;; Start in a C++ file.
    (find-file (cit-file "src/main.cpp"))

    (let* ((target (car cit-symref-operations))
	   (dest (car (cdr cit-symref-operations)))
	   )
      ;; 7 - Run the symref tool
      ;; Create the UI
      (semantic-symref-symbol target)
      (cit-symref-count '(2 3 0 0))

      ;; expand them all
      (semantic-symref-list-expand-all)
      (cit-symref-count '(2 3 4 0))

      ;; Perform a rename.
      (semantic-symref-list-rename-open-hits dest)
      (save-some-buffers t)
      (cit-symref-count '(2 3 4 0))

      ;; Create a new symref buffer!
      (find-file (cit-file "src/main.cpp"))
      (semantic-symref-symbol dest)
      (cit-symref-count '(2 3 0 0))

      ;; Compile and run the refactored code.
      (find-file (cit-file "src/main.cpp"))
      (cit-compile-and-wait)
      (find-file (cit-file "src/main.cpp"))
      (cit-run-target "./Prog")
      )))

(defun cit-symref-quick-find-test ()
  "Test symref finding something, but not the more detailed test."
  (save-excursion
    ;; Start in a C++ file.
    (find-file (cit-file "src/main.cpp"))

    ;; Force new detection for every test.
    (setq semantic-symref-tool 'detect)

    ;; 7 - Run the symref tool
    ;; Create the UI
    (semantic-symref-regexp "doSomethingPublic")

    (cit-symref-count '(3 3 0 0))
    ))

(defun cit-symref-count (expected)
  "Count the current results, and return a list of the results.
Argument EXPECTED is the expected result count."
  (save-excursion
    (goto-char (point-min))
    (sit-for .1)
    (let ((files 0)
	  (hits 0)
	  (res 0)
	  (other 0))
      (while (not (eobp))
	(beginning-of-line)
	(cond ((looking-at "[^ ]")
	       (setq files (1+ files)))
	      ((looking-at "  \\[")
	       (setq hits (1+ hits)))
	      ((looking-at "    ")
	       (setq res (1+ res)))
	      (t (setq other (1+ other))))
	(forward-line 1)
	(end-of-line))
      (unless (and (= files (nth 0 expected))
		   (= hits (nth 1 expected))
		   (= res (nth 2 expected))
		   (= other (nth 3 expected)))
	(error "Symref Missmatch: %S != %S"
	       (list files hits res other)
	       expected)))))


(provide 'cit-symref)

;;; cit-symref.el ends here
