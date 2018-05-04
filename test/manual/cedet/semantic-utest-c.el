;;; semantic-utest-c.el --- C based parsing tests.

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
;; Run some C based parsing tests.

(require 'semantic)

(defvar semantic-utest-c-comparisons
  '( ("testsppreplace.c" . "testsppreplaced.c")
     )
  "List of files to parse and compare against each other.")

;;; Code:
;;;###autoload
(defun semantic-utest-c ()
  "Run parsing test for C from the test directory."
  (interactive)
  (dolist (fp semantic-utest-c-comparisons)
    (let* ((sem (locate-library "semantic"))
	   (sdir (file-name-directory sem))
	   (semantic-lex-c-nested-namespace-ignore-second nil)
	   (tags-actual
	    (save-excursion
	      (set-buffer (find-file-noselect (expand-file-name (concat "tests/" (car fp)) sdir)))
	      (semantic-clear-toplevel-cache)
	      (semantic-fetch-tags)))
	   (tags-expected
	    (save-excursion
	      (set-buffer (find-file-noselect (expand-file-name (concat "tests/" (cdr fp)) sdir)))
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
      ;; Passed?
      (message "PASSED!")
      )))


(provide 'semantic-utest-c)

;;; semantic-utest-c.el ends here
