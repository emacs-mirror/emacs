;;; elisp-benchmarks.el --- elisp benchamrks collection -*- lexical-binding:t -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; Author: akrl@sdf.org
;; Maintainer: akrl@sdf.org
;; Version: 1.0
;; Keywords: languages, lisp
;; Created: 2019-01-12

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; In use for testing the Emacs Lisp implementation performance.

;; To minimize CPU frequency bouncing effects and other sources of
;; noise all benchmarks are repeated `elb-runs' times by default.

;; To add a new benchmark just depose the file into the benchmarks/
;; directory.  Every benchmark foo.el has to define as entry-point a
;; function foo-entry.

;; Tests are of an arbitrary length that on my machine is in the
;; magnitude order of the 10 seconds for each single run
;; byte-compiled.  Please consider this as relative measure when
;; adding new benchmarks.

;;; Usage:
;; emacs -batch -l .../elisp-benchmarks.el -f elisp-benchmarks-run

;;; Code:

(require 'cl-lib)
(require 'benchmark)
(require 'outline)
(require 'org)

(defgroup elb nil
  "Emacs Lisp benchmarks."
  :group 'lisp)

(defcustom elb-runs 3
  "Total number of benchmark iterations."
  :type 'number
  :group 'comp)

(defconst elb-bench-directory
  (concat (file-name-directory (or load-file-name buffer-file-name))
	  "benchmarks/"))

(defconst elb-result-buffer-name "elisp-benchmarks-results"
  "Buffer name where results are presented.")

(defun elb-std-deviation (list)
  "Return the standard deviation of the elements in LIST."
  (let* ((n (length list))
	 (mean (/ (cl-loop for x in list
			   sum x)
		  n)))
    (sqrt (/ (cl-loop for x in list
		   sum (expt (- x mean) 2))
	  (1- n)))))

;;;###autoload
(defun elisp-benchmarks-run (&optional selector recompile runs)
  "Run all the benchmarks and present the results.
If non nil SELECTOR is a regexp to match the benchmark names to be executed.
The test is repeated RUNS number of times.  If RUNS is nil `elb-runs' is used as
default.
RECOMPILE all the benchmark folder when non nil."
  (interactive)
  (cl-loop with runs = (or runs elb-runs)
	   repeat runs
	   for i from 1
	   named test-loop
	   with res = (make-hash-table :test #'equal)
	   with sources = (directory-files elb-bench-directory t "\\.el$")
	   with tests = (if selector
			    (cl-loop for f in sources
				     when (string-match selector f)
				       collect (file-name-base f))
			  (mapcar #'file-name-base sources))
	   initially
	   (if recompile
	       (mapc (lambda (f) (byte-compile-file f t)) sources)
	     (mapc #'load (mapcar #'file-name-sans-extension sources)))
	   (cl-loop for test in tests
		    do (puthash test () res))
	   do
	   (message "Iteration number: %d" i)
	   (cl-loop for test in tests
		    for entry-point = (intern (concat "elb-" test "-entry"))
		    do
		    (garbage-collect)
		    (message "Running %s..." test)
		    (push (eval `(benchmark-run nil (,entry-point)) t)
			  (gethash test res)))
	   finally
	   (pop-to-buffer elb-result-buffer-name)
	   (erase-buffer)
	   (insert "* Results\n\n")
	   (insert "  |test|non-gc avg (s)|gc avg (s)|gcs avg|tot avg (s)|tot avg err (s)\n")
	   (insert "|-\n")
	   (cl-loop for test in tests
		    for l = (gethash test res)
		    for test-elapsed = (cl-loop for x in l sum (car x))
		    for test-gcs = (cl-loop for x in l sum (cadr x))
		    for test-gc-elapsed = (cl-loop for x in l sum (caddr x))
		    for test-err = (elb-std-deviation (mapcar #'car l))
		    do
		    (insert (apply #'format "|%s|%.2f|%.2f|%d|%.2f" test
				   (mapcar (lambda (x) (/ x runs))
					   (list (- test-elapsed test-gc-elapsed)
						 test-gc-elapsed test-gcs
						 test-elapsed))))
		    (insert (format "|%.2f\n" test-err))
		    summing test-elapsed into elapsed
		    summing test-gcs into gcs
		    summing test-gc-elapsed into gc-elapsed
		    collect test-err into errs
		    finally
		    (insert "|-\n")
		    (insert (apply #'format "|total|%.2f|%.2f|%d|%.2f"
				   (mapcar (lambda (x) (/ x runs))
					   (list (- elapsed gc-elapsed)
						 gc-elapsed gcs elapsed))))
		    (insert (format "|%.2f\n"
				    (sqrt (apply #'+ (mapcar (lambda (x)
							    (expt x 2))
							  errs))))))
	   (org-table-align)
	   (goto-char (point-min))
	   (if noninteractive
	       (message (buffer-string))
	     (org-mode)
	     (outline-show-subtree))))

(provide 'elisp-benchmarks)
;;; elisp-benchmarks.el ends here
