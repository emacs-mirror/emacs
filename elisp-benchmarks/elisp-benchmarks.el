;;; elisp-benchmarks.el --- elisp benchmarks collection -*- lexical-binding:t -*-

;; Copyright (C) 2019-2022  Free Software Foundation, Inc.

;; Author: Andrea Corallo <akrl@sdf.org>
;; Maintainer: Andrea Corallo <akrl@sdf.org>
;; Version: 1.14
;; Keywords: languages, lisp
;; Package-Type: multi
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
(if (featurep 'native-compile)
    (require 'comp)
  (defvar native-comp-speed))

(defgroup elb nil
  "Emacs Lisp benchmarks."
  :group 'lisp)

(defcustom elb-runs 3
  "Total number of benchmark iterations."
  :type 'number)

(defcustom elb-speed 3
  "Default `native-comp-speed' to be used for native compiling the benchmarks."
  :type 'number)

(defconst elb-bench-directory
  (expand-file-name "benchmarks/"
	            (file-name-directory
	             (if (fboundp 'macroexp-file-name)
	                 (macroexp-file-name)
	               (or load-file-name buffer-file-name)))))

(defconst elb-result-buffer-name "elisp-benchmarks-results"
  "Buffer name where results are presented.")

(defun elb-std-deviation (list)
  "Return the standard deviation of the elements in LIST."
  (let* ((n (length list))
	 (mean (/ (cl-loop for x in list
			   sum x)
		  (max n 1))))
    (sqrt (/ (cl-loop for x in list
		   sum (expt (- x mean) 2))
	  (1- n)))))

;;;###autoload
(cl-defun elisp-benchmarks-run (&optional selector (recompile t) runs)
  "Run all the benchmarks and present the results.
If non nil SELECTOR is a regexp to match the benchmark names to be executed.
The test is repeated RUNS number of times.  If RUNS is nil `elb-runs' is used as
default.
RECOMPILE all the benchmark folder when non nil."
  (interactive
   (when current-prefix-arg
     (list (read-regexp "Run benchmark matching: "))))
  (let* ((native-comp-speed elb-speed)
	 (compile-function (if (featurep 'native-compile)
			       #'native-compile
			     #'byte-compile-file))
	 (res (make-hash-table :test #'equal))
	 (sources (directory-files elb-bench-directory t "\\.el\\'"))
	 (test-sources (if selector
			   (cl-loop for f in sources
				    when (string-match selector f)
				    collect f)
			 sources)))
    ;; Compile
    (when recompile
      (mapc (lambda (f)
	      (message "Compiling... %s" f)
	      (funcall compile-function f))
	    test-sources))
    ;; Load
    (mapc (lambda (file)
	    (with-demoted-errors "Error loading: %S"
	      (load file)))
	  (mapcar (if (and (featurep 'native-compile)
			   (fboundp 'comp-el-to-eln-filename))
		      ;; FIXME: Isn't the elc->eln
                      ;; remapping fully automatic?
		      #'comp-el-to-eln-filename
		    #'file-name-sans-extension)
		  test-sources))
    (let ((tests (let ((names '()))
	           (mapatoms (lambda (s)
	                      (let ((name (symbol-name s)))
	                        (when (and (fboundp s)
	                                   (string-match
	                                    "\\`elb-\\(.*\\)-entry\\'" name))
	                          (push (match-string 1 name) names)))))
	           (sort names #'string-lessp))))
      ;; (cl-loop for test in tests
      ;;          do (puthash test () res))
      (cl-loop with runs = (or runs elb-runs)
	       repeat runs
	       for i from 1
	       named test-loop
	       do
	       (message "Iteration number: %d" i)
	       (cl-loop for test in tests
		        for entry-point = (intern (concat "elb-" test "-entry"))
		        do
		        (garbage-collect)
		        (message "Running %s..." test)
		        (let ((time
		               (with-demoted-errors "Error running: %S"
			        (eval `(benchmark-run nil (,entry-point)) t))))
		          (when time
			    (push time (gethash test res)))))
	       finally
	       (setq debug-on-error t)

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
	         (outline-show-subtree))))))

(provide 'elisp-benchmarks)
;;; elisp-benchmarks.el ends here
