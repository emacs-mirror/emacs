;;; benchmark.el --- support for benchmarking code  -*- lexical-binding: t -*-

;; Copyright (C) 2003-2022 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: lisp, extensions

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

;; Utilities for timing the execution of forms, including the time
;; taken for GC.  Note that prior to timing code you may want to
;; ensure things like:  there has just been a GC, the relevant code is
;; already loaded (so that there's no overhead from autoloading etc.),
;; and the code is compiled if appropriate (but see
;; `benchmark-run-compiled').

;;; Code:

(eval-when-compile (require 'subr-x))   ;For `named-let'.

(defmacro benchmark-elapse (&rest forms)
  "Return the time in seconds elapsed for execution of FORMS."
  (declare (indent 0) (debug t))
  (let ((t1 (make-symbol "t1")))
    `(let ((,t1 (current-time)))
       ,@forms
       (float-time (time-since ,t1)))))

;;;###autoload
(defun benchmark-call (func &optional repetitions)
  "Measure the run time of calling FUNC a number REPETITIONS of times.
The result is a list (TIME GC GCTIME)
where TIME is the total time it took, in seconds.
GCTIME is the amount of time that was spent in the GC
and GC is the number of times the GC was called.

REPETITIONS can also be a floating point number, in which case it
specifies a minimum number of seconds that the benchmark execution
should take.  In that case the return value is prepended with the
number of repetitions actually used."
  (if (floatp repetitions)
      (benchmark--adaptive func repetitions)
    (unless repetitions (setq repetitions 1))
    (let ((gc gc-elapsed)
	  (gcs gcs-done)
	  (empty-func (lambda () 'empty-func)))
      (list
       (if (> repetitions 1)
	   (- (benchmark-elapse (dotimes (_ repetitions) (funcall func)))
	      (benchmark-elapse (dotimes (_ repetitions) (funcall empty-func))))
	 (- (benchmark-elapse (funcall func))
            (benchmark-elapse (funcall empty-func))))
       (- gcs-done gcs)
       (- gc-elapsed gc)))))

(defun benchmark--adaptive (func time)
  "Measure the run time of FUNC, calling it enough times to last TIME seconds.
Result is (REPETITIONS . DATA) where DATA is as returned by `branchmark-call'."
  (named-let loop ((repetitions 1)
                   (data (let ((x (list 0))) (setcdr x x) x)))
    ;; (message "Running %d iteration" repetitions)
    (let ((newdata (benchmark-call func repetitions)))
      (if (<= (car newdata) 0)
          ;; This can happen if we're unlucky, e.g. the process got preempted
          ;; (or the GC ran) just during the empty-func loop.
          ;; Just try again, hopefully this won't repeat itself.
          (progn
            ;; (message "Ignoring the %d iterations" repetitions)
            (loop (* 2 repetitions) data))
        (let* ((sum (cl-mapcar #'+ data (cons repetitions newdata)))
               (totaltime (nth 1 sum)))
          (if (>= totaltime time)
              sum
            (let* ((iter-time (/ totaltime (car sum)))
                   (missing-time (- time totaltime))
                   (missing-iter (/ missing-time iter-time)))
              ;; `iter-time' is approximate because of effects like the GC,
              ;; so multiply at most by 10, in case we are wildly off the mark.
              (loop (max repetitions
                         (min (ceiling missing-iter)
                              (* 10 repetitions)))
                    sum))))))))

;;;###autoload
(defmacro benchmark-run (&optional repetitions &rest forms)
  "Time execution of FORMS.
If REPETITIONS is supplied as a number, run FORMS that many times,
accounting for the overhead of the resulting loop.  Otherwise run
FORMS once.
Return a list of the total elapsed time for execution, the number of
garbage collections that ran, and the time taken by garbage collection.
See also `benchmark-run-compiled'."
  (declare (indent 1) (debug t))
  (unless (or (natnump repetitions) (and repetitions (symbolp repetitions)))
    (setq forms (cons repetitions forms)
	  repetitions 1))
  `(benchmark-call (lambda () ,@forms) ,repetitions))

;;;###autoload
(defmacro benchmark-run-compiled (&optional repetitions &rest forms)
  "Time execution of compiled version of FORMS.
This is like `benchmark-run', but what is timed is a funcall of the
byte code obtained by wrapping FORMS in a `lambda' and compiling the
result.  The overhead of the `lambda's is accounted for."
  (declare (indent 1) (debug t))
  (unless (or (natnump repetitions) (and repetitions (symbolp repetitions)))
    (setq forms (cons repetitions forms)
	  repetitions 1))
  `(benchmark-call (byte-compile '(lambda () ,@forms)) ,repetitions))

;;;###autoload
(defun benchmark (repetitions form)
  "Print the time taken for REPETITIONS executions of FORM.
Interactively, REPETITIONS is taken from the prefix arg, and
the command prompts for the form to benchmark.
For non-interactive use see also `benchmark-run' and
`benchmark-run-compiled'.
FORM can also be a function in which case we measure the time it takes
to call it without any argument."
  (interactive "p\nxForm: ")
  (let ((result (benchmark-call (eval (pcase form
                                        ((or `#',_ `(lambda . ,_)) form)
                                        (_ `(lambda () ,form)))
                                      t)
                                repetitions)))
    (if (zerop (nth 1 result))
	(message "Elapsed time: %fs" (car result))
      (message "Elapsed time: %fs (%fs in %d GCs)" (car result)
	       (nth 2 result) (nth 1 result)))))

;;;###autoload
(defmacro benchmark-progn (&rest body)
  "Evaluate BODY and message the time taken.
The return value is the value of the final form in BODY."
  (declare (debug body) (indent 0))
  (let ((value (make-symbol "value"))
	(start (make-symbol "start"))
	(gcs (make-symbol "gcs"))
	(gc (make-symbol "gc")))
    `(let ((,gc gc-elapsed)
	   (,gcs gcs-done)
           (,start (current-time))
           (,value (progn
                     ,@body)))
       (message "Elapsed time: %fs%s"
                (float-time (time-since ,start))
                (if (> (- gcs-done ,gcs) 0)
                    (format " (%fs in %d GCs)"
	                    (- gc-elapsed ,gc)
	                    (- gcs-done ,gcs))
                  ""))
       ;; Return the value of the body.
       ,value)))

(provide 'benchmark)

;;; benchmark.el ends here
