;;; erb-task.el --- Emacs Regression Benchmarking -*- lexical-binding: t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Gemini Lasswell
;; Keywords: lisp, tools
;; Version: 1.0

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

;; ERB is a tool for automated benchmarking in Emacs Lisp.  This file
;; implements defining and running benchmark tasks within an Emacs
;; instance.

;; See the file erb.el for the rest of ERB, which implements a user
;; interface for building older versions of Emacs, running the
;; benchmark tasks in them, managing a database of results, and
;; presenting them.

;; For usage information, see ERB's info manual.

;; Significant changes to benchmark.el over the years:
;;   In Emacs 21 500ae43022, benchmark.el was added.
;;   In Emacs 23 e2bac5f625, benchmark-elapse was changed to use
;;     float-time and time-subtract.
;;   In Emacs 26 c7d2a0dd76, repetitions is allowed to be a symbol.

;;; Code:

;; Since it is necessary to load this file into older versions of
;; Emacs in order to define benchmark tasks for them to run, the code
;; in this file must avoid using features or libraries which are not
;; present in those older versions.
(require 'benchmark)

;;; Define benchmark tasks

(defmacro erb-deftask (name _arglist &rest docstring-keys-and-body)
  "Define NAME (a symbol) as a benchmark task.

BODY is evaluated as a `progn' when the task is run.  It should
contain a `erb-task-time' form wrapping the code to be
benchmarked.  Any setup or cleanup work done outside of the
`erb-task-time' form will not be benchmarked.

DOCSTRING-KEYS-AND-BODY may begin with an optional docstring and
an optional plist.  Valid keywords for use as properties in the
plist are:

:version

  A version number for this task, which should be increased if the
  task is changed sufficiently to invalidate previous measurements.

:rev-list

  A list of strings to use as arguments to git-rev-list(1) to get
  the list of commits for which this task should be run.

:discard-first-sample

  If non-nil, discard the result of the first run of the task.
  Use this if you notice the first sample is consistently much
  larger than the following samples.

:special

  If this exists and the value is `startup' a body for the task
  is not required, and the benchmark runner will instead time the
  startup and shutdown of Emacs.  If the value is `own-process',
  run this task in its own process instead of a process shared
  with other tasks."

  (declare (indent 2)
           (doc-string 3)
           (debug (&define :name task
                           name sexp [&optional stringp]
                           [&optional (&rest keywordp sexp)]
                           def-body)))
  (let ((documentation nil)
        (keys nil))
    (when (stringp (car-safe docstring-keys-and-body))
      (setq documentation (car docstring-keys-and-body))
      (pop docstring-keys-and-body))
    (when (keywordp (car-safe (car-safe docstring-keys-and-body)))
      (setq keys (car docstring-keys-and-body))
      (pop docstring-keys-and-body))
    `(progn
       (erb-task--set ',name
                      (erb-task--create-task ',name ,documentation ',keys
                                             (lambda ()
                                               ,@docstring-keys-and-body)))
       ',name)))

(defun erb-task--key-plist-p (list)
  "Return non-nil if LIST is a plist using keywords valid in ERB.
Those are :version, :rev-list, :discard-first-sample, and
:special."
  (while (consp list)
    (setq list (if (and (consp (cdr list))
                        (or (and (eq (car list) :version)
                                 (stringp (cadr list)))
                            (and (eq (car list) :rev-list)
                                 (listp (cadr list)))
                            (and (eq (car list) :special) (symbolp (cadr list)))
                            (eq (car list) :discard-first-sample)))
                   (cddr list)
                 'not-plist)))
  (null list))

(defvar erb-task--result nil)

(defmacro erb-task-time (&rest body)
  "Save timing results for BODY.
Use this macro inside of a benchmark task defined by
`benchmark-deftask' to define the code to be benchmarked.  Only
use it once per task."
  ;; TODO should this collect gc statistics?
  ;; as in (memory-use-counts) before and after,
  ;; do subtraction and sum
  `(progn
     (garbage-collect)
     (setq erb-task--result (benchmark-run ,@body))))

;;;  Internal representation of tasks

;; Use an alist so as not to have to worry about what
;; cl-defstruct was called in old versions of Emacs.
(defun erb-task--create-task (name doc keys body)
  (unless (erb-task--key-plist-p keys)
    (error "Keyword plist for %s contains unexpected keys"
           name))
  `((:name . ,name)
    (:documentation . ,doc)
    (:key-plist . ,keys)
    (:body . ,body)
    ,(cons :results nil)
    ,(cons :messages nil)))

(defsubst erb-task--name (task)
  (alist-get :name task))
(defsubst erb-task--documentation (task)
  (alist-get :documentation task))
(defsubst erb-task--body (task)
  (alist-get :body task))
(defsubst erb-task--key-plist (task)
  (alist-get :key-plist task))
(defsubst erb-task--results (task)
  (alist-get :results task))
(defsubst erb-task--add-result (result task)
  (push result (alist-get :results task)))
(defsubst erb-task--discard-result (task)
  (pop (alist-get :results task)))
(defsubst erb-task--messages (task)
  (alist-get :messages task))
(defsubst erb-task--add-message (message task)
  (push message (alist-get :messages task)))

(defun erb-task--boundp (symbol)
  "Return non-nil if SYMBOL names a task."
  (and (get symbol 'erb-task) t))

(defun erb-task--get-task (symbol)
  "If SYMBOL names a task, return that.  Signal an error otherwise."
  (unless (erb-task--boundp symbol)
    (error "No task named `%S'" symbol))
  (get symbol 'erb-task))

(defun erb-task--all-symbols ()
  (apropos-internal "" #'erb-task--boundp))

(defun erb-task--version (task)
  (plist-get (erb-task--key-plist task) :version))

(defun erb-task--rev-list (task)
  (plist-get (erb-task--key-plist task) :rev-list))

(defun erb-task--set (symbol definition)
  "Make SYMBOL name the task DEFINITION, and return DEFINITION."
  (when (eq symbol 'nil)
    (error "Attempt to define a task named nil"))
  (put symbol 'erb-task definition)
  definition)

(defun erb-task--make-unbound (symbol)
  "Make SYMBOL name no task.
Return SYMBOL."
  (put symbol 'erb-task nil)
  symbol)

(defun erb-delete-all-tasks ()
  "Make all symbols in `obarray' name no task."
  (interactive)
  (when (called-interactively-p 'any)
    (unless (y-or-n-p "Delete all tasks? ")
      (user-error "Aborted")))
  (mapc #'erb-task--make-unbound (erb-task--all-symbols)))

;;; Running tasks

(defvar erb-task-repetitions 10
  "Number of times to run each task.")

(defun erb-task-run-batch (symbols output-file)
  "Run defined benchmark tasks in batch mode.
SYMBOLS is a list of the names of the tasks.  Run each one
`erb-repetitions' times.  Write to OUTPUT-FILE an list of
results.  Each entry of the list will be of the form:

   ((name . NAME)
    (version . VERSION)
    (samples . SAMPLES-LIST)
    (messages . MESSAGES))

where NAME is the name of the task, VERSION is its version as
defined in the optional plist given to `erb-deftask',
SAMPLES-LIST is a list of the return values of benchmark-run, and
MESSAGES is a list of strings containing the messages issued
while the task was running.

If there were errors while running the task,
elements of SAMPLES-LIST will be of the form (error ERROR-INFO)
instead.  This function is used as a command-line entry point
into the target Emacs by `erb-run-start'."
  (let ((print-level nil)
        (print-length nil))
    (dolist (symbol symbols)
      (let* ((task (erb-task--get-task symbol))
             (key-plist (erb-task--key-plist task))
             (discard-first (plist-get key-plist :discard-first-sample)))
        (unless noninteractive
          (message "Running %s" symbol))
        (dotimes (i (+ erb-task-repetitions (if discard-first 1 0)))
          (erb-task--run symbol)
          (when (and discard-first (zerop i))
            (erb-task--discard-result task)))))

    (with-temp-file output-file
      (let ((results
             (mapcar (lambda (symbol)
                       (let ((task (erb-task--get-task symbol)))
                         `((name . ,symbol)
                           (version . ,(erb-task--version task))
                           (samples ,@(reverse (erb-task--results task)))
                           (messages ,@(reverse (erb-task--messages task))))))
                     symbols)))

        (insert (with-temp-buffer
                 (prin1 results (current-buffer))
                 (pp-buffer)
                 (buffer-string)))))))

(defun erb-task-run-all (&optional repetitions)
  "Run all defined benchmark tasks REPETITIONS times and message the results.
REPETITIONS defaults to 1."
  (interactive "p")
  (unless (natnump repetitions) (setq repetitions 1))
  (dotimes (_i repetitions)
    (mapc #'erb-task--run (erb-task--all-symbols)))
  (message "Results:")
  (mapc #'erb-task--message-results (erb-task--all-symbols)))

(defun erb-task--run (symbol)
  "Run the benchmark task associated with SYMBOL."
  (let ((task (erb-task--get-task symbol))
        (message-marker (with-current-buffer (messages-buffer)
                                (point-max-marker))))
    (condition-case err
        (progn
          (setq erb-task--result nil)
          (funcall (erb-task--body task)))
      (error (setq erb-task--result err)))
    (erb-task--add-result erb-task--result task)
    (erb-task--add-message (with-current-buffer (messages-buffer)
                             (buffer-substring message-marker (point-max)))
                           task)))

(defun erb-task--message-results (symbol)
  (message "%s: " symbol)
  (dolist (item (reverse (erb-task--results (erb-task--get-task symbol))))
    (message "  %s" item)))

(provide 'erb-task)
;;; erb-task.el ends here
