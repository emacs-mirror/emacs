;;; esh-worker.el --- eshell workers  -*- lexical-binding:t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

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

;; Workers are an Emacs Lisp-based analogue to external processes,
;; allowing for piping command output into them to perform some
;; action. For example, to uppercase the output of some other command:
;;
;;   echo hi | #'upcase
;;
;;;_* `accumulate' worker
;;
;; Eshell provides several ways to pipe output into a Lisp function.
;; The simplest is the "accumulate" worker, which is the default
;; behavior when piping directly to a Lisp function as above. You can
;; also express this explicitly, as in:
;;
;;   echo hi | accumulate #'upcase
;;
;;;_* `map-lines' worker
;;
;; You can also map each line of output to a function, similar to how
;; `mapcar' works.  For example, you could "quote" some output for
;; pasting into an email like this:
;;
;;   cat some-file.txt | map-lines (lambda (i) (format "> %s" i))
;;
;; This also shows how you use lambda functions as pipe targets in
;; Eshell.
;;
;;;_* `apply-lines' worker
;;
;; Finally, you can apply each line of output as successive arguments to
;; a function.  For example, to sum up a list of numbers written one per
;; line:
;;
;;   cat numbers.txt | apply-lines #'+

;;; Code:

(require 'esh-io)

(declare-function eshell-set-exit-info "esh-cmd" (status &optional result))

(defgroup eshell-worker nil
  "Eshell workers provide a way to construct process-like objects in Emacs
Lisp that can serve as pipe targets, allowing you to manipulating other
command's output with ordinary Lisp."
  :tag "Worker support"
  :group 'eshell)

(cl-defstruct (eshell-worker
               (:constructor eshell-worker-create)
               (:copier nil))
  "A structure representing an abstract process-like object.
This can be inherited from to define the behavior of a running
task in Eshell."
  (status 'run)
  (eshell-buffer (progn
                   (cl-assert (derived-mode-p 'eshell-mode))
                   (current-buffer))
                 :read-only t)
  (output-handles (eshell-duplicate-handles eshell-current-handles))
  (ensure-newline-p t))

(cl-defgeneric eshell-get-pipe (_object)
  "Get the pipe associated with OBJECT, if any."
  nil)

(cl-defmethod eshell-get-pipe ((object eshell-worker)) object)
(cl-defmethod eshell-get-pipe ((object buffer)) object)
(cl-defmethod eshell-get-pipe ((object marker)) object)
(cl-defmethod eshell-get-pipe ((object process)) object)

(cl-defmethod eshell-get-pipe :extra "function" (object)
  (if (functionp object)
      (eshell/accumulate object)
    (cl-call-next-method)))

(cl-defmethod eshell-task-p ((_object eshell-worker))
  t)

(cl-defmethod eshell-task-status ((task eshell-worker))
  (eshell-worker-status task))

(cl-defmethod eshell-task-active-p ((task eshell-worker))
  (eq (eshell-worker-status task) 'run))

(cl-defmethod eshell-get-target ((raw-target eshell-worker) &optional _mode)
  "Convert a raw `eshell-worker' RAW-TARGET into a valid output target.
This just returns RAW-TARGET."
  raw-target)

(cl-defgeneric eshell-worker-name (_worker)
  "Return the program name for WORKER."
  nil)

(defsubst eshell-worker--error (worker error)
  (eshell-errorn (format "%s: %s" (eshell-worker-name worker)
                         (error-message-string error))))

(cl-defmethod eshell-output-object-to-target :around
  (_object (target eshell-worker))
  "Output OBJECT to the Eshell worker TARGET.
This method is called around any primary method and is responsible
for let-binding the proper value for `eshell-current-handles'."
  (unless (eq (eshell-worker-status target) 'run)
    (signal 'eshell-pipe-broken target))
  (let ((eshell-current-handles (eshell-worker-output-handles target))
        (eshell-ensure-newline-p (eshell-worker-ensure-newline-p target)))
    (with-current-buffer (eshell-worker-eshell-buffer target)
      (eshell-condition-case err
          (cl-call-next-method)
        (error
         (eshell-worker--error target err)
         (eshell-set-exit-info 1))))
    (setf (eshell-worker-ensure-newline-p target) eshell-ensure-newline-p)))

(cl-defmethod eshell-close-target :around ((target eshell-worker) _status)
  "Close the worker PROC, passing STATUS as the result.
This method is called around any primary method and is responsible
for let-binding the proper value for `eshell-current-handles',
closing the handles when done, and calling
`eshell-kill-process-function'."
  (when (eq (eshell-worker-status target) 'run)
    (let ((eshell-current-handles (eshell-worker-output-handles target))
          (eshell-ensure-newline-p (eshell-worker-ensure-newline-p target)))
      (with-current-buffer (eshell-worker-eshell-buffer target)
        (unwind-protect
            (eshell-condition-case err
                (cl-call-next-method)
              (error
               (eshell-worker--error target err)
               (eshell-set-exit-info 1)))
          (setf (eshell-worker-status target) 'exit)
          (eshell-close-handles)
          (declare-function eshell-kill-process-function "esh-proc"
                            (proc status))
          (eshell-kill-process-function target "finished\n"))))))

(defun eshell--apply-print (function args)
  "Call FUNCTION with Eshell-converted ARGS and print the result."
  (when-let* ((result (apply function (eshell-convert-args args function))))
    (eshell-print-maybe-n result)
    result))

;; Accumulate worker

(cl-defstruct (eshell-accumulate-worker
               (:include eshell-worker)
               (:constructor eshell-accumulate-worker-create))
  "A worker that calls a Lisp function once with all output.
When outputting data to this worker, it will accumulate all the text,
calling a Lisp FUNCTION once at the end.

When outputting non-string data types to this worker (e.g. lists), it
will first convert the data to a string before concatenating it.  If
this structure only receives a single non-string value as input, it will
pass the value unaltered to FUNCTION."
  (function nil :read-only t)
  (buffer-or-value nil))

(defsubst eshell-accumulate-worker--make-buffer (worker)
  "Ensure WORKER has an internal buffer to add strings to."
  (setf (eshell-accumulate-worker-buffer-or-value worker)
        (generate-new-buffer " *eshell-worker*" t)))

(cl-defmethod eshell-worker-name ((worker eshell-accumulate-worker))
  (symbol-name (eshell-accumulate-worker-function worker)))

(cl-defmethod eshell-output-object-to-target
  (object (target eshell-accumulate-worker))
  "Send OBJECT to the accumulate-worker TARGET.
This calls the function associated with the worker.

The returned value is the OBJECT in the form that it was actually
sent to TARGET (e.g. a string representing OBJECT)."
  (let ((buf-or-val (eshell-accumulate-worker-buffer-or-value target)))
    (cond
     ((bufferp buf-or-val)
      (with-current-buffer buf-or-val
        (insert (eshell-stringify object t))))
     (buf-or-val
      (cl-assert (listp buf-or-val))
      (with-current-buffer (eshell-accumulate-worker--make-buffer target)
        (insert (eshell-concat t (car buf-or-val) object))))
     ((stringp object)
      (with-current-buffer (eshell-accumulate-worker--make-buffer target)
        (insert object)))
     (t
      (setf (eshell-accumulate-worker-buffer-or-value target)
            (list object))))))

(cl-defmethod eshell-close-target ((target eshell-accumulate-worker) _status)
  "Close the accumulate-worker TARGET, flushing its buffer."
  (let* ((function (eshell-accumulate-worker-function target))
         (buf-or-val (eshell-accumulate-worker-buffer-or-value target))
         (input (if (bufferp buf-or-val)
                    (with-current-buffer buf-or-val
                      (eshell-mark-numeric-string (buffer-string)))
                  (car buf-or-val))))
    (eshell--apply-print function (list input))))

(defun eshell/accumulate (function)
  "Call FUNCTION once with all the accumulated Eshell output.
When outputting data to this worker, it will accumulate all the text,
calling FUNCTION once at the end.

When outputting non-string data types to this worker (e.g. lists), it
will first convert the data to a string before concatenating it.  If
this structure only receives a single non-string value as input, it will
pass the value unaltered to FUNCTION."
  (eshell-accumulate-worker-create :function function))

;; Map-lines worker

(cl-defstruct (eshell-map-lines-worker
               (:include eshell-worker)
               (:constructor eshell-map-lines-worker-create))
  "A worker that calls a Lisp function once for each line.
When outputting string data to this worker, it will call a Lisp
FUNCTION once per line of text.  When outputting other data types to
this worker (e.g. lists), it will call FUNCTION once with the
specified value."
  (function nil :read-only t)
  (current-line nil))

(cl-defgeneric eshell-map-lines-worker--apply (line target)
  (let ((function (eshell-map-lines-worker-function target)))
    (eshell--apply-print function (list line))))

(cl-defmethod eshell-worker-name ((worker eshell-map-lines-worker))
  (format "map-lines %s" (eshell-map-lines-worker-function worker)))

(cl-defmethod eshell-output-object-to-target
  (object (target eshell-map-lines-worker))
  "Send OBJECT to the map-lines worker TARGET.
This calls the function associated with the worker.

The returned value is the OBJECT in the form that it was actually
sent to TARGET (e.g. a string representing OBJECT)."
  (if (stringp object)
      (let (line-begin line-end line)
        ;; Prepend any saved text from the current line to the new text.
        (setq object (concat (eshell-map-lines-worker-current-line target)
                             object))
        ;; Pass each full line of text to FUNCTION.
        (while (setq line-end (string-search "\n" object line-begin))
          (setq line (eshell-mark-numeric-string
                      (substring object line-begin line-end))
                line-begin (1+ line-end))
          (eshell-map-lines-worker--apply line target))
        ;; Save any remaining text after the last newline for next time.
        (setf (eshell-map-lines-worker-current-line target)
              (and (length> object (or line-begin 0))
                   (substring object line-begin))))
    (eshell-map-lines-worker--apply object target)))

(cl-defmethod eshell-close-target ((target eshell-map-lines-worker) _status)
  (when-let* ((last-line (eshell-map-lines-worker-current-line target)))
    (eshell-map-lines-worker--apply (eshell-mark-numeric-string last-line)
                                    target)))

(defun eshell/map-lines (function)
  "Map each line of output of another command to FUNCTION.
When outputting string data to this worker, it will call FUNCTION once
per line of text.  When outputting other data types to this
worker (e.g. lists), it will call FUNCTION once with the specified
value."
  (eshell-map-lines-worker-create :function function))

;; Apply-lines worker

(cl-defstruct
    (eshell-apply-lines-worker
     (:include eshell-map-lines-worker)
     (:constructor eshell-apply-lines-worker-create))
  "A worker that calls a Lisp function with each line as an argument.
This worker calls a Lisp FUNCTION once, with each line of string data
corresponding to one argument passed to the fuction.  When outputting
other data types to this worker (e.g. lists), each object is passed as a
single argument to FUNCTION."
  args)                              ; Arguments stored in reverse order

(cl-defmethod eshell-worker-name ((worker eshell-apply-lines-worker))
  (format "apply-lines %s" (eshell-map-lines-worker-function worker)))

(cl-defmethod eshell-map-lines-worker--apply
    (line (target eshell-apply-lines-worker))
  (push line (eshell-apply-lines-worker-args target)))

(cl-defmethod eshell-close-target ((target eshell-apply-lines-worker) _status)
  (cl-call-next-method)
  (let ((function (eshell-map-lines-worker-function target)))
    (eshell--apply-print
     function (nreverse (eshell-apply-lines-worker-args target)))))

(defun eshell/apply-lines (function)
  "Call a Lisp FUNCTION with each line of output as an argument.
This worker calls a Lisp FUNCTION once, with each line of string data
corresponding to one argument passed to the fuction.  When outputting
other data types to this worker (e.g. lists), each object is passed as a
single argument to FUNCTION."
  (eshell-apply-lines-worker-create :function function))

(provide 'esh-worker)
;;; esh-worker.el ends here
