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

;;; Code:

(require 'esh-io)

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
      (cl-call-next-method))
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
        (cl-call-next-method)
        (setf (eshell-worker-status target) 'exit)
        (eshell-close-handles)
        (declare-function eshell-kill-process-function "esh-proc" (proc status))
        (eshell-kill-process-function target "finished\n")))))

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

(provide 'esh-worker)
;;; esh-worker.el ends here
