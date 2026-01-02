;;; esh-io.el --- I/O management  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2026 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;; At the moment, only output redirection is supported in Eshell.  To
;; use input redirection, the following syntax will work, assuming
;; that the command after the pipe is always an external command:
;;
;;   cat <file> | <command>
;;
;; Otherwise, output redirection and piping are provided in a manner
;; consistent with most shells.  Therefore, only unique features are
;; mentioned here.
;;
;;;_* Redirect to a Buffer or Process
;;
;; Buffers and processes can be named with '#<buffer buffer-name>' and
;; '#<process process-name>', respectively.  As a shorthand,
;; '#<buffer-name>' without the explicit "buffer" arg is equivalent to
;; '#<buffer buffer-name>'.
;;
;;   echo hello > #<buffer *scratch*> # Overwrite '*scratch*' with 'hello'.
;;   echo hello > #<*scratch*>        # Same as the command above.
;;
;;   echo hello > #<process shell> # Pipe "hello" into the shell process.
;;
;;;_* Insertion
;;
;; To insert at the location of point in a buffer, use '>>>':
;;
;;   echo alpha >>> #<buffer *scratch*>;
;;
;;;_* Pseudo-devices
;;
;; A few pseudo-devices are provided, since Emacs cannot write
;; directly to a UNIX device file:
;;
;;   echo alpha > /dev/null   ; the bit bucket
;;   echo alpha > /dev/kill   ; set the kill ring
;;   echo alpha >> /dev/clip  ; append to the clipboard
;;
;;;_* Multiple output targets
;;
;; Eshell can write to multiple output targets, including pipes.
;; Example:
;;
;;   (+ 1 2) > a > b > c   ; prints number to all three files
;;   (+ 1 2) > a | wc      ; prints to 'a', and pipes to 'wc'

;;; Code:

(require 'esh-arg)
(require 'esh-util)

(eval-when-compile
  (require 'cl-lib))

(declare-function eshell-interactive-print "esh-mode" (string))
(declare-function eshell-term-as-value "esh-cmd" (term))

(defgroup eshell-io nil
  "Eshell's I/O management code provides a scheme for treating many
different kinds of objects -- symbols, files, buffers, etc. -- as
though they were files."
  :tag "I/O management"
  :group 'eshell)

;;; User Variables:

(defcustom eshell-io-load-hook nil
  "A hook that gets run when `eshell-io' is loaded."
  :version "24.1"			; removed eshell-io-initialize
  :type 'hook
  :group 'eshell-io)

(defcustom eshell-number-of-handles 3
  "The number of file handles that eshell supports.
Currently this is standard input, output and error.  But even all of
these Emacs does not currently support with asynchronous processes
\(which is what eshell uses so that you can continue doing work in
other buffers)."
  :type 'integer
  :group 'eshell-io)

(defcustom eshell-output-handle 1
  "The index of the standard output handle."
  :type 'integer
  :group 'eshell-io)

(defcustom eshell-error-handle 2
  "The index of the standard error handle."
  :type 'integer
  :group 'eshell-io)

(defcustom eshell-print-queue-size 5
  "The size of the print queue, for doing buffered printing.
This variable is obsolete.  You should use `eshell-buffered-print-size'
instead."
  :type 'integer
  :group 'eshell-io)
(make-obsolete-variable 'eshell-print-queue-size
                        'eshell-buffered-print-size "30.1")

(defcustom eshell-buffered-print-size 2048
  "The size of the print queue in characters, for doing buffered printing.
Larger values for this option will generally result in faster execution
by reducing the overhead associated with each print operation, but will
increase the time it takes to see any progress in the output; smaller
values will do the reverse."
  :type 'integer
  :group 'eshell-io
  :version "30.1")

(defcustom eshell-buffered-print-redisplay-throttle 0.025
  "The minimum time in seconds between redisplays when using buffered printing.
If nil, don't redisplay while printing."
  :type '(choice number
                 (const :tag "Don't redisplay" nil))
  :group 'eshell-io
  :version "30.1")

(defcustom eshell-virtual-targets
  '(;; The literal string "/dev/null" is intentional here.  It just
    ;; provides compatibility so that users can redirect to
    ;; "/dev/null" no matter the actual value of `null-device'.
    ("/dev/null" (lambda (_mode) (throw 'eshell-null-device t)) t)
    ("/dev/eshell" eshell-interactive-print nil)
    ("/dev/kill" (lambda (mode)
                   (when (eq mode 'overwrite)
                     (kill-new ""))
                   #'eshell-kill-append)
     t)
    ("/dev/clip" (lambda (mode)
                   (when (eq mode 'overwrite)
                     (let ((select-enable-clipboard t))
                       (kill-new "")))
                   #'eshell-clipboard-append)
     t))
  "Map virtual devices name to Emacs Lisp functions.
Each member is of the following form:

  (FILENAME OUTPUT-FUNCTION [PASS-MODE])

When the user specifies FILENAME as a redirection target, Eshell will
repeatedly call the OUTPUT-FUNCTION with the redirected output as
strings.  OUTPUT-FUNCTION can also be an `eshell-generic-target'
instance.  In this case, Eshell will repeatedly call the function in the
`output-function' slot with the string output; once the redirection has
completed, Eshell will then call the function in the `close-function'
slot, passing the exit status of the redirected command.

If PASS-MODE is non-nil, Eshell will pass the redirection mode as an
argument (which is the symbol `overwrite', `append' or `insert') to
OUTPUT-FUNCTION, which should return the real output function (either an
ordinary function or `eshell-generic-target' as described above)."
  :version "30.1"
  :type '(repeat
	  (list (string :tag "Target")
		function
		(choice (const :tag "Func returns output-func" t)
			(const :tag "Func is output-func" nil))))
  :risky t
  :group 'eshell-io)

(define-error 'eshell-pipe-broken "Pipe broken")

(defvar eshell-ensure-newline-p nil
  "If non-nil, ensure that a newline is emitted after a Lisp form.
This can be changed by Lisp forms that are evaluated from the
Eshell command line.  This behavior only applies to line-oriented
output targets (see `eshell-target-line-oriented-p'.")

;;; Internal Variables:

(defconst eshell-redirection-operators-alist
  '(("<"   . input)                     ; FIXME: Not supported yet.
    (">"   . overwrite)
    (">>"  . append)
    (">>>" . insert))
  "An association list of redirection operators to symbols
describing the mode, e.g. for using with `eshell-get-target'.")

(defvar eshell-current-handles nil)

(defvar eshell-output-file-buffer nil
  "If non-nil, the current buffer is a file output buffer.")

(defvar eshell-print-count)
(defvar eshell-current-redirections)

;;; Functions:

(defun eshell-io-initialize ()      ;Called from `eshell-mode' via intern-soft!
  "Initialize the I/O subsystem code."
  (add-hook 'eshell-parse-argument-hook
            #'eshell-parse-redirection nil t)
  (make-local-variable 'eshell-current-redirections)
  (add-hook 'eshell-pre-rewrite-command-hook
            #'eshell-strip-redirections nil t)
  (add-function :filter-return (local 'eshell-post-rewrite-command-function)
                #'eshell--apply-redirections))

(defun eshell-parse-redirection ()
  "Parse an output redirection, such as `2>' or `>&'."
  (unless (or eshell-current-quoted
              eshell-current-argument-plain)
    (cond
     ;; Copying a handle (e.g. `2>&1').
     ((looking-at (rx (? (group digit))
                      (group (or "<" ">"))
                      "&" (group digit)
                      (* (syntax whitespace))))
      (let ((source (string-to-number (or (match-string 1) "1")))
            (mode (cdr (assoc (match-string 2)
                              eshell-redirection-operators-alist)))
            (target (string-to-number (match-string 3))))
        (when (eq mode 'input)
          (error "Eshell does not support input redirection"))
        (goto-char (match-end 0))
        (eshell-finish-arg (list 'eshell-copy-output-handle
                                 source target))))
     ;; Shorthand for redirecting both stdout and stderr (e.g. `&>').
     ((looking-at (rx (or (seq (group (** 1 3 ">")) "&")
                          (seq "&" (group-n 1 (** 1 3 ">"))))
                      (* (syntax whitespace))))
      (if eshell-current-argument
          (eshell-finish-arg)
        (goto-char (match-end 0))
        (eshell-finish-arg
         (let ((mode (cdr (assoc (match-string 1)
                                 eshell-redirection-operators-alist))))
           (list 'eshell-set-all-output-handles
                 (list 'quote mode))))))
     ;; Shorthand for piping both stdout and stderr (i.e. `|&').
     ((looking-at (rx "|&" (* (syntax whitespace))))
      (if eshell-current-argument
          (eshell-finish-arg)
        (goto-char (match-end 0))
        (eshell-finish-arg
         '(eshell-copy-output-handle eshell-error-handle
                                     eshell-output-handle)
         '(eshell-operator "|"))))
     ;; Regular redirecting (e.g. `2>').
     ((looking-at (rx (? (group digit))
                      (group (or "<" (** 1 3 ">")))
                      (* (syntax whitespace))))
      (if eshell-current-argument
          (eshell-finish-arg)
        (let ((source (if (match-string 1)
                          (string-to-number (match-string 1))
                        eshell-output-handle))
              (mode (cdr (assoc (match-string 2)
                                eshell-redirection-operators-alist))))
          (when (eq mode 'input)
            (error "Eshell does not support input redirection"))
          (goto-char (match-end 0))
          (eshell-finish-arg
           ;; Note: the target will be set later by
           ;; `eshell-strip-redirections'.
           (list 'eshell-set-output-handle
                 source (list 'quote mode)))))))))

(defun eshell-strip-redirections (terms)
  "Rewrite any output redirections in TERMS."
  (setq eshell-current-redirections (list t))
  (let ((tl terms)
        (tt (cdr terms)))
    (while tt
      (cond
       ;; Strip `eshell-copy-output-handle'.
       ((and (consp (car tt))
             (eq (caar tt) 'eshell-copy-output-handle))
        (nconc eshell-current-redirections
               (list (car tt)))
        (setcdr tl (cddr tt))
        (setq tt (cdr tt)))
       ;; Strip `eshell-set-output-handle' or
       ;; `eshell-set-all-output-handles' and the term immediately
       ;; after (the redirection target).
       ((and (consp (car tt))
             (memq (caar tt) '(eshell-set-output-handle
                               eshell-set-all-output-handles)))
        (unless (cdr tt)
          (error "Missing redirection target"))
        (nconc eshell-current-redirections
               `((ignore ,(append (car tt)
                                  (list (eshell-term-as-value (cadr tt)))))))
        (setcdr tl (cddr tt))
        (setq tt (cddr tt)))
       (t
        (setq tt (cdr tt)
              tl (cdr tl)))))
    (setq eshell-current-redirections
          (cdr eshell-current-redirections))))

(defun eshell--apply-redirections (cmd)
  "Apply any redirection which were specified for COMMAND."
  (if eshell-current-redirections
      `(progn
         ,@eshell-current-redirections
         ,cmd)
    cmd))

(defun eshell-create-handles
  (stdout output-mode &optional stderr error-mode)
  "Create a new set of file handles for a command.
The default target for standard output and standard error will
go to STDOUT and STDERR, respectively.  OUTPUT-MODE and
ERROR-MODE are either `overwrite', `append' or `insert'; a nil
value of mode defaults to `insert'.

The result is a vector of file handles.  Each handle is of the form:

  ((TARGETS . REF-COUNT) DEFAULT)

TARGETS is a list of destinations for output.  REF-COUNT is the
number of references to this handle (initially 1); see
`eshell-protect-handles' and `eshell-close-handles'.  DEFAULT is
non-nil if handle has its initial default value (always t after
calling this function)."
  (let* ((handles (make-vector eshell-number-of-handles nil))
         (output-target
          (let ((target (eshell-get-target stdout output-mode)))
            (cons (when target (list target)) 1)))
         (error-target
          (if stderr
              (let ((target (eshell-get-target stderr error-mode)))
                (cons (when target (list target)) 1))
            (incf (cdr output-target))
            output-target)))
    (aset handles eshell-output-handle (list output-target t))
    (aset handles eshell-error-handle (list error-target t))
    handles))

(defun eshell-duplicate-handles (handles &optional steal-p)
  "Create a duplicate of the file handles in HANDLES.
This uses the targets of each handle in HANDLES, incrementing its
reference count by one.  These targets are shared between the original
set of handles and the new one, so the targets are only closed when the
reference count drops to 0 (see `eshell-close-handles').

This function also sets the DEFAULT field for each handle to
t (see `eshell-create-handles').  Unlike the targets, this value
is not shared with the original handles."
  (declare (advertised-calling-convention (handles) "31.1"))
  (let ((dup-handles (make-vector eshell-number-of-handles nil)))
    (dotimes (idx eshell-number-of-handles)
      (when-let* ((handle (aref handles idx)))
        (unless steal-p
          (incf (cdar handle)))
        (aset dup-handles idx (list (car handle) t))))
    dup-handles))

(defun eshell-protect-handles (handles)
  "Protect the handles in HANDLES from a being closed."
  (dotimes (idx eshell-number-of-handles)
    (when-let* ((handle (aref handles idx)))
      (incf (cdar handle))))
  handles)

(declare-function eshell-exit-success-p "esh-cmd")

(defun eshell-close-handles (&optional handles obsolete-1 obsolete-2)
  "Close all of the current HANDLES, taking refcounts into account.
If HANDLES is nil, use `eshell-current-handles'."
  (declare (advertised-calling-convention (&optional handles) "31.1"))
  (when (or obsolete-1 obsolete-2 (numberp handles))
    (declare-function eshell-set-exit-info "esh-cmd"
                      (&optional exit-code result))
    ;; In addition to setting the advertised calling convention, warn
    ;; if we get here.  A caller may have called with the right number
    ;; of arguments but the wrong type.
    (display-warning '(eshell close-handles)
                     "Called `eshell-close-handles' with obsolete arguments")
    ;; Here, HANDLES is really the exit code.
    (when (or handles obsolete-1)
      (eshell-set-exit-info (or handles 0) (cadr obsolete-1)))
    (setq handles obsolete-2))

  (let ((handles (or handles eshell-current-handles))
        (succeeded (eshell-exit-success-p)))
    (dotimes (idx eshell-number-of-handles)
      (eshell-close-handle (aref handles idx) succeeded))))

(defun eshell-close-handle (handle status)
  "Close a single HANDLE, taking refcounts into account.
This will pass STATUS to each target for the handle, which should
be a non-nil value on successful termination."
  (when handle
    (cl-assert (> (cdar handle) 0)
               "Attempted to close a handle with 0 references")
    (when (and (> (cdar handle) 0)
               (= (decf (cdar handle)) 0))
      (dolist (target (caar handle))
        (eshell-close-target target status))
      (setcar (car handle) nil))))

(defun eshell-set-output-handle (index mode &optional target handles)
  "Set handle INDEX for the current HANDLES to point to TARGET using MODE.
If HANDLES is nil, use `eshell-current-handles'.

If the handle is currently set to its default value (see
`eshell-create-handles'), this will overwrite the targets with
the new target.  Otherwise, it will append the new target to the
current list of targets."
  (when target
    (let* ((handles (or handles eshell-current-handles))
           (handle (or (aref handles index)
                       (aset handles index (list (cons nil 1) nil))))
           (defaultp (cadr handle)))
      (when defaultp
        (decf (cdar handle))
        (setcar handle (cons nil 1)))
      (let ((current (caar handle))
            (where (eshell-get-target target mode)))
        (when (and where (not (member where current)))
          (setcar (car handle) (append current (list where)))))
      (setcar (cdr handle) nil))))

(defun eshell-copy-output-handle (index index-to-copy &optional handles)
  "Copy the handle INDEX-TO-COPY to INDEX for the current HANDLES.
If HANDLES is nil, use `eshell-current-handles'."
  (let* ((handles (or handles eshell-current-handles))
         (handle-to-copy (car (aref handles index-to-copy))))
    (when handle-to-copy
      (incf (cdr handle-to-copy)))
    (eshell-close-handle (aref handles index) nil)
    (setcar (aref handles index) handle-to-copy)))

(defun eshell-set-all-output-handles (mode &optional target handles)
  "Set output and error HANDLES to point to TARGET using MODE.
If HANDLES is nil, use `eshell-current-handles'."
  (eshell-set-output-handle eshell-output-handle mode target handles)
  (eshell-copy-output-handle eshell-error-handle eshell-output-handle handles))

(defun eshell-kill-append (string)
  "Call `kill-append' with STRING, if it is indeed a string."
  (if (stringp string)
      (kill-append string nil)))

(defun eshell-clipboard-append (string)
  "Call `kill-append' with STRING, if it is indeed a string."
  (if (stringp string)
      (let ((select-enable-clipboard t))
	(kill-append string nil))))

(defun eshell-interactive-output-p (&optional index handles)
  "Return non-nil if the specified handle is bound for interactive display.
HANDLES is the set of handles to check; if nil, use
`eshell-current-handles'.

INDEX is the handle index to check.  If nil, check
`eshell-output-handle'.  If `all', check both
`eshell-output-handle' and `eshell-error-handle'."
  (let ((handles (or handles eshell-current-handles))
        (index (or index eshell-output-handle)))
    (if (eq index 'all)
        (and (equal (caar (aref handles eshell-output-handle)) '(t))
             (equal (caar (aref handles eshell-error-handle)) '(t)))
      (equal (caar (aref handles index)) '(t)))))

(defvar eshell--buffered-print-queue nil)
(defvar eshell--buffered-print-current-size nil)
(defvar eshell--buffered-print-next-redisplay nil)

(defvar eshell-print-queue nil)
(make-obsolete-variable 'eshell-print-queue
                        'eshell--buffered-print-queue "30.1")
(defvar eshell-print-queue-count -1)
(make-obsolete-variable 'eshell-print-queue-count
                        'eshell--buffered-print-current-size "30.1")

(defsubst eshell-print (object)
  "Output OBJECT to the standard output handle."
  (eshell-output-object object eshell-output-handle))

(defun eshell-init-print-buffer ()
  "Initialize the buffered printing queue."
  (declare (obsolete #'eshell-with-buffered-print "30.1"))
  (setq eshell--buffered-print-queue nil
        eshell--buffered-print-current-size 0))

(defun eshell-flush (&optional redisplay-now)
  "Flush out any text that has been queued for printing.
When printing interactively, this will call `redisplay' every
`eshell-buffered-print-redisplay-throttle' seconds so that the user can
see the progress.  If REDISPLAY-NOW is non-nil, call `redisplay' for
interactive output even if the throttle would otherwise prevent it."
  (ignore
   (when eshell--buffered-print-queue
     (eshell-print (apply #'concat eshell--buffered-print-queue))
     ;; When printing interactively (see `eshell-with-buffered-print'),
     ;; periodically redisplay so the user can see some progress.
     (when (and eshell--buffered-print-next-redisplay
                (or redisplay-now
                    (time-less-p eshell--buffered-print-next-redisplay
                                 (current-time))))
       (redisplay)
       (setq eshell--buffered-print-next-redisplay
             (time-add eshell--buffered-print-next-redisplay
                       eshell-buffered-print-redisplay-throttle)))
     (setq eshell--buffered-print-queue nil
           eshell--buffered-print-current-size 0))))

(defun eshell-buffered-print (&rest strings)
  "A buffered print -- *for strings only*.
When the buffer exceeds `eshell-buffered-print-size' in characters, this
will flush it using `eshell-flush' (which see)."
  (setq eshell--buffered-print-queue
        (nconc eshell--buffered-print-queue strings))
  (incf eshell--buffered-print-current-size
        (apply #'+ (mapcar #'length strings)))
  (when (> eshell--buffered-print-current-size eshell-buffered-print-size)
    (eshell-flush)))

(defmacro eshell-with-buffered-print (&rest body)
  "Initialize buffered printing for Eshell, and then evaluate BODY.
Within BODY, call `eshell-buffered-print' to perform output."
  (declare (indent 0))
  `(let ((eshell--buffered-print-queue nil)
         (eshell--buffered-print-current-size 0)
         (eshell--buffered-print-next-redisplay
          (when (and eshell-buffered-print-redisplay-throttle
                     (eshell-interactive-output-p))
            (time-add (current-time)
                      eshell-buffered-print-redisplay-throttle))))
     (unwind-protect
         ,@body
       (eshell-flush))))

(defsubst eshell-error (object)
  "Output OBJECT to the standard error handle."
  (eshell-output-object object eshell-error-handle))

(defsubst eshell-printn (object)
  "Output OBJECT followed by a newline to the standard output handle."
  (eshell-print object)
  (eshell-print "\n"))

(defsubst eshell-errorn (object)
  "Output OBJECT followed by a newline to the standard error handle."
  (eshell-error object)
  (eshell-error "\n"))

(defun eshell--output-maybe-n (object handle)
  "Output OBJECT to HANDLE.
For any line-oriented output targets on HANDLE, ensure the output
ends in a newline."
  (eshell-output-object object handle)
  (when (and eshell-ensure-newline-p
             (not (and (stringp object)
                       (string-suffix-p "\n" object))))
    (eshell-maybe-output-newline handle)))

(defsubst eshell-print-maybe-n (object)
  "Output OBJECT to the standard output handle.
For any line-oriented output targets, ensure the output ends in a
newline."
  (eshell--output-maybe-n object eshell-output-handle))

(defsubst eshell-error-maybe-n (object)
  "Output OBJECT to the standard error handle.
For any line-oriented output targets, ensure the output ends in a
newline."
  (eshell--output-maybe-n object eshell-error-handle))

(cl-defstruct (eshell-generic-target (:constructor nil))
  "An Eshell target.
This is mainly useful for creating virtual targets (see
`eshell-virtual-targets').")

(cl-defstruct (eshell-function-target
               (:include eshell-generic-target)
               (:constructor nil)
               (:constructor eshell-function-target-create
                             (output-function &optional close-function)))
  "An Eshell target that calls an OUTPUT-FUNCTION."
  output-function close-function)

(cl-defgeneric eshell-get-target (raw-target &optional _mode)
  "Convert RAW-TARGET, which is a raw argument, into a valid output target.
MODE is either `overwrite', `append' or `insert'; if it is omitted or nil,
it defaults to `insert'."
  (error "Invalid redirection target: %s" (eshell-stringify raw-target)))

(cl-defmethod eshell-get-target ((raw-target string) &optional mode)
  "Convert a string RAW-TARGET into a valid output target using MODE.
If TARGET is a virtual target (see `eshell-virtual-targets'),
return an `eshell-generic-target' instance; otherwise, return a
marker for a file named TARGET."
  (setq mode (or mode 'insert))
  (if-let* ((redir (assoc raw-target eshell-virtual-targets)))
      (let (target)
        (catch 'eshell-null-device
          (setq target (if (nth 2 redir)
                           (funcall (nth 1 redir) mode)
                         (nth 1 redir)))
          (unless (eshell-generic-target-p target)
            (setq target (eshell-function-target-create target))))
        target)
    (let ((exists (get-file-buffer raw-target))
          (buf (find-file-noselect raw-target t)))
      (with-current-buffer buf
        (when buffer-file-read-only
          (error "Cannot write to read-only file `%s'" raw-target))
          (setq buffer-read-only nil)
          (setq-local eshell-output-file-buffer
                      (if (eq exists buf) 0 t))
          (cond ((eq mode 'overwrite)
                 (erase-buffer))
                ((eq mode 'append)
                 (goto-char (point-max))))
          (point-marker)))))

(cl-defmethod eshell-get-target ((raw-target buffer) &optional mode)
  "Convert a buffer RAW-TARGET into a valid output target using MODE.
This returns a marker for that buffer."
  (with-current-buffer raw-target
    (cond ((eq mode 'overwrite)
           (erase-buffer))
          ((eq mode 'append)
           (goto-char (point-max))))
    (point-marker)))

(cl-defmethod eshell-get-target ((raw-target symbol) &optional mode)
  "Convert a symbol RAW-TARGET into a valid output target using MODE.
This returns RAW-TARGET, with its value initialized to nil if MODE is
`overwrite'."
  (when (eq mode 'overwrite)
    (set raw-target nil))
  raw-target)

(cl-defmethod eshell-get-target ((raw-target process) &optional _mode)
  "Convert a process RAW-TARGET into a valid output target.
This just returns RAW-TARGET."
  raw-target)

(cl-defmethod eshell-get-target ((raw-target marker) &optional _mode)
  "Convert a marker RAW-TARGET into a valid output target.
This just returns RAW-TARGET."
  raw-target)

(cl-defgeneric eshell-close-target (target status)
  "Close an output TARGET, passing STATUS as the result.
STATUS should be non-nil on successful termination of the output.")

(cl-defmethod eshell-close-target ((_target symbol) _status)
  "Close a symbol TARGET."
  nil)

(cl-defmethod eshell-close-target ((target marker) status)
  "Close a marker TARGET.
If TARGET was created from a file name, save and kill the buffer.
If status is nil, prompt before killing."
  (when (buffer-live-p (marker-buffer target))
    (with-current-buffer (marker-buffer target)
      (when eshell-output-file-buffer
        (save-buffer)
        (when (eq eshell-output-file-buffer t)
          (or status (set-buffer-modified-p nil))
          (kill-buffer))))))

(cl-defmethod eshell-close-target ((target process) _status)
  "Close a process TARGET."
  ;; According to POSIX.1-2017, section 11.1.9, when communicating via
  ;; terminal, sending EOF causes all bytes waiting to be read to be
  ;; sent to the process immediately.  Thus, if there are any bytes
  ;; waiting, we need to send EOF twice: once to flush the buffer, and
  ;; a second time to cause the next read() to return a size of 0,
  ;; indicating end-of-file to the reading process.  However, some
  ;; platforms (e.g. Solaris) actually require sending a *third* EOF.
  ;; Since sending extra EOFs to a running process is a no-op, we'll
  ;; just send the maximum we'd ever need.  See bug#56025 for further
  ;; details.
  (catch 'done
    (dotimes (_ (if (process-tty-name target 'stdin) 3 1))
      (unless (process-live-p target)
        (throw 'done nil))
      (process-send-eof target))))

(cl-defmethod eshell-close-target ((target eshell-function-target) status)
  "Close an Eshell function TARGET."
  (when-let* ((close-function (eshell-function-target-close-function target)))
    (funcall close-function status)))

(cl-defgeneric eshell-output-object-to-target (object target)
  "Output OBJECT to TARGET.
Returns what was actually sent, or nil if nothing was sent.")

(cl-defmethod eshell-output-object-to-target (object (_target (eql t)))
  "Output OBJECT to the display."
  (setq object (eshell-stringify object))
  (eshell-interactive-print object))

(cl-defmethod eshell-output-object-to-target (object (target symbol))
  "Output OBJECT to the value of the symbol TARGET."
  (if (not (and (boundp target) (symbol-value target)))
      (set target object)
    (setq object (eshell-stringify object))
    (if (not (stringp (symbol-value target)))
        (set target (eshell-stringify
                     (symbol-value target))))
    (set target (concat (symbol-value target) object)))
  object)

(cl-defmethod eshell-output-object-to-target (object (target marker))
  "Output OBJECT to the marker TARGET."
  (when (buffer-live-p (marker-buffer target))
    (with-current-buffer (marker-buffer target)
      (let ((moving (= (point) target)))
        (save-excursion
          (goto-char target)
          (unless (stringp object)
            (setq object (eshell-stringify object)))
          (insert-and-inherit object)
          (set-marker target (point-marker)))
        (when moving
          (goto-char target)))))
  object)

(cl-defmethod eshell-output-object-to-target (object (target process))
  "Output OBJECT to the process TARGET."
  (unless (stringp object)
    (setq object (eshell-stringify object)))
  (condition-case err
      (process-send-string target object)
    (error
     ;; If `process-send-string' raises an error and the process has
     ;; finished, treat it as a broken pipe.  Otherwise, just re-raise
     ;; the signal.  NOTE: When running Emacs in batch mode
     ;; (e.g. during regression tests), Emacs can abort due to SIGPIPE
     ;; here.  Maybe `process-send-string' should handle SIGPIPE even
     ;; in batch mode (bug#66186).
     (if (process-live-p target)
         (signal (car err) (cdr err))
       (signal 'eshell-pipe-broken (list target)))))
  object)

(cl-defmethod eshell-output-object-to-target (object
                                              (target eshell-function-target))
  "Output OBJECT to the Eshell function TARGET."
  (funcall (eshell-function-target-output-function target) object))

(cl-defgeneric eshell-target-line-oriented-p (_target)
  "Return non-nil if the specified TARGET is line-oriented.
Line-oriented targets are those that expect a newline after
command output when `eshell-ensure-newline-p' is non-nil."
  nil)

(cl-defmethod eshell-target-line-oriented-p ((_target (eql t)))
  "Return non-nil to indicate that the display is line-oriented."
  t)

(defun eshell-output-object (object &optional handle-index handles)
  "Insert OBJECT, using HANDLE-INDEX specifically.
If HANDLE-INDEX is nil, output to `eshell-output-handle'.
HANDLES is the set of file handles to use; if nil, use
`eshell-current-handles'."
  (let ((targets (caar (aref (or handles eshell-current-handles)
                             (or handle-index eshell-output-handle)))))
    (dolist (target targets)
      (eshell-output-object-to-target object target))))

(defun eshell-maybe-output-newline (&optional handle-index handles)
  "Maybe insert a newline, using HANDLE-INDEX specifically.
This inserts a newline for all line-oriented output targets.

If HANDLE-INDEX is nil, output to `eshell-output-handle'.
HANDLES is the set of file handles to use; if nil, use
`eshell-current-handles'."
  (let ((targets (caar (aref (or handles eshell-current-handles)
                             (or handle-index eshell-output-handle)))))
    (dolist (target targets)
      (when (eshell-target-line-oriented-p target)
        (eshell-output-object-to-target "\n" target)))))

(provide 'esh-io)
;;; esh-io.el ends here
