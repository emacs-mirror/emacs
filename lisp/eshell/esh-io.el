;;; esh-io.el --- I/O management  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2023 Free Software Foundation, Inc.

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
This is basically a speed enhancement, to avoid blocking the Lisp code
from executing while Emacs is redisplaying."
  :type 'integer
  :group 'eshell-io)

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
If the user specifies any of the filenames above as a redirection
target, the function in the second element will be called.

If the third element is non-nil, the redirection mode is passed as an
argument (which is the symbol `overwrite', `append' or `insert'), and
the function is expected to return another function -- which is the
output function.  Otherwise, the second element itself is the output
function.

The output function is then called repeatedly with single strings,
which represents successive pieces of the output of the command, until nil
is passed, meaning EOF."
  :version "30.1"
  :type '(repeat
	  (list (string :tag "Target")
		function
		(choice (const :tag "Func returns output-func" t)
			(const :tag "Func is output-func" nil))))
  :risky t
  :group 'eshell-io)

(define-error 'eshell-pipe-broken "Pipe broken")

;;; Internal Variables:

(defconst eshell-redirection-operators-alist
  '(("<"   . input)                     ; FIXME: Not supported yet.
    (">"   . overwrite)
    (">>"  . append)
    (">>>" . insert))
  "An association list of redirection operators to symbols
describing the mode, e.g. for using with `eshell-get-target'.")

(defvar eshell-current-handles nil)

(defvar eshell-last-command-status 0
  "The exit code from the last command.  0 if successful.")

(defvar eshell-last-command-result nil
  "The result of the last command.  Not related to success.")

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
  (when (not eshell-current-quoted)
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
               (list (list 'ignore
                           (append (car tt) (list (cadr tt))))))
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
            (cl-incf (cdr output-target))
            output-target)))
    (aset handles eshell-output-handle (list output-target t))
    (aset handles eshell-error-handle (list error-target t))
    handles))

(defun eshell-duplicate-handles (handles &optional steal-p)
  "Create a duplicate of the file handles in HANDLES.
This uses the targets of each handle in HANDLES, incrementing its
reference count by one (unless STEAL-P is non-nil).  These
targets are shared between the original set of handles and the
new one, so the targets are only closed when the reference count
drops to 0 (see `eshell-close-handles').

This function also sets the DEFAULT field for each handle to
t (see `eshell-create-handles').  Unlike the targets, this value
is not shared with the original handles."
  (let ((dup-handles (make-vector eshell-number-of-handles nil)))
    (dotimes (idx eshell-number-of-handles)
      (when-let ((handle (aref handles idx)))
        (unless steal-p
          (cl-incf (cdar handle)))
        (aset dup-handles idx (list (car handle) t))))
    dup-handles))

(defun eshell-protect-handles (handles)
  "Protect the handles in HANDLES from a being closed."
  (dotimes (idx eshell-number-of-handles)
    (when-let ((handle (aref handles idx)))
      (cl-incf (cdar handle))))
  handles)

(defun eshell-close-handles (&optional exit-code result handles)
  "Close all of the current HANDLES, taking refcounts into account.
If HANDLES is nil, use `eshell-current-handles'.

EXIT-CODE is the process exit code (zero, if the command
completed successfully).  If nil, then use the exit code already
set in `eshell-last-command-status'.

RESULT is the quoted value of the last command.  If nil, then use
the value already set in `eshell-last-command-result'."
  (when exit-code
    (setq eshell-last-command-status exit-code))
  (when result
    (cl-assert (eq (car result) 'quote))
    (setq eshell-last-command-result (cadr result)))
  (let ((handles (or handles eshell-current-handles))
        (succeeded (= eshell-last-command-status 0)))
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
               (= (cl-decf (cdar handle)) 0))
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
        (cl-decf (cdar handle))
        (setcar handle (cons nil 1)))
      (catch 'eshell-null-device
        (let ((current (caar handle))
              (where (eshell-get-target target mode)))
          (unless (member where current)
            (setcar (car handle) (append current (list where))))))
      (setcar (cdr handle) nil))))

(defun eshell-copy-output-handle (index index-to-copy &optional handles)
  "Copy the handle INDEX-TO-COPY to INDEX for the current HANDLES.
If HANDLES is nil, use `eshell-current-handles'."
  (let* ((handles (or handles eshell-current-handles))
         (handle-to-copy (car (aref handles index-to-copy))))
    (when handle-to-copy
      (cl-incf (cdr handle-to-copy)))
    (eshell-close-handle (aref handles index) nil)
    (setcar (aref handles index) handle-to-copy)))

(defun eshell-set-all-output-handles (mode &optional target handles)
  "Set output and error HANDLES to point to TARGET using MODE.
If HANDLES is nil, use `eshell-current-handles'."
  (eshell-set-output-handle eshell-output-handle mode target handles)
  (eshell-copy-output-handle eshell-error-handle eshell-output-handle handles))

(defun eshell-close-target (target status)
  "Close an output TARGET, passing STATUS as the result.
STATUS should be non-nil on successful termination of the output."
  (cond
   ((symbolp target) nil)

   ;; If we were redirecting to a file, save the file and close the
   ;; buffer.
   ((markerp target)
    (let ((buf (marker-buffer target)))
      (when buf                         ; somebody's already killed it!
	(save-current-buffer
	  (set-buffer buf)
	  (when eshell-output-file-buffer
	    (save-buffer)
	    (when (eq eshell-output-file-buffer t)
	      (or status (set-buffer-modified-p nil))
	      (kill-buffer buf)))))))

   ;; If we're redirecting to a process (via a pipe, or process
   ;; redirection), send it EOF so that it knows we're finished.
   ((eshell-processp target)
    ;; According to POSIX.1-2017, section 11.1.9, when communicating
    ;; via terminal, sending EOF causes all bytes waiting to be read
    ;; to be sent to the process immediately.  Thus, if there are any
    ;; bytes waiting, we need to send EOF twice: once to flush the
    ;; buffer, and a second time to cause the next read() to return a
    ;; size of 0, indicating end-of-file to the reading process.
    ;; However, some platforms (e.g. Solaris) actually require sending
    ;; a *third* EOF.  Since sending extra EOFs while the process is
    ;; running are a no-op, we'll just send the maximum we'd ever
    ;; need.  See bug#56025 for further details.
    (let ((i 0)
          ;; Only call `process-send-eof' once if communicating via a
          ;; pipe (in truth, this just closes the pipe).
          (max-attempts (if (process-tty-name target 'stdin) 3 1)))
      (while (and (<= (cl-incf i) max-attempts)
                  (eq (process-status target) 'run))
        (process-send-eof target))))

   ;; A plain function redirection needs no additional arguments
   ;; passed.
   ((functionp target)
    (funcall target status))

   ;; But a more complicated function redirection (which can only
   ;; happen with aliases at the moment) has arguments that need to be
   ;; passed along with it.
   ((consp target)
    (apply (car target) status (cdr target)))))

(defun eshell-kill-append (string)
  "Call `kill-append' with STRING, if it is indeed a string."
  (if (stringp string)
      (kill-append string nil)))

(defun eshell-clipboard-append (string)
  "Call `kill-append' with STRING, if it is indeed a string."
  (if (stringp string)
      (let ((select-enable-clipboard t))
	(kill-append string nil))))

(defun eshell-get-target (target &optional mode)
  "Convert TARGET, which is a raw argument, into a valid output target.
MODE is either `overwrite', `append' or `insert'; if it is omitted or nil,
it defaults to `insert'."
  (setq mode (or mode 'insert))
  (cond
   ((stringp target)
    (let ((redir (assoc target eshell-virtual-targets)))
      (if redir
	  (if (nth 2 redir)
	      (funcall (nth 1 redir) mode)
	    (nth 1 redir))
	(let* ((exists (get-file-buffer target))
	       (buf (find-file-noselect target t)))
	  (with-current-buffer buf
	    (if buffer-file-read-only
		(error "Cannot write to read-only file `%s'" target))
	    (setq buffer-read-only nil)
            (setq-local eshell-output-file-buffer
                        (if (eq exists buf) 0 t))
	    (cond ((eq mode 'overwrite)
		   (erase-buffer))
		  ((eq mode 'append)
		   (goto-char (point-max))))
	    (point-marker))))))


   ((bufferp target)
    (with-current-buffer target
      (cond ((eq mode 'overwrite)
             (erase-buffer))
            ((eq mode 'append)
             (goto-char (point-max))))
      (point-marker)))

   ((functionp target) nil)

   ((symbolp target)
    (if (eq mode 'overwrite)
	(set target nil))
    target)

   ((or (eshell-processp target)
	(markerp target))
    target)

   (t
    (error "Invalid redirection target: %s"
	   (eshell-stringify target)))))

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

(defvar eshell-print-queue nil)
(defvar eshell-print-queue-count -1)

(defsubst eshell-print (object)
  "Output OBJECT to the standard output handle."
  (eshell-output-object object eshell-output-handle))

(defun eshell-flush (&optional reset-p)
  "Flush out any lines that have been queued for printing.
Must be called before printing begins with -1 as its argument, and
after all printing is over with no argument."
  (ignore
   (if reset-p
       (setq eshell-print-queue nil
	     eshell-print-queue-count reset-p)
     (if eshell-print-queue
	 (eshell-print eshell-print-queue))
     (eshell-flush 0))))

(defun eshell-init-print-buffer ()
  "Initialize the buffered printing queue."
  (eshell-flush -1))

(defun eshell-buffered-print (&rest strings)
  "A buffered print -- *for strings only*."
  (if (< eshell-print-queue-count 0)
      (progn
	(eshell-print (apply 'concat strings))
	(setq eshell-print-queue-count 0))
    (if (= eshell-print-queue-count eshell-print-queue-size)
	(eshell-flush))
    (setq eshell-print-queue
	  (concat eshell-print-queue (apply 'concat strings))
	  eshell-print-queue-count (1+ eshell-print-queue-count))))

(defsubst eshell-error (object)
  "Output OBJECT to the standard error handle."
  (eshell-output-object object eshell-error-handle))

(defsubst eshell-errorn (object)
  "Output OBJECT followed by a newline to the standard error handle."
  (eshell-error object)
  (eshell-error "\n"))

(defsubst eshell-printn (object)
  "Output OBJECT followed by a newline to the standard output handle."
  (eshell-print object)
  (eshell-print "\n"))

(defun eshell-output-object-to-target (object target)
  "Insert OBJECT into TARGET.
Returns what was actually sent, or nil if nothing was sent."
  (cond
   ((functionp target)
    (funcall target object))

   ((symbolp target)
    (if (eq target t)                   ; means "print to display"
	(eshell-interactive-print (eshell-stringify object))
      (if (not (symbol-value target))
	  (set target object)
	(setq object (eshell-stringify object))
	(if (not (stringp (symbol-value target)))
	    (set target (eshell-stringify
			 (symbol-value target))))
	(set target (concat (symbol-value target) object)))))

   ((markerp target)
    (if (buffer-live-p (marker-buffer target))
	(with-current-buffer (marker-buffer target)
	  (let ((moving (= (point) target)))
	    (save-excursion
	      (goto-char target)
	      (unless (stringp object)
		(setq object (eshell-stringify object)))
	      (insert-and-inherit object)
	      (set-marker target (point-marker)))
	    (if moving
		(goto-char target))))))

   ((eshell-processp target)
    (unless (stringp object)
      (setq object (eshell-stringify object)))
    (condition-case err
        (process-send-string target object)
      (error
       ;; If `process-send-string' raises an error and the process has
       ;; finished, treat it as a broken pipe.  Otherwise, just
       ;; re-throw the signal.
       (if (memq (process-status target)
		 '(run stop open closed))
           (signal (car err) (cdr err))
         (signal 'eshell-pipe-broken (list target))))))

   ((consp target)
    (apply (car target) object (cdr target))))
  object)

(defun eshell-output-object (object &optional handle-index handles)
  "Insert OBJECT, using HANDLE-INDEX specifically.
If HANDLE-INDEX is nil, output to `eshell-output-handle'.
HANDLES is the set of file handles to use; if nil, use
`eshell-current-handles'."
  (let ((targets (caar (aref (or handles eshell-current-handles)
                             (or handle-index eshell-output-handle)))))
    (dolist (target targets)
      (eshell-output-object-to-target object target))))

(provide 'esh-io)
;;; esh-io.el ends here
