;;; trace.el --- tracing facility for Emacs Lisp functions  -*- lexical-binding: t -*-

;; Copyright (C) 1993, 1998, 2000-2022 Free Software Foundation, Inc.

;; Author: Hans Chalupsky <hans@cs.buffalo.edu>
;; Maintainer: emacs-devel@gnu.org
;; Created: 15 Dec 1992
;; Keywords: tools, lisp

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

;; LCD Archive Entry:
;; trace|Hans Chalupsky|hans@cs.buffalo.edu|
;; Tracing facility for Emacs Lisp functions|
;; 1993/05/18 00:41:16|2.0|~/packages/trace.el.Z|


;;; Commentary:

;; Introduction:
;; =============
;; A simple trace package that utilizes nadvice.el.  It generates trace
;; information in a Lisp-style fashion and inserts it into a trace output
;; buffer.  Tracing can be done in the background (or silently) so that
;; generation of trace output won't interfere with what you are currently
;; doing.

;; Restrictions:
;; =============
;; - Traced subrs when called interactively will always show nil as the
;;   value of their arguments.
;; - Only functions/macros/subrs that are called via their function cell will
;;   generate trace output, hence, you won't get trace output for:
;;   + Subrs called directly from other subrs/C-code
;;   + Compiled calls to subrs that have special byte-codes associated
;;     with them (e.g., car, cdr, ...)
;;   + Macros that were expanded during compilation
;; - All the restrictions that apply to nadvice.el

;; Usage:
;; ======
;; - To trace a function use `M-x trace-function', which will ask you for the
;;   name of the function/subr/macro to trace.
;; - If you want to trace a function that switches buffers or does other
;;   display oriented stuff use `M-x trace-function-background', which will
;;   generate the trace output silently in the background without popping
;;   up windows and doing other irritating stuff.
;; - `M-x trace-package' will ask you for a function name prefix, and trace
;;   (in the background) all matching functions.
;; - `M-x trace-regexp' will ask you for a function name pattern (regexp),
;;   and trace (in the background) all matching functions.
;; - `M-x trace-library' will ask you for a library name, and trace (in the
;;   background) all functions defined by that file.
;; - Interactively in all cases, a prefix argument can be used to prompt
;;   for the output buffer and context arguments and, for bulk tracing
;;   commands, whether or not the traces should be automatically updated
;;   after loading lisp files.
;; - To untrace a function use `M-x untrace-function'.
;; - To untrace multiple functions by prefix use `M-x untrace-package'.
;; - To untrace multiple functions by regexp use `M-x untrace-regexp'.
;; - To untrace multiple functions by file use `M-x untrace-library'.
;; - To untrace all currently traced functions use `M-x untrace-all'.
;; - To list all currently traced functions use `M-x trace-currently-traced'.

;; Examples:
;; =========
;;
;;  (defun fact (n)
;;    (if (= n 0) 1
;;      (* n (fact (1- n)))))
;;  fact
;;
;;  (trace-function 'fact)
;;  fact
;;
;;  Now, evaluating this...
;;
;;  (fact 4)
;;  24
;;
;;  ...will generate the following in *trace-buffer*:
;;
;;  1 -> fact: n=4
;;  | 2 -> fact: n=3
;;  | | 3 -> fact: n=2
;;  | | | 4 -> fact: n=1
;;  | | | | 5 -> fact: n=0
;;  | | | | 5 <- fact: 1
;;  | | | 4 <- fact: 1
;;  | | 3 <- fact: 2
;;  | 2 <- fact: 6
;;  1 <- fact: 24
;;
;;
;;  (defun ack (x y z)
;;    (if (= x 0)
;;        (+ y z)
;;      (if (and (<= x 2) (= z 0))
;;          (1- x)
;;        (if (and (> x 2) (= z 0))
;;            y
;;          (ack (1- x) y (ack x y (1- z)))))))
;;  ack
;;
;;  (trace-function 'ack)
;;  ack
;;
;;  Try this for some interesting trace output:
;;
;;  (ack 3 3 1)
;;  27
;;
;;
;; The following does something similar to the functionality of the package
;; log-message.el by Robert Potter, which is giving you a chance to look at
;; messages that might have whizzed by too quickly (you won't see subr
;; generated messages though):
;;
;; (trace-function-background 'message "*Message Log*")


;;; Change Log:

;; 2017-06-17  Phil Sainty
;;	* New commands `trace-package', `untrace-package', `trace-regexp',
;;	  `untrace-regexp', `trace-library', `untrace-library'.
;;      * Documentation added to the elisp reference manual.
;;
;; 2012-2014  Stefan Monnier, Glenn Morris
;;	* Adapted for nadvice.el
;;	* New `context' argument and display in trace buffer
;;	* `trace-function' renamed to (and now an alias of)
;;	  `trace-function-foreground'
;;
;; 2005-02-27  Stefan Monnier
;;	* New `inhibit-trace' variable
;;
;; 1998-04-05  Stephen Eglen
;;	* New customize group `trace'
;;
;; Revision 2.0 1993/05/18 00:41:16 hans
;;	* Adapted for advice.el 2.0; it now also works
;;	  for GNU Emacs-19 and Lemacs
;;	* Separate function `trace-function-background'
;;	* Separate pieces of advice for foreground and background tracing
;;	* Less insane handling of interactive trace buffer specification
;;	* String arguments and values are now printed properly
;;
;; Revision 1.1 1992/12/15 22:45:15 hans
;;	* Created, first public release


;;; Code:

(eval-when-compile (require 'cl-macs))

(defgroup trace nil
  "Tracing facility for Emacs Lisp functions."
  :prefix "trace-"
  :group 'lisp)

;;;###autoload
(defcustom trace-buffer "*trace-output*"
  "Trace output will by default go to that buffer."
  :type 'string)

;; Current level of traced function invocation:
(defvar trace-level 0)

;; Semi-cryptic name used for a piece of trace advice:
(defvar trace-advice-name 'trace-function\ )

;; Used to separate new trace output from previous traced runs:
(defvar trace-separator (format "%s\n" (make-string 70 ?=)))

(defvar inhibit-trace nil
  "If non-nil, all tracing is temporarily inhibited.")

;;;###autoload
(defun trace-values (&rest values)
  "Helper function to get internal values.
You can call this function to add internal values in the trace buffer."
  (unless inhibit-trace
    (with-current-buffer (get-buffer-create trace-buffer)
      (goto-char (point-max))
      (insert
       (trace-entry-message
        'trace-values trace-level values "")))))

(defun trace-entry-message (function level args context)
  "Generate a string that describes that FUNCTION has been entered.
LEVEL is the trace level, ARGS is the list of arguments passed to FUNCTION,
and CONTEXT is a string describing the dynamic context (e.g. values of
some global variables)."
  (let ((print-circle t)
        (print-escape-newlines t))
    (format "%s%s%d -> %S%s\n"
            (mapconcat #'char-to-string (make-string (max 0 (1- level)) ?|) " ")
            (if (> level 1) " " "")
            level
            ;; FIXME: Make it so we can click the function name to jump to its
            ;; definition and/or untrace it.
            (cons function args)
            (if context (format " [%s]" context) ""))))

(defun trace-exit-message (function level value context)
  "Generate a string that describes that FUNCTION has exited.
LEVEL is the trace level, VALUE value returned by FUNCTION,
and CONTEXT is a string describing the dynamic context (e.g. values of
some global variables)."
  (let ((print-circle t)
        (print-escape-newlines t))
    (format "%s%s%d <- %s: %S%s\n"
            (mapconcat 'char-to-string (make-string (1- level) ?|) " ")
            (if (> level 1) " " "")
            level
            function
            ;; Do this so we'll see strings:
            value
            (if context (format " [%s]" context) ""))))

(defvar trace--timer nil)

(defun trace--display-buffer (buf)
  (unless (or trace--timer
	      (get-buffer-window buf 'visible))
    (setq trace--timer
	  ;; Postpone the display to some later time, in case we
	  ;; can't actually do it now.
	  (run-with-timer 0 nil
			  (lambda ()
			    (setq trace--timer nil)
			    (display-buffer buf nil 0))))))


(defun trace-make-advice (function buffer background context)
  "Build the piece of advice to be added to trace FUNCTION.
FUNCTION is the name of the traced function.
BUFFER is the buffer where the trace should be printed.
BACKGROUND if nil means to display BUFFER.
CONTEXT, if non-nil, should be either a function or an expression
that returns extra info, which will be printed after the
arguments or return value in the trace."
  (setq context (if context
                    (if (functionp context)
                        context
                      (trace-make-context context))
                  (lambda () "")))
  (lambda (body &rest args)
    (let ((trace-level (1+ trace-level))
          (trace-buffer (get-buffer-create buffer))
          (deactivate-mark nil)         ;Protect deactivate-mark.
          (ctx (funcall context)))
      (unless inhibit-trace
        (with-current-buffer trace-buffer
          (setq-local page-delimiter (format "^%s" (regexp-quote trace-separator)))
          (setq-local window-point-insertion-type t)
          (unless background (trace--display-buffer trace-buffer))
          (goto-char (point-max))
          ;; Insert a separator from previous trace output:
          (if (= trace-level 1) (insert trace-separator))
          (insert
           (trace-entry-message
            function trace-level args ctx))))
      (let ((result))
        (unwind-protect
            (setq result (list (apply body args)))
          (unless inhibit-trace
            (let ((ctx (funcall context)))
              (with-current-buffer trace-buffer
                (unless background (trace--display-buffer trace-buffer))
                (goto-char (point-max))
                (insert
                 (trace-exit-message
                  function
                  trace-level
                  (if result (car result) '\!non-local\ exit\!)
                  ctx))))))
        (car result)))))

(defun trace-function-internal (function buffer background context)
  "Add trace advice for FUNCTION."
  (advice-add
   function :around
   (trace-make-advice function (or buffer trace-buffer) background context)
   `((name . ,trace-advice-name) (depth . -100))))

(defun trace-is-traceable-p (sym)
  "Whether the given symbol is a traceable function.
Autoloaded functions are traceable."
  (or (functionp sym) (macrop sym)))

(defun trace-is-traced-p (function)
  "Whether FUNCTION is currently traced."
  (advice-member-p trace-advice-name function))

(define-obsolete-function-alias 'trace-is-traced 'trace-is-traced-p "29.1")

(defun trace-currently-traced (&optional display-message)
  "Return the list of currently traced function symbols.
Interactively, display the list as a message."
  (interactive "p")
  (let ((tracelist (cl-loop for sym being the symbols
                            if (trace-is-traced-p sym)
                            collect sym)))
    (when display-message
      (message "%S" tracelist))
    tracelist))

(defun trace--read-function (prompt)
  "Read a function name, prompting with string PROMPT."
  (let ((default (function-called-at-point)))
    (intern (completing-read (format-prompt prompt default)
                             obarray 'trace-is-traceable-p t nil nil
                             (if default (symbol-name default))))))

(defun trace--read-library (&optional prompt)
  "Read a library name, prompting with string PROMPT."
  (completing-read
   (or prompt "Library: ")
   (apply-partially 'locate-file-completion-table
                    load-path (get-load-suffixes))))

(defun trace--read-extra-args ()
  "Read a buffer and a \"context\" (Lisp expression).
Return (BUFFER CONTEXT)."
  (list
   (read-buffer "Output to buffer" trace-buffer)
   (when-let ((exp (read-from-minibuffer
                    "Context expression: "
                    nil read-expression-map t
                    'read-expression-history "nil")))
     (trace-make-context exp))))

(defun trace-make-context (exp)
  "Return a context function for expression EXP."
  (lambda ()
    (let ((print-circle t)
          (print-escape-newlines t))
      (prin1-to-string (eval exp t)))))

;;;###autoload
(defun trace-function-foreground (function &optional buffer context)
  "Trace calls to function FUNCTION.
With a prefix argument, also prompt for the trace output BUFFER
\(default `trace-buffer'), and a Lisp expression CONTEXT.
When called from Lisp, CONTEXT should be a function of no arguments
which returns a value to insert into BUFFER during the trace.

Tracing a function causes every call to that function to insert
into BUFFER Lisp-style trace messages that display the function's
arguments and return values.  It also evaluates CONTEXT, if that is
non-nil, and inserts its value too.  For example, you can use this
to track the current buffer, or position of point.

This function creates BUFFER if it does not exist.  This buffer will
popup whenever FUNCTION is called.  Do not use this function to trace
functions that switch buffers, or do any other display-oriented
stuff - use `trace-function-background' instead.

Calling `trace-function-foreground' again for the same FUNCTION
will update the optional argument behaviours to respect the new
values.

To stop tracing a function, use `untrace-function' or `untrace-all'."
  (interactive
   (cons (trace--read-function "Trace function")
         (and current-prefix-arg (trace--read-extra-args))))
  (trace-function-internal function buffer nil context))

;;;###autoload
(defun trace-function-background (function &optional buffer context)
  "Trace calls to function FUNCTION, quietly.
This is like `trace-function-foreground', but without popping up
the output buffer or changing the window configuration."
  (interactive
   (cons (trace--read-function "Trace function in background")
         (and current-prefix-arg (trace--read-extra-args))))
  (trace-function-internal function buffer t context))

;;;###autoload
(defalias 'trace-function 'trace-function-foreground)

(defun untrace-function (function)
  "Remove trace from FUNCTION.  If FUNCTION was not traced this is a noop."
  (interactive
   (list (intern (completing-read "Untrace function: "
                                  obarray #'trace-is-traced-p t))))
  (advice-remove function trace-advice-name))

;;;###autoload
(defun trace-package (prefix &optional buffer context after-load)
  "Trace all functions with names starting with PREFIX.
For example, to trace all diff functions, do the following:

\\[trace-package] RET diff- RET

Background tracing is used.  Switch to the trace output buffer to
view the results.  For any autoload declarations matching PREFIX,
the associated function will be traced if and when it is defined.

With a prefix argument, also prompt for the optional arguments.
If AFTER-LOAD is non-nil then re-process PREFIX after loading any
file.  See `trace-function-foreground' for details of BUFFER and
CONTEXT, and of foreground vs background tracing.

Calling `trace-package' again for the same PREFIX will update the
optional argument behaviours to respect the new values.

See also `untrace-package'."
  ;; Derived in part from `elp-instrument-package'.
  (interactive
   (cons (completing-read "Prefix of package to trace: "
                          obarray #'trace-is-traceable-p)
         (and current-prefix-arg
              (nconc (trace--read-extra-args)
                     (list (y-or-n-p "Update traces after loading files?"))))))
  (when (zerop (length prefix))
    (error "Tracing all Emacs functions would render Emacs unusable"))
  (mapc (lambda (name)
          (trace-function-background (intern name) buffer context))
        (all-completions prefix obarray #'trace-is-traceable-p))
  (message
   "Tracing to %s.  Use %s to untrace a package, or %s to remove all traces."
   (or buffer trace-buffer)
   (substitute-command-keys "\\[untrace-package]")
   (substitute-command-keys "\\[untrace-all]"))
  ;; Handle `after-load' argument.
  (when after-load
    (trace--after-load 'prefix prefix buffer context)))

(defun untrace-package (prefix)
  "Remove all traces from functions with names starting with PREFIX.

See also `trace-package'."
  (interactive
   (list (completing-read "Prefix of package to untrace: "
                          obarray #'trace-is-traced-p)))
  (if (and (zerop (length prefix))
           (y-or-n-p "Remove all function traces?"))
      (untrace-all)
    (mapc (lambda (name)
            (untrace-function (intern name)))
          (all-completions prefix obarray #'trace-is-traced-p)))
  ;; Remove any `after-load' behaviour.
  (trace--remove-after-load 'prefix prefix))

;;;###autoload
(defun trace-regexp (regexp &optional buffer context after-load)
  "Trace all functions with names matching REGEXP.
For example, to trace indentation-related functions, you could try:

\\[trace-regexp] RET indent\\|offset RET

Warning: Do not attempt to trace all functions.  Tracing too many
functions at one time will render Emacs unusable.

Background tracing is used.  Switch to the trace output buffer to
view the results.  For any autoload declarations matching REGEXP,
the associated function will be traced if and when it is defined.

With a prefix argument, also prompt for the optional arguments.
If AFTER-LOAD is non-nil then re-process REGEXP after loading any
file.  See `trace-function-foreground' for details of BUFFER and
CONTEXT, and of foreground vs background tracing.

Calling `trace-regexp' again for the same REGEXP will update the
optional argument behaviours to respect the new values.

See also `untrace-regexp'."
  (interactive
   (cons (read-regexp "Regexp matching functions to trace: ")
         (and current-prefix-arg
              (nconc (trace--read-extra-args)
                     (list (y-or-n-p "Update traces after loading files?"))))))
  (when (member regexp '("" "." ".+" ".*"))
    ;; Not comprehensive, but it catches the most likely attempts.
    (error "Tracing all Emacs functions would render Emacs unusable"))
  (mapatoms
   (lambda (sym)
     (and (trace-is-traceable-p sym)
          (string-match-p regexp (symbol-name sym))
          (trace-function-background sym buffer context))))
  (message
   "Tracing to %s.  Use %s to untrace by regexp, or %s to remove all traces."
   (or buffer trace-buffer)
   (substitute-command-keys "\\[untrace-regexp]")
   (substitute-command-keys "\\[untrace-all]"))
  ;; Handle `after-load' argument.
  (when after-load
    (trace--after-load 'regexp regexp buffer context)))

(defun untrace-regexp (regexp)
  "Remove all traces from functions with names matching REGEXP.

See also `trace-regexp'."
  (interactive
   (list (read-regexp "Regexp matching functions to untrace: ")))
  (if (and (zerop (length regexp))
           (y-or-n-p "Remove all function traces?"))
      (untrace-all)
    (mapatoms
     (lambda (sym)
       (and (trace-is-traced-p sym)
            (string-match-p regexp (symbol-name sym))
            (untrace-function sym)))))
  ;; Remove any `after-load' behaviour.
  (trace--remove-after-load 'regexp regexp))

;;;###autoload
(defun trace-library (library &optional buffer context after-load)
  "Trace functions defined by LIBRARY.
For example, to trace tramp.el functions, you could use:

\\[trace-library] RET tramp RET

Background tracing is used.  Switch to the trace output buffer to
view the results.  For any autoload declarations with a file name
matching LIBRARY, the associated function will be traced if and
when it is defined.  (Autoload file names will not match if LIBRARY
specifies a longer, more specific path.)

With a prefix argument, also prompt for the optional arguments.
If AFTER-LOAD is non-nil then re-process LIBRARY after loading it
\(ensuring that all of its functions will be traced).  See
`trace-function-foreground' for details of BUFFER and CONTEXT,
and of foreground vs background tracing.

Calling `trace-library' again for the same LIBRARY will update the
optional argument behaviours to respect the new values.

See also `untrace-library'."
  (interactive
   (cons (trace--read-library)
         (and current-prefix-arg
              (nconc (trace--read-extra-args)
                     (list (y-or-n-p "Update traces after loading this library?"))))))
  ;; Build list of library functions and autoloads.
  (let ((defs (nconc (trace--library-defuns library)
                     (trace--library-autoloads library))))
    ;; Trace each of those definitions.
    (mapc (lambda (func)
            (trace-function-background func buffer context))
          defs))
  ;; Handle `after-load' argument.
  (when after-load
    (trace--after-load 'library library buffer context)))

(defun trace--library-defuns (library)
  "Returns a list of loaded function definitions associated with LIBRARY."
  (delq nil (mapcar (lambda (x)
                      (and (consp x)
                           (eq (car x) 'defun)
                           (cdr x)))
                    (cdr (load-history-filename-element
                          (load-history-regexp library))))))

(defun trace--library-autoloads (library)
  "Returns a list of all current autoloads associated with LIBRARY.

Autoload file names will not match if LIBRARY specifies a longer,
more specific path than that of the autoload declaration itself."
  (let* ((functions nil)
         (filepattern (load-history-regexp library))
         (predicate (apply-partially 'trace--library-provides-autoload-p
                                     filepattern)))
    (mapatoms (lambda (sym)
                (when (funcall predicate sym)
                  (push sym functions))))
    functions))

(defun trace--library-provides-autoload-p (filepattern sym)
  "Whether symbol SYM is an autoload associated with FILEPATTERN.

FILEPATTERN should be the result of calling `load-history-regexp'."
  (when (fboundp sym)
    (let ((f (symbol-function sym)))
      (and (autoloadp f)
           (string-match filepattern (cadr f))))))

(defun untrace-library (library)
  "Remove all traces from functions defined by LIBRARY.

See also `trace-library'."
  (interactive (list (trace--read-library)))
  ;; Remove traces from known LIBRARY defuns.
  ;; (Also process autoloads, in case LIBRARY is unloaded.)
  (let ((defs (nconc (trace--library-defuns library)
                     (trace--library-autoloads library))))
    (mapc (lambda (func)
            (when (trace-is-traced-p func)
              (untrace-function func)))
          defs))
  ;; Remove any `after-load' behaviour.
  (trace--remove-after-load 'library library))

(defvar trace--after-load-alist nil
  "List of trace types to update after loading.

Each list item has the form ((TYPE . VALUE) BUFFER CONTEXT),
where TYPE is one of the symbols `prefix', `regexp', or `library';
and VALUE is the respective first argument to `trace-package',
`trace-regexp', or `trace-library'; with BUFFER and CONTEXT being
the values of those arguments as they were passed to the same
function.")

(defun trace--after-load (type value &optional buffer context)
  "Arrange to update traces after libraries are loaded.

TYPE is one of the symbols `prefix', `regexp', or `library';
VALUE is the respective first argument to `trace-package',
`trace-regexp', or `trace-library'; and BUFFER and CONTEXT are
the values of those arguments as they were passed to the same
function.

Adds `trace--after-load-function' to `after-load-functions'."
  ;; Remove any existing spec for this (TYPE VALUE) key.
  (trace--remove-after-load type value)
  ;; Add the new spec.
  (push (list (cons type value) buffer context)
        trace--after-load-alist)
  ;; Arrange to call `trace--after-load-function'.
  (add-hook 'after-load-functions #'trace--after-load-function))

(defun trace--after-load-function (file)
  "React to FILE being loaded.  Callback for `after-load-functions'.

See also `trace--after-load'."
  (dolist (spec trace--after-load-alist)
    (cl-destructuring-bind ((type . value) buffer context)
        spec
      (cl-case type
        (prefix (trace-package value nil buffer context))
        (regexp (trace-regexp value nil buffer context))
        (library (when (string-match (load-history-regexp value) file)
                   (trace-library value nil buffer context)))))))

(defun trace--remove-after-load (type value)
  "Remove any (TYPE . VALUE) entry from `trace--after-load-alist'.

Remove `trace--after-load-function' from `after-load-functions'
if it is no longer needed."
  (setq trace--after-load-alist
        (cl-delete (cons type value) trace--after-load-alist
                   :key #'car :test #'equal))
  (unless trace--after-load-alist
    (remove-hook 'after-load-functions #'trace--after-load-function)))

(defun trace--remove-after-load-all ()
  "Reset `trace--after-load-alist'.
Remove `trace--after-load-function' from `after-load-functions'"
  (setq trace--after-load-alist nil)
  (remove-hook 'after-load-functions #'trace--after-load-function))

(defun untrace-all ()
  "Remove traces from all currently traced functions."
  (interactive)
  (mapatoms #'untrace-function)
  (trace--remove-after-load-all))

(provide 'trace)

;;; trace.el ends here
