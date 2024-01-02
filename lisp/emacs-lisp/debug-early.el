;;; debug-early.el --- Dump a Lisp backtrace without frills  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author: Alan Mackenzie <acm@muc.de>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: internal, backtrace, bootstrap.
;; Package: emacs

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

;; This file dumps a backtrace on stderr when an error is thrown.  It
;; has no dependencies on any Lisp libraries and is thus used for
;; generating backtraces for bugs in the early parts of bootstrapping.
;; It is also always used in batch model.  It was introduced in Emacs
;; 29, before which there was no backtrace available during early
;; bootstrap.

;;; Code:

(defalias 'debug-early-backtrace
  #'(lambda ()
      "Print a trace of Lisp function calls currently active.
The output stream used is the value of `standard-output'.

This is a simplified version of the standard `backtrace'
function, intended for use in debugging the early parts
of the build process."
      (princ "\n")
      (let ((print-escape-newlines t)
            (print-escape-control-characters t)
            (print-escape-nonascii t)
            (prin1 (if (and (fboundp 'cl-prin1)
                            (fboundp 'cl-defmethod) ;Used by `cl-print'.
                            (condition-case nil
                                (require 'cl-print)
                              (error nil)))
                       #'cl-prin1
                     #'prin1)))
        (mapbacktrace
         #'(lambda (evald func args _flags)
             (let ((args args))
	       (if evald
	           (progn
	             (princ "  ")
	             (funcall prin1 func)
	             (princ "("))
	         (progn
	           (princ "  (")
	           (setq args (cons func args))))
	       (if args
	           (while (progn
	                    (funcall prin1 (car args))
	                    (setq args (cdr args)))
	             (princ " ")))
	       (princ ")\n")))))))

(defalias 'debug-early
  #'(lambda (&rest args)
  "Print an error message with a backtrace of active Lisp function calls.
The output stream used is the value of `standard-output'.

The Emacs core calls this function after an error has been
signaled, and supplies two ARGS.  These are the symbol
`error' (which is ignored) and a cons of the error symbol and the
error data.

`debug-early' is a simplified version of `debug', and is
available during the early parts of the build process.  It is
superseded by `debug' after enough Lisp has been loaded to
support the latter, except in batch mode which always uses
`debug-early'.

\(In versions of Emacs prior to Emacs 29, no backtrace was
available before `debug' was usable.)"
  (princ "\nError: ")
  (prin1 (car (car (cdr args))))	; The error symbol.
  (princ " ")
  (prin1 (cdr (car (cdr args))))	; The error data.
  (debug-early-backtrace)))

;;; debug-early.el ends here.
