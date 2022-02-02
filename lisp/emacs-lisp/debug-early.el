;;; debug-early.el --- Dump a Lisp backtrace without frills  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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

;; This file dumps a backtrace on stderr when an error is thrown.
;; It has no dependencies on any Lisp libraries and is thus suitable
;; for generating backtraces in the early parts of bootstrapping.  It
;; is also good for generating backtraces in batch mode in general.

(defalias 'debug-early-backtrace
  #'(lambda ()
  "Print a trace of Lisp function calls currently active.
The output stream used is the value of `standard-output'.

This is a simplified version of the standard `backtrace'
function, intended for use in debugging the early parts
of the build process."
  (princ "\n")
  (mapbacktrace
   #'(lambda (evald func args _flags)
       (let ((args args))
	 (if evald
	     (progn
	       (princ "  ")
	       (prin1 func)
	       (princ "(")
	       (while args
		 (prin1 (car args))
		 (setq args (cdr args))
		 (if args
		     (princ " ")))
	       (princ ")\n"))
	   (while args
	     (princ "  ")
	     (prin1 (car args))
	     (princ "\n")
	     (setq args (cdr args)))))))))

(defalias 'debug-early
  #'(lambda (&rest args)
  "Print a trace of Lisp function calls currently active.
The output stream used is the value of `standard-output'.

There should be two ARGS, the symbol `error' and a cons of
the error symbol and its data.

This is a simplified version of `debug', intended for use
in debugging the early parts of the build process."
  (princ "\nError: ")
  (prin1 (car (car (cdr args))))	; The error symbol.
  (princ " ")
  (prin1 (cdr (car (cdr args))))	; The error data.
  (debug-early-backtrace)))

;;; debug-early.el ends here.
