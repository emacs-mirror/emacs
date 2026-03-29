;;; em-basic.el --- basic shell builtin commands  -*- lexical-binding:t -*-

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

;; There are very few basic Eshell commands -- so-called builtins.
;; They are: echo, umask, and version.
;;
;;;_* `echo'
;;
;; The `echo' command repeats its arguments to the screen.  It is
;; optional whether this is done in a Lisp-friendly fashion (so that
;; the value of echo is useful to a Lisp command using the result of
;; echo as an argument), or whether it should try to act like a normal
;; shell echo, and always result in a flat string being returned.

;; An example of the difference is the following:
;;
;;   echo Hello world
;;
;; If `eshell-plain-echo-behavior' is non-nil, this will yield the
;; string "Hello world".  If Lisp behavior is enabled, however, it
;; will yield a list whose two elements are the strings "Hello" and
;; "world".  The way to write an equivalent expression for both would
;; be:
;;
;;   echo "Hello world"
;;
;; This always returns a single string.
;;
;;;_* `umask'
;;
;; The umask command changes the default file permissions for newly
;; created files.  It uses the same syntax as bash.

;;; Code:

(require 'esh-cmd)
(require 'esh-io)
(require 'esh-opt)
(require 'esh-util)

;;;###esh-module-autoload
(progn
(defgroup eshell-basic nil
  "The \"basic\" code provides a set of convenience functions which
are traditionally considered shell builtins.  Since all of the
functionality provided by them is accessible through Lisp, they are
not really builtins at all, but offer a command-oriented way to do the
same thing."
  :tag "Basic shell commands"
  :group 'eshell-module))

(defcustom eshell-plain-echo-behavior nil
  "If non-nil, `echo' tries to behave like an ordinary shell echo.
This comes at some detriment to Lisp functionality.  However, the Lisp
equivalent of `echo' can always be achieved by using `identity'."
  :type 'boolean
  :group 'eshell-basic)

;;; Functions:

(defun eshell-echo (args &optional output-newline)
  "Implementation code for a Lisp version of `echo'.
It returns a formatted value that should be passed to `eshell-print'
or `eshell-printn' for display."
  (if eshell-plain-echo-behavior
      (progn
        ;; If the output does not end in a newline, do not emit one.
        (setq eshell-ensure-newline-p nil)
        (concat (apply #'eshell-flatten-and-stringify args)
                (when output-newline "\n")))
    (let ((value
	   (cond
	    ((= (length args) 0) "")
	    ((= (length args) 1)
	     (car args))
	    (t
	     (mapcar
              (lambda (arg)
                (if (stringp arg)
                    (set-text-properties 0 (length arg) nil arg))
                arg)
	      args)))))
      (if output-newline
	  (cond
	   ((stringp value)
	    (concat value "\n"))
	   ((listp value)
	    (append value (list "\n")))
	   (t
	    (concat (eshell-stringify value) "\n")))
	value))))

(defun eshell/echo (&rest args)
  "Implementation of `echo'.  See `eshell-plain-echo-behavior'."
  (eshell-eval-using-options
   "echo" args
   '((?n nil (nil) output-newline
         "do not output the trailing newline")
     (?N nil (t)   output-newline
         "terminate with a newline")
     (?E nil nil   _disable-escapes
         "don't interpret backslash escapes (default)")
     (?h "help" nil nil
         "output this help screen")
     :preserve-args
     :usage "[OPTION]... [OBJECT]...")
   (if eshell-plain-echo-behavior
       (eshell-echo args (if output-newline (car output-newline) t))
     ;; In Emacs 28.1 and earlier, "-n" was used to add a newline to
     ;; non-plain echo in Eshell.  This caused confusion due to "-n"
     ;; generally having the opposite meaning for echo.  Retain this
     ;; compatibility for the time being.  For more info, see
     ;; bug#27361.
     (when (equal output-newline '(nil))
       (display-warning
        '(eshell echo)
        "To terminate with a newline, you should use -N instead."))
     (eshell-echo args output-newline))))

(defun eshell/printnl (&rest args)
  "Print out each of the arguments as strings, separated by newlines."
  (let ((elems (flatten-tree args)))
    (dolist (elem elems)
      (eshell-printn (eshell-stringify elem)))))

(defun eshell/listify (&rest args)
  "Return the argument(s) as a single list."
  (if (> (length args) 1)
      args
    (if (listp (car args))
	(car args)
      (list (car args)))))

(defun eshell/umask (&rest args)
  "Shell-like implementation of `umask'."
  (eshell-eval-using-options
   "umask" args
   '((?S "symbolic" nil symbolic-p "display umask symbolically")
     (?h "help" nil nil  "display this usage message")
     :preserve-args
     :usage "[-S] [mode]")
   (cond
    (args
     (let* ((mask (car args))
            (modes
             (if (stringp mask)
                 (if (string-match (rx bos (+ (any "0-7")) eos) mask)
                     (- #o777 (string-to-number mask 8))
                   (file-modes-symbolic-to-number
                    mask (default-file-modes)))
               (- #o777 mask))))
       (set-default-file-modes modes)
       (eshell-print
        "Warning: umask changed for all new files created by Emacs.\n")))
    (symbolic-p
     (let ((mode (default-file-modes)))
       (eshell-printn
        (format "u=%s,g=%s,o=%s"
                (concat (and (= (logand mode 64) 64) "r")
                        (and (= (logand mode 128) 128) "w")
                        (and (= (logand mode 256) 256) "x"))
                (concat (and (= (logand mode 8) 8) "r")
                        (and (= (logand mode 16) 16) "w")
                        (and (= (logand mode 32) 32) "x"))
                (concat (and (= (logand mode 1) 1) "r")
                        (and (= (logand mode 2) 2) "w")
                        (and (= (logand mode 4) 4) "x"))))))
    (t
     (eshell-printn (format "%03o" (logand (lognot (default-file-modes))
                                           #o777)))))
   nil))

(put 'eshell/umask 'eshell-no-numeric-conversions t)

(defun eshell/eshell-debug (&rest args)
  "A command for toggling certain debug variables."
  (eshell-eval-using-options
   "eshell-debug" args
   '((?h "help" nil nil "display this usage message")
     :usage "[KIND]...
This command is used to aid in debugging problems related to Eshell
itself.  It is not useful for anything else.  The recognized `kinds'
are:

   error       stops Eshell from trapping errors
   form        shows command form manipulation in `*eshell last cmd*'
   process     shows process events in `*eshell last cmd*'")
   (if args
       (dolist (kind args)
         (if (equal kind "error")
             (setq eshell-handle-errors (not eshell-handle-errors))
           (let ((kind-sym (intern kind)))
             (if (memq kind-sym eshell-debug-command)
                 (setq eshell-debug-command
                       (delq kind-sym eshell-debug-command))
               (push kind-sym eshell-debug-command)))))
     ;; Output the currently-enabled debug kinds.
     (unless eshell-handle-errors
       (eshell-print "errors\n"))
     (dolist (kind eshell-debug-command)
       (eshell-printn (symbol-name kind))))))

(defun pcomplete/eshell-mode/eshell-debug ()
  "Completion for the `debug' command."
  (while (pcomplete-here '("error" "form" "process"))))

(provide 'em-basic)
;;; em-basic.el ends here
