;;; em-script.el --- Eshell script files  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2025 Free Software Foundation, Inc.

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

;;; Code:

(require 'esh-mode)
(require 'esh-io)

;;;###esh-module-autoload
(progn
(defgroup eshell-script nil
  "This module allows for the execution of files containing Eshell
commands, as a script file."
  :tag "Running script files."
  :group 'eshell-module))

;;; User Variables:

(defcustom eshell-script-load-hook nil
  "A list of functions to call when loading `eshell-script'."
  :version "24.1"                       ; removed eshell-script-initialize
  :type 'hook
  :group 'eshell-script)

(defcustom eshell-login-script (expand-file-name "login" eshell-directory-name)
  "If non-nil, a file to invoke when starting up Eshell interactively.
This file should be a file containing Eshell commands, where comment
lines begin with `#'."
  :type 'file
  :group 'eshell-script)

(defcustom eshell-rc-script (expand-file-name "profile" eshell-directory-name)
  "If non-nil, a file to invoke whenever Eshell is started.
This includes when running `eshell-command'."
  :type 'file
  :group 'eshell-script)

;;; Functions:

(defun eshell-script-initialize ()  ;Called from `eshell-mode' via intern-soft!
  "Initialize the script parsing code."
  (setq-local eshell-interpreter-alist
              (cons (cons (lambda (file _args)
                            (and (file-regular-p file)
                                 (string= (file-name-nondirectory file)
                                          "eshell")))
                          'eshell/source)
                    eshell-interpreter-alist))
  (setq-local eshell-complex-commands
	(append '("source" ".") eshell-complex-commands))
  ;; these two variables are changed through usage, but we don't want
  ;; to ruin it for other modules
  (let (eshell-inside-quote-regexp
	eshell-outside-quote-regexp)
    (and (not (bound-and-true-p eshell-non-interactive-p))
	 eshell-login-script
	 (file-readable-p eshell-login-script)
	 (eshell-do-eval
	  `(eshell-commands ,(eshell--source-file eshell-login-script))
          t))
    (and eshell-rc-script
	 (file-readable-p eshell-rc-script)
	 (eshell-do-eval
	  `(eshell-commands ,(eshell--source-file eshell-rc-script))
          t))))

(defun eshell--source-file (file &optional args subcommand-p)
  "Return a Lisp form for executing the Eshell commands in FILE, passing ARGS.
If SUBCOMMAND-P is non-nil, execute this as a subcommand."
  (let ((cmd (eshell-parse-command `(:file . ,file))))
    (when subcommand-p
      (setq cmd `(eshell-as-subcommand ,cmd)))
    `(let ((eshell-command-name ',file)
           (eshell-command-arguments ',args)
           ;; Don't print subjob messages by default.  Otherwise, if
           ;; this function was called as a subjob, then *all* commands
           ;; in the script would print start/stop messages.
           (eshell-subjob-messages nil))
       ,cmd)))

(defun eshell-source-file (file &optional args subcommand-p)
  "Execute a series of Eshell commands in FILE, passing ARGS.
Comments begin with `#'."
  (declare (obsolete nil "30.1"))
  (throw 'eshell-replace-command
         (eshell--source-file file args subcommand-p)))

;;;###autoload
(defun eshell-execute-file (file &optional args destination)
  "Execute a series of Eshell commands in FILE, passing ARGS.
If DESTINATION is t, write the command output to the current buffer.  If
nil, don't write the output anywhere.  For any other value, output to
the corresponding Eshell target (see `eshell-get-target').

Comments begin with `#'."
  (let ((eshell-non-interactive-p t)
        (stdout (if (eq destination t) (current-buffer) destination)))
    (with-temp-buffer
      (eshell-mode)
      (eshell-do-eval
       `(let ((eshell-current-handles
               (eshell-create-handles ',stdout 'insert))
              (eshell-current-subjob-p))
          ,(eshell--source-file file args))
       t))))

(cl-defstruct (eshell-princ-target
               (:include eshell-generic-target)
               (:constructor nil)
               (:constructor eshell-princ-target-create
                             (&optional printcharfun)))
  "A virtual target calling `princ' (see `eshell-virtual-targets')."
  printcharfun)

(cl-defmethod eshell-output-object-to-target (object
                                              (target eshell-princ-target))
  "Output OBJECT to the `princ' function TARGET."
  (princ object (eshell-princ-target-printcharfun target)))

(cl-defmethod eshell-target-line-oriented-p ((_target eshell-princ-target))
  "Return non-nil to indicate that the display is line-oriented."
  t)

(cl-defmethod eshell-close-target ((_target eshell-princ-target) _status)
  "Close the `princ' function TARGET."
  nil)

;;;###autoload
(defun eshell-batch-file ()
  "Execute an Eshell script as a batch script from the command line.
Inside your Eshell script file, you can add the following at the
top in order to make it into an executable script:

  #!/usr/bin/env -S emacs --batch -f eshell-batch-file"
  (let ((file (pop command-line-args-left))
        (args command-line-args-left)
        (eshell-non-interactive-p t)
        (eshell-module-loading-messages nil)
        (eshell-virtual-targets
         (append `(("/dev/stdout" ,(eshell-princ-target-create) nil)
                   ("/dev/stderr" ,(eshell-princ-target-create
                                    #'external-debugging-output)
                   nil))
                 eshell-virtual-targets)))
    (setq command-line-args-left nil)
    (with-temp-buffer
      (eshell-mode)
      (eshell-do-eval
       `(let ((eshell-current-handles
               (eshell-create-handles "/dev/stdout" 'append
                                      "/dev/stderr" 'append))
              (eshell-current-subjob-p))
          ,(eshell--source-file file args))
       t))))

(defun eshell/source (file &rest args)
  "Source a FILE in a subshell environment."
  (throw 'eshell-replace-command
         (eshell--source-file file args t)))

(put 'eshell/source 'eshell-no-numeric-conversions t)

(defun eshell/. (file &rest args)
  "Source a FILE in the current environment."
  (throw 'eshell-replace-command
         (eshell--source-file file args)))

(put 'eshell/. 'eshell-no-numeric-conversions t)

(provide 'em-script)
;;; em-script.el ends here
