;;; comp-runtime.el --- runtime Lisp native compiler code  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

;; Author: Andrea Corallo <acorallo@gnu.org>
;; Keywords: lisp
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

;; While the main native compiler is implemented in comp.el, when
;; commonly used as a jit compiler it is only loaded by Emacs sub
;; processes performing async compilation.  This file contains all
;; the code needed to drive async compilations and any Lisp code
;; needed at runtime to run native code.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'comp-common)
(require 'bytecomp) ;; For `emacs-lisp-compilation-mode'.

(defgroup comp-run nil
  "Emacs Lisp native compiler runtime."
  :group 'lisp)

(defcustom native-comp-jit-compilation-deny-list
  '()
  "List of regexps to exclude matching files from deferred native compilation.
Files whose names match any regexp are excluded from native compilation."
  :type '(repeat regexp)
  :version "28.1")

(defcustom native-comp-async-jobs-number 0
  "Default number of subprocesses used for async native compilation.
Value of zero means to use half the number of the CPU's execution units,
or one if there's just one execution unit."
  :type 'natnum
  :risky t
  :version "28.1")

;; TODO If we could start compilations that were skipped if and when AC
;;      power is subsequently reconnected, we could consider changing
;;      the default to nil.  --spwhitton
(defcustom native-comp-async-on-battery-power t
  "Whether to start asynchronous native compilation while on battery power.
Customize this to nil to disable starting async compilations when AC
power is not connected.  Async compilations that are already running
when AC power is disconnected are not affected.  Compilations skipped
because AC power was disconnected are not started again if AC power is
subsequently reconnected; in most cases, those compilations will not be
tried again until after you next restart Emacs.

Customizing this to nil has no effect unless `battery-status-function'
can correctly detect the absence of connected AC power on your platform."
  :type 'boolean
  :version "31.1")

(defcustom native-comp-async-report-warnings-errors t
  "Whether to report warnings and errors from asynchronous native compilation.

When native compilation happens asynchronously, it can produce
warnings and errors, some of which might not be emitted by a
byte-compilation.  The typical case for that is native-compiling
a file that is missing some `require' of a necessary feature,
while having it already loaded into the environment when
byte-compiling.

As asynchronous native compilation always starts from a pristine
environment, it is more sensitive to such omissions, and might be
unable to compile such Lisp source files correctly.

Set this variable to nil to suppress warnings altogether, or to
the symbol `silent' to log warnings but not pop up the *Warnings*
buffer."
  :type '(choice
          (const :tag "Do not report warnings/errors" nil)
          (const :tag "Report and display warnings/errors" t)
          (const :tag "Report but do not display warnings/errors" silent))
  :version "28.1")

(defcustom native-comp-async-warnings-errors-kind 'important
  "Which kind of warnings and errors to report from async native compilation.

Setting this variable to `important' (the default) will report
only important warnings and all errors.
Setting this variable to `all' will report all warnings and
errors."
  :type '(choice
          (const :tag "Report all warnings/errors" all)
          (const :tag "Report important warnings and all errors" important))
  :version "30.1")

(defcustom native-comp-always-compile nil
  "Non-nil means unconditionally (re-)compile all files."
  :type 'boolean
  :version "28.1")

(make-obsolete-variable 'native-comp-deferred-compilation-deny-list
                        'native-comp-jit-compilation-deny-list
                        "29.1")

(defcustom native-comp-async-cu-done-functions nil
  "List of functions to call when asynchronous compilation of a file is done.
Each function is called with one argument FILE, the filename whose
compilation has completed."
  :type 'hook
  :version "28.1")

(defcustom native-comp-async-all-done-hook nil
  "Hook run after completing asynchronous compilation of all input files."
  :type 'hook
  :version "28.1")

(defcustom native-comp-async-query-on-exit nil
  "Whether to query the user about killing async compilations when exiting.
If this is non-nil, Emacs will ask for confirmation to exit and kill the
asynchronous native compilations if any are running.  If nil, when you
exit Emacs, it will silently kill those asynchronous compilations even
if `confirm-kill-processes' is non-nil."
  :type 'boolean
  :version "28.1")

(defconst comp-async-buffer-name "*Async-native-compile-log*"
  "Name of the async compilation buffer log.")

(defvar comp-no-spawn nil
  "Non-nil don't spawn native compilation processes.")

(defvar comp-async-compilations (make-hash-table :test #'equal)
  "Hash table file-name -> async compilation process.")

;; These variables and functions are defined in comp.c
(defvar comp--no-native-compile)
(defvar comp-deferred-pending-h)
(defvar comp-installed-trampolines-h)
(defvar native-comp-enable-subr-trampolines)

(declare-function comp--install-trampoline "comp.c")
(declare-function comp-el-to-eln-filename "comp.c")
(declare-function native-elisp-load "comp.c")

(defun native--compile-async-skip-p (file load selector)
  "Return non-nil if FILE's compilation should be skipped.

LOAD and SELECTOR work as described in `native--compile-async'."
  ;; Make sure we are not already compiling `file' (bug#40838).
  (or (gethash file comp-async-compilations)
      (gethash (file-name-with-extension file "elc") comp--no-native-compile)
      (cond
       ((null selector) nil)
       ((functionp selector) (not (funcall selector file)))
       ((stringp selector) (not (string-match-p selector file)))
       (t (error "SELECTOR must be a function a regexp or nil")))
      ;; Also exclude files from deferred compilation if
      ;; any of the regexps in
      ;; `native-comp-jit-compilation-deny-list' matches.
      (and (eq load 'late)
           (seq-some (lambda (re)
                      (string-match-p re file))
                    native-comp-jit-compilation-deny-list))))

(defvar battery-status-function)

(defun native--compile-skip-on-battery-p ()
  "Should we skip JIT compilation because we're running on battery power?"
  ;; The `battery-status-function' API is not specified so as to
  ;; render it cleanly machine-readable, so we resort to heuristics.
  ;; We could extend the API to return machine-readable information in
  ;; the alist when an optional boolean argument is provided to the
  ;; `battery-status-function'; we could use `func-arity' to check
  ;; whether a custom `battery-status-function' supports the extension.
  ;; However, so far in the time we've had battery.el, it would appear
  ;; that this is the first time we've wanted to use the information
  ;; other than just for generating messages.
  (and-let* (((not native-comp-async-on-battery-power))
             ((require 'battery))
             battery-status-function
             (res (funcall battery-status-function))
             ((or (member (cdr (assq ?L res)) '("off-line" "BAT" "Battery"))
                  ;; If %L has not given us what we need, we don't
                  ;; consider battery charge levels or percentages,
                  ;; because power users often configure their batteries
                  ;; to stop charging at less than 100% as a way to
                  ;; extend the lifetime of their battery hardware.
                  (string= (cdr (assq ?b res)) "+")
                  (member (cdr (assq ?B res)) '("charging" "pending-charge"))
                  (not (string= (cdr (assq ?B res)) "discharging")))))))

(defvar comp-files-queue ()
  "List of Emacs Lisp files to be compiled.")

(defun comp--async-runnings ()
  "Return the number of async compilations currently running.
This function has the side effect of cleaning-up finished
processes from `comp-async-compilations'"
  (cl-loop
   for file-name in (cl-loop
                     for file-name being each hash-key of comp-async-compilations
                     for prc = (gethash file-name comp-async-compilations)
                     unless (process-live-p prc)
                     collect file-name)
   do (remhash file-name comp-async-compilations))
  (hash-table-count comp-async-compilations))

(defvar comp-num-cpus nil)
(defun comp--effective-async-max-jobs ()
  "Compute the effective number of async jobs."
  (if (zerop native-comp-async-jobs-number)
      (or comp-num-cpus
          (setf comp-num-cpus
		(max 1 (/ (num-processors) 2))))
    native-comp-async-jobs-number))

(defvar-local comp-last-scanned-async-output nil)
;; From warnings.el
(defvar warning-suppress-types)
(defun comp--accept-and-process-async-output (process)
  "Accept PROCESS output and check for diagnostic messages."
  (if native-comp-async-report-warnings-errors
      (let ((warning-suppress-types
             (if (eq native-comp-async-report-warnings-errors 'silent)
                 (cons '(native-compiler) warning-suppress-types)
               warning-suppress-types))
            (regexp (if (eq native-comp-async-warnings-errors-kind 'all)
                        "^.*?\\(?:Error\\|Warning\\): .*$"
                      (rx bol
                          (*? nonl)
                          (or
                           (seq "Error: " (*? nonl))
                           (seq "Warning: the function ‘" (1+ (not "’"))
                                "’ is not known to be defined."))
                          eol))))
        (with-current-buffer (process-buffer process)
          (save-excursion
            (accept-process-output process)
            (goto-char (or comp-last-scanned-async-output (point-min)))
            (while (re-search-forward regexp nil t)
              (display-warning 'native-compiler (match-string 0)))
            (setq comp-last-scanned-async-output (point-max)))))
    (accept-process-output process)))

(defconst comp-valid-source-re (rx ".el" (? ".gz") eos)
  "Regexp to match filename of valid input source files.")

(defun comp--run-async-workers ()
  "Start compiling files from `comp-files-queue' asynchronously.
When compilation is finished, run `native-comp-async-all-done-hook' and
display a message."
  (cl-assert (null comp-no-spawn))
  (if (or comp-files-queue
          (> (comp--async-runnings) 0))
      (unless (or (>= (comp--async-runnings) (comp--effective-async-max-jobs))
                  (native--compile-skip-on-battery-p))
        (cl-loop
         for (source-file . load) = (pop comp-files-queue)
         while source-file
         do (cl-assert (string-match-p comp-valid-source-re source-file) nil
                       "`comp-files-queue' should be \".el\" files: %s"
                       source-file)
         when (or native-comp-always-compile
                  load ; Always compile when the compilation is
                       ; commanded for late load.
                  ;; Skip compilation if `comp-el-to-eln-filename' fails
                  ;; to find a writable directory.
                  (with-demoted-errors "Async compilation :%S"
                    (file-newer-than-file-p
                     source-file (comp-el-to-eln-filename source-file))))
         do (let* ((expr `((require 'comp)
                           (setq comp-async-compilation t
                                 warning-fill-column most-positive-fixnum)
                           ,(let ((set (list 'setq)))
                              (dolist (var '(comp-file-preloaded-p
                                             native-compile-target-directory
                                             native-comp-speed
                                             native-comp-debug
                                             native-comp-verbose
                                             comp-libgccjit-reproducer
                                             native-comp-eln-load-path
                                             native-comp-compiler-options
                                             native-comp-driver-options
                                             load-path
                                             backtrace-line-length
                                             byte-compile-warnings
                                             comp-sanitizer-emit
                                             ;; package-load-list
                                             ;; package-user-dir
                                             ;; package-directory-list
                                             ))
                                (when (boundp var)
                                  (push var set)
                                  (push `',(symbol-value var) set)))
                              (nreverse set))
                           ;; FIXME: Activating all packages would align the
                           ;; functionality offered with what is usually done
                           ;; for ELPA packages (and thus fix some compilation
                           ;; issues with some ELPA packages), but it's too
                           ;; blunt an instrument (e.g. we don't even know if
                           ;; we're compiling such an ELPA package at
                           ;; this point).
                           ;;(package-activate-all)
                           ,native-comp-async-env-modifier-form
                           (message "Compiling %s..." ,source-file)
                           (comp--native-compile ,source-file ,(and load t))))
                   (source-file1 source-file) ;; Make the closure works :/
                   (temp-file (make-temp-file
                               (concat "emacs-async-comp-"
                                       (file-name-base source-file) "-")
                               nil ".el"))
                   (expr-strings (let ((print-length nil)
                                       (print-level nil))
                                   (mapcar #'prin1-to-string expr)))
                   (_ (progn
                        (with-temp-file temp-file
                          (insert ";;; -*- lexical-binding: t -*-\n")
                          (mapc #'insert expr-strings))
                        (comp-log "\n")
                        (mapc #'comp-log expr-strings)))
                   (load1 load)
                   (default-directory invocation-directory)
                   (process (make-process
                             :name (concat "Compiling: " source-file)
                             :buffer (with-current-buffer
                                         (get-buffer-create
                                          comp-async-buffer-name)
                                       (unless (derived-mode-p 'compilation-mode)
                                         (emacs-lisp-compilation-mode))
			               (current-buffer))
                             :command (list
                                       (expand-file-name invocation-name
                                                         invocation-directory)
                                       "-no-comp-spawn" "-Q" "--batch"
                                       "--eval"
                                       ;; Suppress Abort dialogs on MS-Windows
                                       "(setq w32-disable-abort-dialog t)"
                                       "-l" temp-file)
                             :sentinel
                             (lambda (process _event)
                               (run-hook-with-args
                                'native-comp-async-cu-done-functions
                                source-file)
                               (comp--accept-and-process-async-output process)
                               (ignore-errors (delete-file temp-file))
                               (let ((eln-file (comp-el-to-eln-filename
                                                source-file1)))
                                 (when (and load1
                                            (zerop (process-exit-status
                                                    process))
                                            (file-exists-p eln-file))
                                   (native-elisp-load eln-file
                                                      (eq load1 'late))))
                               (comp--run-async-workers))
                             :noquery (not native-comp-async-query-on-exit))))
              (set-process-thread process nil)
              (puthash source-file process comp-async-compilations))
         when (>= (comp--async-runnings) (comp--effective-async-max-jobs))
         do (cl-return)))
    ;; No files left to compile and all processes finished.
    (run-hooks 'native-comp-async-all-done-hook)
    (with-current-buffer (get-buffer-create comp-async-buffer-name)
      (save-excursion
        (unless (derived-mode-p 'compilation-mode)
          (emacs-lisp-compilation-mode))
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "Compilation finished.\n"))))
    ;; `comp-deferred-pending-h' should be empty at this stage.
    ;; Reset it anyway.
    (clrhash comp-deferred-pending-h)))

(defconst comp-warn-primitives
  '(null memq gethash and subrp not native-comp-function-p
         comp--install-trampoline concat if symbolp symbol-name make-string
         length aset aref length> mapcar expand-file-name
         file-name-as-directory file-exists-p native-elisp-load)
  "List of primitives we want to warn about in case of redefinition.
This are essential for the trampoline machinery to work properly.")

(defun comp--trampoline-search (subr-name)
  "Search a trampoline file for SUBR-NAME.
Return the trampoline if found or nil otherwise."
  (cl-loop
   with rel-filename = (comp-trampoline-filename subr-name)
   for dir in (comp-eln-load-path-eff)
   for filename = (expand-file-name rel-filename dir)
   when (file-exists-p filename)
     do (cl-return (native-elisp-load filename))))

(declare-function comp-trampoline-compile "comp")
;;;###autoload
(defun comp-subr-trampoline-install (subr-name)
  "Make SUBR-NAME effectively advice-able when called from native code."
  (when (memq subr-name comp-warn-primitives)
    (warn "Redefining `%s' might break native compilation of trampolines."
          subr-name))
  (let ((subr (symbol-function subr-name)))
    (unless (or (not (string= subr-name (subr-name subr))) ;; (bug#69573)
                (null native-comp-enable-subr-trampolines)
                (memq subr-name native-comp-never-optimize-functions)
                (gethash subr-name comp-installed-trampolines-h))
      (cl-assert (subr-primitive-p subr))
      (when-let* ((trampoline (or (comp--trampoline-search subr-name)
                                  (comp-trampoline-compile subr-name))))
        (comp--install-trampoline subr-name trampoline)))))

;;;###autoload
(defun native--compile-async (files &optional recursively load selector)
  ;; BEWARE, this function is also called directly from C.
  "Compile FILES asynchronously.
FILES is one filename or a list of filenames or directories.

If optional argument RECURSIVELY is non-nil, recurse into
subdirectories of given directories.

If optional argument LOAD is non-nil, request to load the file
after compiling.

The optional argument SELECTOR has the following valid values:

nil -- Select all files.
a string -- A regular expression selecting files with matching names.
a function -- A function selecting files with matching names.

The variable `native-comp-async-jobs-number' specifies the number
of (commands) to run simultaneously.

LOAD can also be the symbol `late'.  This is used internally if
the byte code has already been loaded when this function is
called.  It means that we request the special kind of load
necessary in that situation, called \"late\" loading.

During a \"late\" load, instead of executing all top-level forms
of the original files, only function definitions are
loaded (paying attention to have these effective only if the
bytecode definition was not changed in the meantime)."
  (comp-ensure-native-compiler)
  (unless (member load '(nil t late))
    (error "LOAD must be nil, t or 'late"))
  (unless (listp files)
    (setf files (list files)))
  (let ((added-something nil)
        (old-comp-files-queue comp-files-queue)
        file-list)
    (dolist (file-or-dir files)
      (cond ((file-directory-p file-or-dir)
             (dolist (file (if recursively
                               (directory-files-recursively
                                file-or-dir comp-valid-source-re)
                             (directory-files file-or-dir
                                              t comp-valid-source-re)))
               (push file file-list)))
            ((file-exists-p file-or-dir) (push file-or-dir file-list))
            (t (signal 'native-compiler-error
                       (list "Not a file nor directory" file-or-dir)))))
    (dolist (file file-list)
      (if-let* ((entry (seq-find (lambda (x) (string= file (car x))) comp-files-queue)))
          ;; Most likely the byte-compiler has requested a deferred
          ;; compilation, so update `comp-files-queue' to reflect that.
          (unless (or (null load)
                      (eq load (cdr entry)))
            ;; IIUC, this is a non-destructive version of
            ;; (setcdr entry load)?
            (setf comp-files-queue
                  (cl-loop for i in comp-files-queue
                           with old = (car entry)
                           if (string= (car i) old)
                             collect (cons file load)
                           else
                             collect i)))

        (unless (native--compile-async-skip-p file load selector)
          (let* ((out-filename (comp-el-to-eln-filename file))
                 (out-dir (file-name-directory out-filename)))
            (unless (file-exists-p out-dir)
              (make-directory out-dir t))
            (if (file-writable-p out-filename)
                (setf comp-files-queue
                      (append comp-files-queue `((,file . ,load)))
                      added-something t)
              (display-warning 'native-compiler
                               (format "Cannot write %s; skipping."
                                       out-filename)))))))
    ;; Perhaps nothing passed `native--compile-async-skip-p'?
    (when (and added-something
               ;; If the queue was already non-empty, then we already
               ;; in the middle of processing the queue.
               (null old-comp-files-queue)
               ;; Don't start if there's one already running.
               (zerop (comp--async-runnings)))
      (comp--run-async-workers))))

;;;###autoload
(defun native-compile-async (files &optional recursively load selector)
  "Compile FILES asynchronously.
FILES is one file or a list of filenames or directories.

If optional argument RECURSIVELY is non-nil, recurse into
subdirectories of given directories.

If optional argument LOAD is non-nil, request to load the file
after compiling.

The optional argument SELECTOR has the following valid values:

nil -- Select all files.
a string -- A regular expression selecting files with matching names.
a function -- A function selecting files with matching names.

The variable `native-comp-async-jobs-number' specifies the number
of (commands) to run simultaneously."
  ;; Normalize: we only want to pass t or nil, never e.g. `late'.
  (let ((load (not (not load))))
    (native--compile-async files recursively load selector)))

(provide 'comp-run)

;;; comp-run.el ends here
