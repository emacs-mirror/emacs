;;; ert-x.el --- Staging area for experimental extensions to ERT  -*- lexical-binding: t -*-

;; Copyright (C) 2008, 2010-2024 Free Software Foundation, Inc.

;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;;         Christian Ohler <ohler@gnu.org>

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

;; This file includes some extra helper functions to use while writing
;; automated tests with ERT.  These have been proposed as extensions
;; to ERT but are not mature yet and likely to change.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'ert)
(require 'subr-x) ; string-trim


;;; Test buffers.

(defun ert--text-button (string &rest properties)
  "Return a string containing STRING as a text button with PROPERTIES.

See `make-text-button'."
  (with-temp-buffer
    (insert string)
    (apply #'make-text-button (point-min) (point-max) properties)
    (buffer-string)))

(defun ert--format-test-buffer-name (base-name)
  "Compute a test buffer name based on BASE-NAME.

Helper function for `ert--test-buffers'."
  (format "*Test buffer (%s)%s*"
	  (or (and (ert-running-test)
		   (ert-test-name (ert-running-test)))
	      "<anonymous test>")
	  (if base-name
	      (format ": %s" base-name)
	    "")))

(defvar ert--test-buffers (make-hash-table :weakness t)
  "Table of all test buffers.  Keys are the buffer objects, values are t.

The main use of this table is for `ert-kill-all-test-buffers'.
Not all buffers in this table are necessarily live, but all live
test buffers are in this table.")

(define-button-type 'ert--test-buffer-button
  'action #'ert--test-buffer-button-action
  'help-echo "mouse-2, RET: Pop to test buffer")

(defun ert--test-buffer-button-action (button)
  "Pop to the test buffer that BUTTON is associated with."
  (pop-to-buffer (button-get button 'ert--test-buffer)))

(defun ert--call-with-test-buffer (ert--base-name ert--thunk)
  "Helper function for `ert-with-test-buffer'.

Create a test buffer with a name based on ERT--BASE-NAME and run
ERT--THUNK with that buffer as current."
  (let* ((ert--buffer (generate-new-buffer
                       (ert--format-test-buffer-name ert--base-name)))
         (ert--button (ert--text-button (buffer-name ert--buffer)
                                        :type 'ert--test-buffer-button
                                        'ert--test-buffer ert--buffer)))
    (puthash ert--buffer 't ert--test-buffers)
    ;; We don't use `unwind-protect' here since we want to kill the
    ;; buffer only on success.
    (prog1 (with-current-buffer ert--buffer
             (ert-info (ert--button :prefix "Buffer: ")
               (funcall ert--thunk)))
      (kill-buffer ert--buffer)
      (remhash ert--buffer ert--test-buffers))))

(cl-defmacro ert-with-test-buffer ((&key ((:name name-form)))
                                   &body body)
  "Create a test buffer and run BODY in that buffer.

To be used in ERT tests.  If BODY finishes successfully, the test
buffer is killed; if there is an error, the test buffer is kept
around for further inspection.  Its name is derived from
the name of the test and the result of NAME-FORM."
  (declare (debug ((":name" form) def-body))
           (indent 1))
  `(ert--call-with-test-buffer ,name-form (lambda () ,@body)))

(cl-defmacro ert-with-buffer-selected (buffer-or-name &body body)
  "Display a buffer in a temporary selected window and run BODY.

If BUFFER-OR-NAME is nil, the current buffer is used.

The buffer is made the current buffer, and the temporary window
becomes the `selected-window', before BODY is evaluated.  The
modification hooks `before-change-functions' and
`after-change-functions' are not inhibited during the evaluation
of BODY, which makes it easier to use `execute-kbd-macro' to
simulate user interaction.  The window configuration is restored
before returning, even if BODY exits nonlocally.  The return
value is the last form in BODY."
  (declare (debug (form body)) (indent 1))
  `(save-window-excursion
     (with-current-buffer (or ,buffer-or-name (current-buffer))
       (with-selected-window (display-buffer (current-buffer))
         ,@body))))

(cl-defmacro ert-with-test-buffer-selected ((&key name) &body body)
  "Create a test buffer, switch to it, and run BODY.

This combines `ert-with-test-buffer' and
`ert-with-buffer-selected'.  The return value is the last form in
BODY."
  (declare (debug ((":name" form) body)) (indent 1))
  `(ert-with-test-buffer (:name ,name)
     (ert-with-buffer-selected (current-buffer)
       ,@body)))

;;;###autoload
(defun ert-kill-all-test-buffers ()
  "Kill all test buffers that are still live."
  (interactive)
  (let ((count 0))
    (maphash (lambda (buffer _dummy)
	       (when (or (not (buffer-live-p buffer))
			 (kill-buffer buffer))
		 (cl-incf count)))
	     ert--test-buffers)
    (message "%s out of %s test buffers killed"
	     count (hash-table-count ert--test-buffers)))
  ;; It could be that some test buffers were actually kept alive
  ;; (e.g., due to `kill-buffer-query-functions').  I'm not sure what
  ;; to do about this.  For now, let's just forget them.
  (clrhash ert--test-buffers)
  nil)


;;; Simulate commands.

(defun ert-simulate-command (command)
  ;; FIXME: add unread-events
  "Simulate calling COMMAND the way the Emacs command loop would call it.

This effectively executes

  (apply (car COMMAND) (cdr COMMAND))

and returns the same value, but additionally runs hooks like
`pre-command-hook' and `post-command-hook', and sets variables
like `this-command' and `last-command'.

COMMAND should be a list where the car is the command symbol and
the rest are arguments to the command.

NOTE: Since the command is not called by `call-interactively'
test for `called-interactively' in the command will fail."
  (cl-assert (listp command) t)
  (cl-assert (commandp (car command)) t)
  (cl-assert (not unread-command-events) t)
  (let (return-value)
    ;; For the order of things here see command_loop_1 in keyboard.c.
    ;;
    ;; The command loop will reset the command-related variables so
    ;; there is no reason to let-bind them. They are set here,
    ;; however, to be able to test several commands in a row and how
    ;; they affect each other.
    (setq deactivate-mark nil
          this-original-command (car command)
          ;; remap through active keymaps
          this-command (or (command-remapping this-original-command)
                           this-original-command))
    (run-hooks 'pre-command-hook)
    (setq return-value (apply (car command) (cdr command)))
    (run-hooks 'post-command-hook)
    (setq real-last-command (car command)
          last-command this-command)
    (when (boundp 'last-repeatable-command)
      (setq last-repeatable-command real-last-command))
    (when (and deactivate-mark transient-mark-mode) (deactivate-mark))
    (cl-assert (not unread-command-events) t)
    return-value))

(defmacro ert-simulate-keys (keys &rest body)
  "Execute BODY with KEYS as pseudo-interactive input."
  (declare (debug t) (indent 1))
  `(let ((unread-command-events
          ;; Add some C-g to try and make sure we still exit
          ;; in case something goes wrong.
          (append ,keys '(?\C-g ?\C-g ?\C-g)))
         ;; Tell `read-from-minibuffer' not to read from stdin when in
         ;; batch mode.
         (executing-kbd-macro t))
     ,@body))

(defun ert-run-idle-timers ()
  "Run all idle timers (from `timer-idle-list')."
  (dolist (timer (copy-sequence timer-idle-list))
    (timer-event-handler timer)))


;;; Miscellaneous utilities.

(defun ert-filter-string (s &rest regexps)
  "Return a copy of S with all matches of REGEXPS removed.

Elements of REGEXPS may also be two-element lists \(REGEXP
SUBEXP), where SUBEXP is the number of a subexpression in
REGEXP.  In that case, only that subexpression will be removed
rather than the entire match."
  ;; Use a temporary buffer since replace-match copies strings, which
  ;; would lead to N^2 runtime.
  (with-temp-buffer
    (insert s)
    (dolist (x regexps)
      (cl-destructuring-bind (regexp subexp) (if (listp x) x `(,x nil))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (replace-match "" t t nil subexp))))
    (buffer-string)))


(defun ert-propertized-string (&rest args)
  "Return a string with properties as specified by ARGS.

ARGS is a list of strings and plists.  The strings in ARGS are
concatenated to produce an output string.  In the output string,
each string from ARGS will be have the preceding plist as its
property list, or no properties if there is no plist before it.

As a simple example,

\(ert-propertized-string \"foo \" \\='(face italic) \"bar\" \" baz\" nil \
\" quux\")

would return the string \"foo bar baz quux\" where the substring
\"bar baz\" has a `face' property with the value `italic'.

None of the ARGS are modified, but the return value may share
structure with the plists in ARGS."
  (with-temp-buffer
    (cl-loop with current-plist = nil
             for x in args do
             (cl-etypecase x
               (string (let ((begin (point)))
                         (insert x)
                         (set-text-properties begin (point) current-plist)))
               (list (unless (zerop (mod (length x) 2))
                       (error "Odd number of args in plist: %S" x))
                     (setq current-plist x))))
    (buffer-string)))


(defun ert-call-with-buffer-renamed (buffer-name thunk)
  "Protect the buffer named BUFFER-NAME from side-effects and run THUNK.

Renames the buffer BUFFER-NAME to a new temporary name, creates a
new buffer named BUFFER-NAME, executes THUNK, kills the new
buffer, and renames the original buffer back to BUFFER-NAME.

This is useful if THUNK has undesirable side-effects on an Emacs
buffer with a fixed name such as *Messages*."
  (let ((new-buffer-name (generate-new-buffer-name
                          (format "%s orig buffer" buffer-name))))
    (with-current-buffer (get-buffer-create buffer-name)
      (rename-buffer new-buffer-name))
    (unwind-protect
        (progn
          (get-buffer-create buffer-name)
          (funcall thunk))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name))
      (with-current-buffer new-buffer-name
        (rename-buffer buffer-name)))))

(cl-defmacro ert-with-buffer-renamed ((buffer-name-form) &body body)
  "Protect the buffer named BUFFER-NAME from side-effects and run BODY.

See `ert-call-with-buffer-renamed' for details."
  (declare (indent 1))
  `(ert-call-with-buffer-renamed ,buffer-name-form (lambda () ,@body)))


(defun ert-buffer-string-reindented (&optional buffer)
  "Return the contents of BUFFER after reindentation.

BUFFER defaults to current buffer.  Does not modify BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let ((clone nil))
      (unwind-protect
          (progn
            ;; `clone-buffer' doesn't work if `buffer-file-name' is non-nil.
            (let ((buffer-file-name nil))
              (setq clone (clone-buffer)))
            (with-current-buffer clone
              (let ((inhibit-read-only t))
                (indent-region (point-min) (point-max)))
              (buffer-string)))
        (when clone
          (let ((kill-buffer-query-functions nil))
            (kill-buffer clone)))))))


(defmacro ert-with-message-capture (var &rest body)
  "Execute BODY while collecting messages in VAR.

Capture messages issued by Lisp code and concatenate them
separated by newlines into one string.  This includes messages
written by `message' as well as objects printed by `print',
`prin1' and `princ' to the echo area.  Messages issued from C
code using the above mentioned functions will not be captured.

This is useful for separating the issuance of messages by the
code under test from the behavior of the *Messages* buffer."
  (declare (debug (symbolp body))
           (indent 1))
  (let ((g-message-advice (gensym))
        (g-print-advice (gensym))
        (g-collector (gensym)))
    `(let* ((,var "")
            (,g-collector (lambda (msg) (setq ,var (concat ,var msg))))
            (,g-message-advice (ert--make-message-advice ,g-collector))
            (,g-print-advice (ert--make-print-advice ,g-collector)))
       (advice-add 'message :around ,g-message-advice)
       (advice-add 'prin1 :around ,g-print-advice)
       (advice-add 'princ :around ,g-print-advice)
       (advice-add 'print :around ,g-print-advice)
       (unwind-protect
           (progn ,@body)
         (advice-remove 'print ,g-print-advice)
         (advice-remove 'princ ,g-print-advice)
         (advice-remove 'prin1 ,g-print-advice)
         (advice-remove 'message ,g-message-advice)))))

(defun ert--make-message-advice (collector)
  "Create around advice for `message' for `ert-collect-messages'.
COLLECTOR will be called with the message before it is passed
to the real `message'."
  (lambda (func &rest args)
    (if (or (null args) (member (car args) '("" nil)))
        (apply func args)
      (let ((msg (apply #'format-message args)))
        (funcall collector (concat msg "\n"))
        (funcall func "%s" msg)))))

(defun ert--make-print-advice (collector)
  "Create around advice for print functions for `ert-collect-messages'.
The created advice function will just call the original function
unless the output is going to the echo area (when PRINTCHARFUN is
t or PRINTCHARFUN is nil and `standard-output' is t).  If the
output is destined for the echo area, the advice function will
convert it to a string and pass it to COLLECTOR first."
  ;;; FIXME: Pass on OVERRIDES.
  (lambda (func object &optional printcharfun _overrides)
    (if (not (eq t (or printcharfun standard-output)))
        (funcall func object printcharfun)
      (funcall collector (with-output-to-string
                           (funcall func object)))
      (funcall func object printcharfun))))

(defvar ert-resource-directory-format "%s-resources/"
  "Format for `ert-resource-directory'.")
(defvar ert-resource-directory-trim-left-regexp ""
  "Regexp for `string-trim' (left) used by `ert-resource-directory'.")
(defvar ert-resource-directory-trim-right-regexp "\\(-tests?\\)?\\.el"
  "Regexp for `string-trim' (right) used by `ert-resource-directory'.")

(defmacro ert-resource-directory ()
  "Return absolute file name of the resource (test data) directory.

The path to the resource directory is the \"resources\" directory
in the same directory as the test file this is called from.

If that directory doesn't exist, find a directory based on the
test file name.  If the file is named \"foo-tests.el\", return
the absolute file name for \"foo-resources\".

If you want a different resource directory naming scheme, set the
variable `ert-resource-directory-format'.  Before formatting, the
file name will be trimmed using `string-trim' with arguments
`ert-resource-directory-trim-left-regexp' and
`ert-resource-directory-trim-right-regexp'."
  `(when-let ((testfile ,(or (macroexp-file-name)
                             buffer-file-name)))
     (let ((default-directory (file-name-directory testfile)))
       (file-truename
        (if (file-accessible-directory-p "resources/")
            (expand-file-name "resources/")
          (expand-file-name
           (format ert-resource-directory-format
                   (string-trim testfile
                                ert-resource-directory-trim-left-regexp
                                ert-resource-directory-trim-right-regexp))))))))

(defmacro ert-resource-file (file)
  "Return absolute file name of resource (test data) file named FILE.
A resource file is defined as any file placed in the resource
directory as returned by `ert-resource-directory'."
  `(expand-file-name ,file (ert-resource-directory)))

(defvar ert-temp-file-prefix "emacs-test-"
  "Prefix used by `ert-with-temp-file' and `ert-with-temp-directory'.")

(defvar ert-temp-file-suffix nil
  "Suffix used by `ert-with-temp-file' and `ert-with-temp-directory'.")

(defun ert--with-temp-file-generate-suffix (filename)
  "Generate temp file suffix from FILENAME."
  (thread-last
    (file-name-base filename)
    (replace-regexp-in-string (rx string-start
                                  (group (+? not-newline))
                                  (regexp "-?tests?")
                                  string-end)
                              "\\1")
    (concat "-")))

(defmacro ert-with-temp-file (name &rest body)
  "Bind NAME to the name of a new temporary file and evaluate BODY.
Delete the temporary file after BODY exits normally or
non-locally.  NAME will be bound to the file name of the temporary
file.

The following keyword arguments are supported:

:prefix STRING  If non-nil, pass STRING to `make-temp-file' as
                the PREFIX argument.  Otherwise, use the value of
                `ert-temp-file-prefix'.

:suffix STRING  If non-nil, pass STRING to `make-temp-file' as the
                SUFFIX argument.  Otherwise, use the value of
                `ert-temp-file-suffix'; if the value of that
                variable is nil, generate a suffix based on the
                name of the file that `ert-with-temp-file' is
                called from.

:text STRING    If non-nil, pass STRING to `make-temp-file' as
                the TEXT argument.

:buffer SYMBOL  Open the temporary file using `find-file-noselect'
                and bind SYMBOL to the buffer.  Kill the buffer
                after BODY exits normally or non-locally.

:coding CODING  If non-nil, bind `coding-system-for-write' to CODING
                when executing BODY.  This is handy when STRING includes
                non-ASCII characters or the temporary file must have a
                specific encoding or end-of-line format.

See also `ert-with-temp-directory'."
  (declare (indent 1) (debug (symbolp body)))
  (cl-check-type name symbol)
  (let (keyw prefix suffix directory text extra-keywords buffer coding)
    (while (keywordp (setq keyw (car body)))
      (setq body (cdr body))
      (pcase keyw
        (:prefix (setq prefix (pop body)))
        (:suffix (setq suffix (pop body)))
        ;; This is only for internal use by `ert-with-temp-directory'
        ;; and is therefore not documented.
        (:directory (setq directory (pop body)))
        (:text (setq text (pop body)))
        (:buffer (setq buffer (pop body)))
        (:coding (setq coding (pop body)))
        (_ (push keyw extra-keywords) (pop body))))
    (when extra-keywords
      (error "Invalid keywords: %s" (mapconcat #'symbol-name extra-keywords " ")))
    (let ((temp-file (make-symbol "temp-file"))
          (prefix (or prefix ert-temp-file-prefix))
          (suffix (or suffix ert-temp-file-suffix
                      (ert--with-temp-file-generate-suffix
                       (or (macroexp-file-name) buffer-file-name)))))
      `(let* ((coding-system-for-write ,(or coding coding-system-for-write))
              (,temp-file (,(if directory 'file-name-as-directory 'identity)
                           (make-temp-file ,prefix ,directory ,suffix ,text)))
              (,name ,(if directory
                          `(file-name-as-directory ,temp-file)
                        temp-file))
              ,@(when buffer
                  (list `(,buffer (find-file-literally ,temp-file)))))
         (unwind-protect
             (progn ,@body)
           (ignore-errors
             ,@(when buffer
                 (list `(with-current-buffer ,buffer
                          (set-buffer-modified-p nil))
                       `(kill-buffer ,buffer))))
           (ignore-errors
             ,(if directory
                  `(delete-directory ,temp-file :recursive)
                `(delete-file ,temp-file))))))))

(defmacro ert-with-temp-directory (name &rest body)
  "Bind NAME to the name of a new temporary directory and evaluate BODY.
Delete the temporary directory after BODY exits normally or
non-locally.

NAME is bound to the directory name, not the directory file
name.  (In other words, it will end with the directory delimiter;
on Unix-like systems, it will end with \"/\".)

The same keyword arguments are supported as in
`ert-with-temp-file' (which see), except for :text."
  (declare (indent 1) (debug (symbolp body)))
  (let ((tail body) keyw)
    (while (keywordp (setq keyw (car tail)))
      (setq tail (cddr tail))
      (pcase keyw (:text (error "Invalid keyword for directory: :text")))))
  `(ert-with-temp-file ,name
     :directory t
     ,@body))

(defun ert-gcc-is-clang-p ()
  "Return non-nil if the `gcc' command actually runs the Clang compiler."
  ;; Some macOS machines run llvm when you type gcc.  (!)
  ;; We can't even check if it's a symlink; it's a binary placed in
  ;; "/usr/bin/gcc".  So we need to check the output.
  (string-match "Apple \\(LLVM\\|[Cc]lang\\)\\|Xcode\\.app"
                (shell-command-to-string "gcc --version")))

(defvar tramp-default-host-alist)
(defvar tramp-methods)
(defvar tramp-remote-path)

;; This should happen on hydra only.
(when (and (featurep 'tramp) (getenv "EMACS_HYDRA_CI"))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; If this defconst is used in a test file, `tramp' shall be loaded
;; prior `ert-x'.  There is no default value on w32 systems, which
;; could work out of the box.
(defconst ert-remote-temporary-file-directory
  (when (featurep 'tramp)
    (cond
     ((getenv "REMOTE_TEMPORARY_FILE_DIRECTORY"))
     ((eq system-type 'windows-nt) null-device)
     (t (add-to-list
         'tramp-methods
         '("mock"
	   (tramp-login-program	     "sh")
	   (tramp-login-args	     (("-i")))
           (tramp-direct-async       ("-c"))
	   (tramp-remote-shell	     "/bin/sh")
	   (tramp-remote-shell-args  ("-c"))
	   (tramp-connection-timeout 10)))
        (add-to-list
         'tramp-default-host-alist
         `("\\`mock\\'" nil ,(system-name)))
        ;; Emacs's Makefile sets $HOME to a nonexistent value.  Needed
        ;; in batch mode only, therefore.
        (when (and noninteractive (not (file-directory-p "~/")))
          (setenv "HOME" temporary-file-directory))
        (format "/mock::%s" temporary-file-directory))))
    "Temporary directory for remote file tests.")

(provide 'ert-x)

;;; ert-x.el ends here
