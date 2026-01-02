;;; files-tests.el --- tests for files.el.  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2026 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'nadvice)
(eval-when-compile (require 'cl-lib))
(require 'bytecomp) ; `byte-compiler-base-file-name'.
(require 'dired) ; `dired-uncache'.
(require 'filenotify) ; `file-notify-add-watch'.

;; Set to t if the local variable was set, `query' if the query was
;; triggered.
(defvar files-test-result nil)

(defvar files-test-safe-result nil)
(put 'files-test-safe-result 'safe-local-variable 'booleanp)

(defun files-test-fun1 ()
  (setq files-test-result t))

(ert-deftest files-test-locate-user-emacs-file ()
  (ert-with-temp-directory home
    (with-environment-variables (("HOME" home))
      (let* ((user-emacs-directory (expand-file-name ".emacs.d/" home)))
        (make-directory user-emacs-directory 'parents)
        (should-error (locate-user-emacs-file nil "any-file")
                      :type 'wrong-type-argument)
        ;; No file exists.
        (should (equal (locate-user-emacs-file "always")
                       (expand-file-name "always" user-emacs-directory)))
        (should (equal (locate-user-emacs-file "always" "never")
                       (expand-file-name "always" user-emacs-directory)))
        ;; Only the file in $HOME/.conf exists.
        (let ((exists (expand-file-name "exists" home)))
          (write-region "data" nil exists nil 'quietly)
          (should (equal (locate-user-emacs-file "missing" "exists")
                         exists)))
        ;; Only the file in ~/.emacs.d/ exists.
        (let ((exists (expand-file-name "exists" user-emacs-directory)))
          (write-region "data" nil exists nil 'quietly)
          (should (equal (locate-user-emacs-file "exists" "missing")
                         exists)))
        ;; Both files exist.
        (let* ((basename "testconfig")
               (in-home (expand-file-name basename home))
               (in-edir (expand-file-name basename user-emacs-directory)))
          (write-region "data" nil in-home nil 'quietly)
          (write-region "data" nil in-edir nil 'quietly)
          (should (equal (locate-user-emacs-file basename)
                         in-edir))
          (should (equal (locate-user-emacs-file basename "anything")
                         in-edir)))
        ;; NEW-FILE is a list.
        (should (equal (locate-user-emacs-file '("first" "second"))
                       (expand-file-name "first" user-emacs-directory)))
        (should (equal (locate-user-emacs-file '("first" "second") "never")
                       (expand-file-name "first" user-emacs-directory)))
        (let ((exists (expand-file-name "exists" user-emacs-directory)))
          (write-region "data" nil exists nil 'quietly)
          (should (equal (locate-user-emacs-file '("missing" "exists"))
                         exists))
          (should (equal (locate-user-emacs-file '("missing1" "exists") "missing2")
                         exists)))))))

;; Test combinations:
;; `enable-local-variables' t, nil, :safe, :all, or something else.
;; `enable-local-eval' t, nil, or something else.

(defvar files-test-local-variable-data
  ;; Unsafe eval form
  '((("eval: (files-test-fun1)")
     (t t         (eq files-test-result t))
     (t nil       (eq files-test-result nil))
     (t maybe     (eq files-test-result 'query))
     (nil t       (eq files-test-result nil))
     (nil nil     (eq files-test-result nil))
     (nil maybe   (eq files-test-result nil))
     (:safe t     (eq files-test-result nil))
     (:safe nil   (eq files-test-result nil))
     (:safe maybe (eq files-test-result nil))
     (:all t      (eq files-test-result t))
     (:all nil    (eq files-test-result nil))
     (:all maybe  (eq files-test-result t)) ; This combination is ambiguous.
     (maybe t     (eq files-test-result 'query))
     (maybe nil   (eq files-test-result nil))
     (maybe maybe (eq files-test-result 'query)))
    ;; Unsafe local variable value
    (("files-test-result: t")
     (t t         (eq files-test-result 'query))
     (t nil       (eq files-test-result 'query))
     (t maybe     (eq files-test-result 'query))
     (nil t       (eq files-test-result nil))
     (nil nil     (eq files-test-result nil))
     (nil maybe   (eq files-test-result nil))
     (:safe t     (eq files-test-result nil))
     (:safe nil   (eq files-test-result nil))
     (:safe maybe (eq files-test-result nil))
     (:all t      (eq files-test-result t))
     (:all nil    (eq files-test-result t))
     (:all maybe  (eq files-test-result t))
     (maybe t     (eq files-test-result 'query))
     (maybe nil   (eq files-test-result 'query))
     (maybe maybe (eq files-test-result 'query)))
    ;; Safe local variable
    (("files-test-safe-result: t")
     (t t         (eq files-test-safe-result t))
     (t nil       (eq files-test-safe-result t))
     (t maybe     (eq files-test-safe-result t))
     (nil t       (eq files-test-safe-result nil))
     (nil nil     (eq files-test-safe-result nil))
     (nil maybe   (eq files-test-safe-result nil))
     (:safe t     (eq files-test-safe-result t))
     (:safe nil   (eq files-test-safe-result t))
     (:safe maybe (eq files-test-safe-result t))
     (:all t      (eq files-test-safe-result t))
     (:all nil    (eq files-test-safe-result t))
     (:all maybe  (eq files-test-safe-result t))
     (maybe t     (eq files-test-result 'query))
     (maybe nil   (eq files-test-result 'query))
     (maybe maybe (eq files-test-result 'query)))
    ;; Safe local variable with unsafe value
    (("files-test-safe-result: 1")
     (t t         (eq files-test-result 'query))
     (t nil       (eq files-test-result 'query))
     (t maybe     (eq files-test-result 'query))
     (nil t       (eq files-test-safe-result nil))
     (nil nil     (eq files-test-safe-result nil))
     (nil maybe   (eq files-test-safe-result nil))
     (:safe t     (eq files-test-safe-result nil))
     (:safe nil   (eq files-test-safe-result nil))
     (:safe maybe (eq files-test-safe-result nil))
     (:all t      (eq files-test-safe-result 1))
     (:all nil    (eq files-test-safe-result 1))
     (:all maybe  (eq files-test-safe-result 1))
     (maybe t     (eq files-test-result 'query))
     (maybe nil   (eq files-test-result 'query))
     (maybe maybe (eq files-test-result 'query))))
  "List of file-local variable tests.
Each list element should have the form

  (LOCAL-VARS-LIST . TEST-LIST)

where LOCAL-VARS-LISTS should be a list of local variable
definitions (strings) and TEST-LIST is a list of tests to
perform.  Each entry of TEST-LIST should have the form

 (ENABLE-LOCAL-VARIABLES ENABLE-LOCAL-EVAL FORM)

where ENABLE-LOCAL-VARIABLES is the value to assign to
`enable-local-variables', ENABLE-LOCAL-EVAL is the value to
assign to `enable-local-eval', and FORM is a desired `should'
form.")

(defun file-test--do-local-variables-test (str test-settings)
  (with-temp-buffer
    (insert str)
    (setq files-test-result nil
	  files-test-safe-result nil)
    (let ((enable-local-variables (nth 0 test-settings))
	  (enable-local-eval      (nth 1 test-settings))
	  ;; Prevent any dir-locals file interfering with the tests.
	  (enable-dir-local-variables nil))
      (hack-local-variables)
      (eval (nth 2 test-settings) t))))

(ert-deftest files-tests-local-variables ()
  "Test the file-local variables implementation."
  (cl-letf (((symbol-function 'hack-local-variables-confirm)
             (lambda (&rest _)
               (setq files-test-result 'query)
               nil)))
    (dolist (test files-test-local-variable-data)
      (let ((str (concat "text\n\n;; Local Variables:\n;; "
                         (mapconcat 'identity (car test) "\n;; ")
                         "\n;; End:\n")))
        (dolist (subtest (cdr test))
          (should (file-test--do-local-variables-test str subtest)))))))

(ert-deftest files-tests-permanent-local-variables ()
  (let ((enable-local-variables nil))
    (with-temp-buffer
      (setq lexical-binding nil)
      (insert ";;; test-test.el --- tests  -*- lexical-binding: t; -*-\n\n")
      (hack-local-variables)
      (should (eq lexical-binding t))))
  (let ((enable-local-variables nil)
        (permanently-enabled-local-variables nil))
    (with-temp-buffer
      (setq lexical-binding nil)
      (insert ";;; test-test.el --- tests  -*- lexical-binding: t; -*-\n\n")
      (hack-local-variables)
      (should (eq lexical-binding nil)))))

(ert-deftest files-tests-safe-local-variable-directories ()
  ;; safe-local-variable-directories should be risky,
  ;; so use it as an arbitrary risky variable.
  (let ((test-alist '((safe-local-variable-directories . "some_val")))
        (fakedir default-directory)
        (enable-local-eval t))
    (with-temp-buffer
      (setq safe-local-variable-directories (list fakedir))
      (hack-local-variables-filter test-alist fakedir)
      (should (equal file-local-variables-alist test-alist)))
    (with-temp-buffer
      (setq safe-local-variable-directories (list fakedir))
      (setq noninteractive t)
      (hack-local-variables-filter test-alist "wrong")
      (should-not (equal file-local-variables-alist test-alist)))
    (with-temp-buffer
      (setq safe-local-variable-directories '())
      (setq noninteractive t)
      (hack-local-variables-filter test-alist fakedir)
      (should-not (equal file-local-variables-alist test-alist)))))

(defvar files-test-bug-18141-file
  (ert-resource-file "files-bug18141.el.gz")
  "Test file for bug#18141.")

(ert-deftest files-tests-bug-18141 ()
  "Test for https://debbugs.gnu.org/18141 ."
  (skip-unless (executable-find "gzip"))
  ;; If called interactively, environment variable
  ;; $EMACS_TEST_DIRECTORY does not exist.
  (skip-unless (file-exists-p files-test-bug-18141-file))
  (ert-with-temp-file tempfile
    :prefix "emacs-test-files-bug-18141"
    :suffix ".gz"
    (copy-file files-test-bug-18141-file tempfile t)
    (with-current-buffer (find-file-noselect tempfile)
      (set-buffer-modified-p t)
      (save-buffer)
      (should (eq buffer-file-coding-system 'iso-2022-7bit-unix)))))

(ert-deftest files-tests-make-temp-file-empty-prefix ()
  "Test make-temp-file with an empty prefix."
  (let ((tempfile (make-temp-file ""))
        (tempdir (make-temp-file "" t))
        (tempfile-. (make-temp-file "."))
        (tempdir-. (make-temp-file "." t))
        (tempfile-.. (make-temp-file ".."))
        (tempdir-.. (make-temp-file ".." t)))
    (dolist (file (list tempfile tempfile-. tempfile-..))
      (should file)
      (delete-file file))
    (dolist (dir (list tempdir tempdir-. tempdir-..))
      (should dir)
      (delete-directory dir))))

;; Stop the above "Local Var..." confusing Emacs.


(ert-deftest files-tests-bug-21454 ()
  "Test for https://debbugs.gnu.org/21454 ."
  (let ((input-result
         (if (memq system-type '(windows-nt ms-dos))
             '(("/foo/bar//baz/;/bar/foo/baz//" nil
                ("/foo/bar//baz/" "/bar/foo/baz//"))
               ("x:/foo/bar/;y:/bar/qux/;z:/qux/foo" nil
                ("x:/foo/bar/" "y:/bar/qux/" "z:/qux/foo/"))
               ("x://foo/bar/;y:/bar/qux/;z:/qux/foo/" nil
                ("x://foo/bar/" "y:/bar/qux/" "z:/qux/foo/"))
               ("x:/foo/bar/;y:/bar/qux/;z:/qux/foo/" nil
                ("x:/foo/bar/" "y:/bar/qux/" "z:/qux/foo/"))
               ("x:/foo//bar/;y:/bar/qux/;z:/qux/foo/" nil
                ("x:/foo//bar/" "y:/bar/qux/" "z:/qux/foo/"))
               ("x:/foo//bar/;y:/bar/qux/;z:/qux/foo" nil
                ("x:/foo//bar/" "y:/bar/qux/" "z:/qux/foo/"))
               ("x:/foo/bar" "$FOO/baz/;z:/qux/foo/"
                ("x:/foo/bar/baz/" "z:/qux/foo/"))
               ("///foo/bar/" "$FOO/baz/;/qux/foo/"
                ("//foo/bar//baz/" "/qux/foo/")))
           (if (eq system-type 'cygwin)
               '(("/foo/bar//baz/:/bar/foo/baz//" nil
                  ("/foo/bar//baz/" "/bar/foo/baz//"))
                 ("/foo/bar/:/bar/qux/:/qux/foo" nil
                  ("/foo/bar/" "/bar/qux/" "/qux/foo/"))
                 ("//foo/bar/:/bar/qux/:/qux/foo/" nil
                  ("//foo/bar/" "/bar/qux/" "/qux/foo/"))
                 ("/foo/bar/:/bar/qux/:/qux/foo/" nil
                  ("/foo/bar/" "/bar/qux/" "/qux/foo/"))
                 ("/foo//bar/:/bar/qux/:/qux/foo/" nil
                  ("/foo//bar/" "/bar/qux/" "/qux/foo/"))
                 ("/foo//bar/:/bar/qux/:/qux/foo" nil
                  ("/foo//bar/" "/bar/qux/" "/qux/foo/"))
                 ("/foo/bar" "$FOO/baz/:/qux/foo/"
                  ("/foo/bar/baz/" "/qux/foo/"))
                 ("///foo/bar/" "$FOO/baz/:/qux/foo/"
                  ("//foo/bar//baz/" "/qux/foo/")))
             '(("/foo/bar//baz/:/bar/foo/baz//" nil
                ("/foo/bar//baz/" "/bar/foo/baz//"))
               ("/foo/bar/:/bar/qux/:/qux/foo" nil
                ("/foo/bar/" "/bar/qux/" "/qux/foo/"))
               ("//foo/bar/:/bar/qux/:/qux/foo/" nil
                ("/foo/bar/" "/bar/qux/" "/qux/foo/"))
               ("/foo/bar/:/bar/qux/:/qux/foo/" nil
                ("/foo/bar/" "/bar/qux/" "/qux/foo/"))
               ("/foo//bar/:/bar/qux/:/qux/foo/" nil
                ("/foo//bar/" "/bar/qux/" "/qux/foo/"))
               ("/foo//bar/:/bar/qux/:/qux/foo" nil
                ("/foo//bar/" "/bar/qux/" "/qux/foo/"))
               ("/foo/bar" "$FOO/baz/:/qux/foo/"
                ("/foo/bar/baz/" "/qux/foo/"))
               ("//foo/bar/" "$FOO/baz/:/qux/foo/"
                ("/foo/bar//baz/" "/qux/foo/"))))))
        (foo-env (getenv "FOO"))
        (bar-env (getenv "BAR")))
    (unwind-protect
        (dolist (test input-result)
          (let ((foo (nth 0 test))
                (bar (nth 1 test))
                (res (nth 2 test)))
            (setenv "FOO" foo)
            (if bar
                (progn
                  (setenv "BAR" bar)
                  (should (equal res (parse-colon-path (getenv "BAR")))))
              (should (equal res (parse-colon-path "$FOO"))))))
      (setenv "FOO" foo-env)
      (setenv "BAR" bar-env))))

(ert-deftest files-tests-save-buffers-kill-emacs--confirm-kill-processes ()
  "Test that `save-buffers-kill-emacs' honors
`confirm-kill-processes'."
  (cl-letf* ((yes-or-no-p-prompts nil)
             ((symbol-function #'yes-or-no-p)
              (lambda (prompt)
                (push prompt yes-or-no-p-prompts)
                nil))
             (kill-emacs-args nil)
             ((symbol-function #'kill-emacs)
              (lambda (&rest args) (push args kill-emacs-args)))
             (process
              (make-process
               :name "sleep"
               :command (list
                         (expand-file-name invocation-name invocation-directory)
                         "-batch" "-Q" "-eval" "(sleep-for 1000)")))
             (confirm-kill-processes nil))
    (save-buffers-kill-emacs)
    (kill-process process)
    (should-not yes-or-no-p-prompts)
    (should (equal kill-emacs-args '((nil nil))))))

(ert-deftest files-tests-read-file-in-~ ()
  "Test file prompting in directory named `~'.
If we are in a directory named `~', the default value should not
be $HOME."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _coll &optional _pred _req init _hist def _)
               (or def init))))
    (ert-with-temp-directory dir
      (let ((subdir (expand-file-name "./~/" dir)))
        (make-directory subdir t)
        (with-temp-buffer
          (setq default-directory subdir)
          (should-not (equal
                       (expand-file-name (read-file-name "File: "))
                       (expand-file-name "~/")))
          ;; Don't overquote either!
          (setq default-directory (concat "/:" subdir))
          (should-not (equal
                       (expand-file-name (read-file-name "File: "))
                       (concat "/:/:" subdir))))))))

(ert-deftest files-tests-file-name-non-special-quote-unquote ()
  (let (;; Just in case it is quoted, who knows.
        (temporary-file-directory (file-name-unquote temporary-file-directory)))
    (should-not (file-name-quoted-p temporary-file-directory))
    (should (file-name-quoted-p (file-name-quote temporary-file-directory)))
    (should (equal temporary-file-directory
                   (file-name-unquote
                    (file-name-quote temporary-file-directory))))
    ;; It does not hurt to quote/unquote a file several times.
    (should (equal (file-name-quote temporary-file-directory)
                   (file-name-quote
                    (file-name-quote temporary-file-directory))))
    (should (equal (file-name-unquote temporary-file-directory)
                   (file-name-unquote
                    (file-name-unquote temporary-file-directory))))))

(ert-deftest files-tests-file-name-non-special--subprocess ()
  "Check that Bug#25949 and Bug#48177 are fixed."
  (skip-unless (and (executable-find "true") (file-exists-p null-device)
                    ;; These systems cannot set date of the null device.
                    (not (memq system-type '(windows-nt ms-dos)))))
  (let ((default-directory (file-name-quote temporary-file-directory))
        (true (file-name-quote (executable-find "true")))
        (null (file-name-quote null-device)))
    (should (zerop (process-file true null `((:file ,null) ,null))))
    (should (processp (start-file-process "foo" nil true)))
    (should (zerop (shell-command true)))
    (should (processp (make-process :name "foo" :command `(,true))))))

(defmacro files-tests--with-advice (symbol where function &rest body)
  (declare (indent 3))
  (cl-check-type symbol symbol)
  (cl-check-type where keyword)
  (cl-check-type function function)
  (macroexp-let2 nil function function
    `(progn
       (advice-add #',symbol ,where ,function)
       (unwind-protect
           (progn ,@body)
         (advice-remove #',symbol ,function)))))

(ert-deftest files-tests-file-name-non-special--buffers ()
  "Check that Bug#25951 is fixed.
We call `verify-visited-file-modtime' on a buffer visiting a file
with a quoted name.  We use two different variants: first with
the buffer current and a nil argument, second passing the buffer
object explicitly.  In both cases no error should be raised and
the `file-name-non-special' handler for quoted file names should
be invoked with the right arguments."
  (ert-with-temp-file temp-file-name
    (with-temp-buffer
     (let* ((buffer-visiting-file (current-buffer))
            (actual-args ())
            (log (lambda (&rest args) (push args actual-args))))
       (insert-file-contents (file-name-quote temp-file-name) :visit)
       (should (stringp buffer-file-name))
       (should (file-name-quoted-p buffer-file-name))
       ;; The following is not true for remote files.
       (should (string-prefix-p "/:" buffer-file-name))
       (should (consp (visited-file-modtime)))
       (should (equal (find-file-name-handler buffer-file-name
                                              #'verify-visited-file-modtime)
                      #'file-name-non-special))
       (files-tests--with-advice file-name-non-special :before log
         ;; This should call the file name handler with the right
         ;; buffer and not signal an error.  The file hasn't been
         ;; modified, so `verify-visited-file-modtime' should return
         ;; t.
         (should (equal (verify-visited-file-modtime) t))
         (with-temp-buffer
           (should (stringp (buffer-file-name buffer-visiting-file)))
           ;; This should call the file name handler with the right
           ;; buffer and not signal an error.  The file hasn't been
           ;; modified, so `verify-visited-file-modtime' should return
           ;; t.
           (should (equal (verify-visited-file-modtime buffer-visiting-file)
                          t))))
       ;; Verify that the handler was actually called.  We called
       ;; `verify-visited-file-modtime' twice, so both calls should be
       ;; recorded in reverse order.
       (should (equal actual-args
                      `((verify-visited-file-modtime ,buffer-visiting-file)
                        (verify-visited-file-modtime nil))))))))

(cl-defmacro files-tests--with-temp-non-special
    ((name non-special-name &optional dir-flag) &rest body)
  "Run tests with quoted file name.
NAME is the symbol which contains the name of a created temporary
file.  NON-SPECIAL-NAME is another symbol, which contains the
temporary file name with quoted file name syntax.  If DIR-FLAG is
non-nil, a temporary directory is created instead.
After evaluating BODY, the temporary file or directory is deleted."
  (declare (indent 1) (debug ((symbolp symbolp &optional form) body)))
  (cl-check-type name symbol)
  (cl-check-type non-special-name symbol)
  `(let* ((temporary-file-directory (file-truename temporary-file-directory))
          (temporary-file-directory
           (file-name-as-directory (make-temp-file "files-tests" t)))
          (,name (make-temp-file "files-tests" ,dir-flag))
          (,non-special-name (file-name-quote ,name)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,name)
         (if ,dir-flag (delete-directory ,name t)
           (delete-file ,name)))
       (when (file-exists-p ,non-special-name)
         (if ,dir-flag (delete-directory ,non-special-name t)
           (delete-file ,non-special-name)))
       (when (file-exists-p temporary-file-directory)
         (delete-directory temporary-file-directory t)))))

(defconst files-tests--special-file-name-extension ".special"
  "Trailing string for test file name handler.")

(defconst files-tests--special-file-name-regexp
  (concat (regexp-quote files-tests--special-file-name-extension) "\\'")
  "Regular expression for test file name handler.")

(defun files-tests--special-file-name-handler (operation &rest args)
  "File name handler for files with extension \".special\"."
  (let ((arg args)
        ;; Avoid cyclic call.
        (file-name-handler-alist
         (delete
          (rassoc
           'files-tests--special-file-name-handler file-name-handler-alist)
          file-name-handler-alist)))
    ;; Remove trailing "\\.special\\'" from arguments, if they are not quoted.
    (while arg
      (when (and (stringp (car arg))
                 (not (file-name-quoted-p (car arg)))
                 (string-match files-tests--special-file-name-regexp (car arg)))
        (setcar arg (replace-match "" nil nil (car arg))))
      (setq arg (cdr arg)))
    ;; Call it.
    (apply operation args)))

(cl-defmacro files-tests--with-temp-non-special-and-file-name-handler
    ((name non-special-name &optional dir-flag) &rest body)
  "Run tests with quoted file name, see `files-tests--with-temp-non-special'.
Both file names in NAME and NON-SPECIAL-NAME have the extension
\".special\".  The created temporary file or directory does not have
that extension.
A file name handler is added which is activated for files with
that extension.  It simply removes the extension from file names.
It is expected, that this file name handler works only for
unquoted file names."
  (declare (indent 1) (debug ((symbolp symbolp &optional form) body)))
  (cl-check-type name symbol)
  (cl-check-type non-special-name symbol)
  `(let* ((temporary-file-directory (file-truename temporary-file-directory))
          (temporary-file-directory
           (file-name-as-directory (make-temp-file "files-tests" t)))
          (file-name-handler-alist
           `((,files-tests--special-file-name-regexp
              . files-tests--special-file-name-handler)
             . ,file-name-handler-alist))
          (,name (concat
                  (make-temp-file "files-tests" ,dir-flag)
                  files-tests--special-file-name-extension))
          (,non-special-name (file-name-quote ,name)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,name)
         (if ,dir-flag (delete-directory ,name t)
           (delete-file ,name)))
       (when (file-exists-p ,non-special-name)
         (if ,dir-flag (delete-directory ,non-special-name t)
           (delete-file ,non-special-name)))
       (when (file-exists-p temporary-file-directory)
         (delete-directory temporary-file-directory t)))))

(defun files-tests--new-name (name part)
  (let (file-name-handler-alist)
    (concat (file-name-sans-extension name) part (file-name-extension name t))))

(ert-deftest files-tests-file-name-non-special-abbreviate-file-name ()
  (let* ((homedir temporary-file-directory)
         (process-environment (cons (format "HOME=%s" homedir)
                                    process-environment))
         (abbreviated-home-dir nil))
    ;; Check that abbreviation doesn't occur for quoted file names.
    (should (equal (concat "/:" homedir "foo/bar")
                   (abbreviate-file-name (concat "/:" homedir "foo/bar"))))))

(ert-deftest files-tests-file-name-non-special-access-file ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    ;; Both versions of the file name work.
    (should-not (access-file tmpfile "test"))
    (should-not (access-file nospecial "test")))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-not (access-file tmpfile "test"))
    ;; The quoted file name does not work.
    (should-error (access-file nospecial "test"))))

(ert-deftest files-tests-file-name-non-special-add-name-to-file ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (let ((newname (files-tests--new-name nospecial "add-name")))
      ;; Both versions work.
      (add-name-to-file tmpfile newname)
      (should (file-exists-p newname))
      (delete-file newname)
      (add-name-to-file nospecial newname)
      (should (file-exists-p newname))
      (delete-file newname)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (let ((newname (files-tests--new-name tmpfile "add-name")))
      ;; Using an unquoted file name works.
      (add-name-to-file tmpfile newname)
      (should (file-exists-p newname))
      (delete-file newname))
    (let ((newname (files-tests--new-name nospecial "add-name")))
      (add-name-to-file tmpfile newname)
      (should (file-exists-p newname))
      (delete-file newname)
      ;; The quoted special file name does not work.
      (should-error (add-name-to-file nospecial newname)))))

(ert-deftest files-tests-file-name-non-special-byte-compiler-base-file-name ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (equal (byte-compiler-base-file-name nospecial)
                   (byte-compiler-base-file-name tmpfile))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should (equal (byte-compiler-base-file-name nospecial) tmpfile))
    (should-not (equal (byte-compiler-base-file-name tmpfile) tmpfile))))

(ert-deftest files-tests-file-name-non-special-copy-directory ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (let ((newname (files-tests--new-name
                    (directory-file-name nospecial-dir) "copy-dir")))
      (copy-directory nospecial-dir newname)
      (should (file-directory-p newname))
      (delete-directory newname)
      (should-not (file-directory-p newname))))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (let ((newname (files-tests--new-name
                    (directory-file-name nospecial-dir) "copy-dir")))
      (should-error (copy-directory nospecial-dir newname))
      (delete-directory newname))))

(ert-deftest files-tests-file-name-non-special-copy-file ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (let ((newname
           (files-tests--new-name (directory-file-name nospecial) "copy-file")))
      (copy-file nospecial newname)
      (should (file-exists-p newname))
      (delete-file newname)
      (should-not (file-exists-p newname))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (let ((newname
           (files-tests--new-name (directory-file-name nospecial) "copy-file")))
      (should-error (copy-file nospecial newname)))))

(ert-deftest files-tests-file-name-non-special-delete-directory ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (delete-directory nospecial-dir))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (should-error (delete-directory nospecial-dir))))

(ert-deftest files-tests-file-name-non-special-delete-file ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (delete-file nospecial))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (delete-file nospecial)
    (should (file-exists-p tmpfile))))

(ert-deftest files-tests-file-name-non-special-diff-latest-backup-file ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (write-region "foo" nil (make-backup-file-name tmpfile))
    (should (equal (diff-latest-backup-file nospecial)
                   (diff-latest-backup-file tmpfile)))
    (delete-file (diff-latest-backup-file nospecial)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (write-region "foo" nil (make-backup-file-name tmpfile))
    (should-not (equal (diff-latest-backup-file nospecial)
                       (diff-latest-backup-file tmpfile)))
    (delete-file (diff-latest-backup-file nospecial))))

(ert-deftest files-tests-file-name-non-special-directory-file-name ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (should (equal (directory-file-name nospecial-dir)
                   (file-name-quote (directory-file-name tmpdir)))))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (should-not (equal (directory-file-name nospecial-dir)
                       (file-name-quote (directory-file-name tmpdir))))))

(ert-deftest files-tests-file-name-non-special-directory-files ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (should (equal (directory-files nospecial-dir)
                   (directory-files tmpdir))))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (should-error (directory-files nospecial-dir))))

(defun files-tests-file-attributes-equal (attr1 attr2)
  ;; Element 4 is access time, which may be changed by the act of
  ;; checking the attributes.
  (setf (nth 4 attr1) nil)
  (setf (nth 4 attr2) nil)
  ;; Element 9 is unspecified.
  (setf (nth 9 attr1) nil)
  (setf (nth 9 attr2) nil)
  (equal attr1 attr2))

(ert-deftest files-tests-file-name-non-special-directory-files-and-attributes ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (cl-loop for (file1 . attr1) in (directory-files-and-attributes nospecial-dir)
             for (file2 . attr2) in (directory-files-and-attributes tmpdir)
             do
             (should (equal file1 file2))
             (should (files-tests-file-attributes-equal attr1 attr2))))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (should-error (directory-files-and-attributes nospecial-dir))))

(ert-deftest files-tests-file-name-non-special-dired-compress-handler ()
  ;; `dired-compress-file' can get confused by filenames with ":" in
  ;; them, which causes this to fail on `windows-nt' systems.
  (when (string-search ":" (expand-file-name temporary-file-directory))
    (ert-skip "FIXME: `dired-compress-file' unreliable when filenames contain `:'."))
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (let ((compressed (dired-compress-file nospecial)))
      (when compressed
        ;; FIXME: Should it return a still-quoted name?
        (should (file-equal-p nospecial (dired-compress-file compressed))))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-not (dired-compress-file nospecial))))

(ert-deftest files-tests-file-name-non-special-dired-uncache ()
  ;; FIXME: This is not a real test.  We need cached values, and check
  ;; whether they disappear.
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (dired-uncache nospecial-dir))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (dired-uncache nospecial-dir)))

(ert-deftest files-tests-file-name-non-special-expand-file-name ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (equal (expand-file-name nospecial) nospecial)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should (equal (expand-file-name nospecial) nospecial))))

(ert-deftest files-tests-file-name-non-special-expand-file-name-tilde ()
  (let ((process-environment
         (cons (format "HOME=%s" (file-truename temporary-file-directory))
               process-environment))
        abbreviated-home-dir)
    (files-tests--with-temp-non-special (tmpfile nospecial)
      (let (file-name-handler-alist)
        (setq nospecial (file-name-quote (abbreviate-file-name tmpfile))))
      (should (equal (expand-file-name nospecial)
                     (expand-file-name (file-name-unquote nospecial t)))))
    (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
      (let (file-name-handler-alist)
        (setq nospecial (file-name-quote (abbreviate-file-name tmpfile))))
      (should-not
       (equal (expand-file-name nospecial)
              ;; The file name handler deletes the ".special" extension.
              (expand-file-name (file-name-unquote nospecial t)))))))

(ert-deftest files-tests-file-name-non-special-file-accessible-directory-p ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (should (file-accessible-directory-p nospecial-dir)))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (should-not (file-accessible-directory-p nospecial-dir))))

(ert-deftest files-tests-file-name-non-special-file-acl ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (equal (file-acl nospecial) (file-acl tmpfile))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-not (file-acl nospecial))))

(ert-deftest files-tests-file-name-non-special-file-attributes ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (files-tests-file-attributes-equal
             (file-attributes nospecial) (file-attributes tmpfile))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-not (file-attributes nospecial))))

(ert-deftest files-tests-file-name-non-special-file-directory-p ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (should (file-directory-p nospecial-dir)))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (should-not (file-directory-p nospecial-dir))))

(ert-deftest files-tests-file-name-non-special-file-equal-p ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (file-equal-p nospecial tmpfile))
    (should (file-equal-p tmpfile nospecial))
    (should (file-equal-p nospecial nospecial)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should (file-equal-p (file-name-unquote nospecial) tmpfile))
    (should (file-equal-p tmpfile (file-name-unquote nospecial)))
    ;; File `nospecial' does not exist, so it cannot be compared.
    (should-not (file-equal-p nospecial nospecial))
    (write-region "foo" nil nospecial)
    (should (file-equal-p nospecial nospecial))))

(ert-deftest files-tests-file-name-non-special-file-executable-p ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should-not (file-executable-p nospecial)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-not (file-executable-p nospecial))))

(ert-deftest files-tests-file-name-non-special-file-exists-p ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (file-exists-p tmpfile))
    (should (file-exists-p nospecial)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should (file-exists-p tmpfile))
    (should-not (file-exists-p nospecial))))

(ert-deftest files-tests-file-name-non-special-file-in-directory-p ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (let ((nospecial-tempdir (file-name-quote temporary-file-directory)))
      (should (file-in-directory-p nospecial temporary-file-directory))
      (should (file-in-directory-p tmpfile nospecial-tempdir))
      (should (file-in-directory-p nospecial nospecial-tempdir))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (let ((nospecial-tempdir (file-name-quote temporary-file-directory)))
      (should (file-in-directory-p nospecial temporary-file-directory))
      (should (file-in-directory-p tmpfile nospecial-tempdir))
      (should (file-in-directory-p nospecial nospecial-tempdir)))))

(ert-deftest files-tests-file-name-non-special-file-local-copy ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should-not (file-local-copy nospecial))) ; Already local.
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-not (file-local-copy nospecial)))) ; Already local.

(ert-deftest files-tests-file-name-non-special-file-modes ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (equal (file-modes nospecial) (file-modes tmpfile))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-not (equal (file-modes nospecial) (file-modes tmpfile)))))

(ert-deftest files-tests-file-name-non-special-file-name-all-completions ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (let ((nospecial-tempdir (file-name-quote temporary-file-directory))
          (tmpdir temporary-file-directory)
          (file (file-name-nondirectory tmpfile))
          (nospecial-file (file-name-nondirectory nospecial)))
      (should (string-equal file nospecial-file))
      (should (equal (file-name-all-completions
                      nospecial-file nospecial-tempdir)
                     (file-name-all-completions file tmpdir)))
      (should (equal (file-name-all-completions file nospecial-tempdir)
                     (file-name-all-completions file tmpdir)))
      (should (equal (file-name-all-completions nospecial-file tmpdir)
                     (file-name-all-completions file tmpdir)))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (let ((nospecial-tempdir (file-name-quote temporary-file-directory))
          (tmpdir temporary-file-directory)
          (file (file-name-nondirectory tmpfile))
          (nospecial-file (file-name-nondirectory nospecial)))
      (should-not (string-equal file nospecial-file))
      (should (equal (file-name-all-completions nospecial-file nospecial-tempdir)
                     (file-name-all-completions file tmpdir)))
      (should (equal (file-name-all-completions file nospecial-tempdir)
                     (file-name-all-completions file tmpdir)))
      (should (equal (file-name-all-completions nospecial-file tmpdir)
                     (file-name-all-completions file tmpdir))))))

(ert-deftest files-tests-file-name-non-special-file-name-as-directory ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (should (equal (file-name-as-directory nospecial-dir)
                   (file-name-quote (file-name-as-directory tmpdir)))))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (should-not (equal (file-name-as-directory nospecial-dir)
                   (file-name-quote (file-name-as-directory tmpdir))))))

(ert-deftest files-tests-file-name-non-special-file-name-case-insensitive-p ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (equal (file-name-case-insensitive-p nospecial)
                   (file-name-case-insensitive-p tmpfile))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should (equal (file-name-case-insensitive-p nospecial)
                   (file-name-case-insensitive-p tmpfile)))))

(ert-deftest files-tests-file-name-non-special-file-name-completion ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (let ((nospecial-tempdir (file-name-quote temporary-file-directory))
          (tmpdir temporary-file-directory)
          (file (file-name-nondirectory tmpfile))
          (nospecial-file (file-name-nondirectory nospecial)))
      (should (string-equal file nospecial-file))
      (should (equal (file-name-completion nospecial-file nospecial-tempdir)
                     (file-name-completion file tmpdir)))
      (should (equal (file-name-completion file nospecial-tempdir)
                     (file-name-completion file tmpdir)))
      (should (equal (file-name-completion nospecial-file tmpdir)
                     (file-name-completion file tmpdir)))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (let ((nospecial-tempdir (file-name-quote temporary-file-directory))
          (tmpdir temporary-file-directory)
          (file (file-name-nondirectory tmpfile))
          (nospecial-file (file-name-nondirectory nospecial)))
      (should-not (string-equal file nospecial-file))
      (should (equal (file-name-completion nospecial-file nospecial-tempdir)
                     (file-name-completion file tmpdir)))
      (should (equal (file-name-completion file nospecial-tempdir)
                     (file-name-completion file tmpdir)))
      (should (equal (file-name-completion nospecial-file tmpdir)
                     (file-name-completion file tmpdir))))))

(ert-deftest files-tests-file-name-non-special-file-name-directory ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (equal (file-name-directory nospecial)
                   (file-name-quote temporary-file-directory))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should (equal (file-name-directory nospecial)
                   (file-name-quote temporary-file-directory)))))

(ert-deftest files-tests-file-name-non-special-file-name-nondirectory ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (equal (file-name-nondirectory nospecial)
                   (file-name-nondirectory tmpfile))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-not (equal (file-name-nondirectory nospecial)
                       (file-name-nondirectory tmpfile)))))

(ert-deftest files-tests-file-name-non-special-file-name-sans-versions ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (equal (file-name-sans-versions nospecial) nospecial)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should (equal (file-name-sans-versions nospecial) nospecial))))

(ert-deftest files-tests-file-name-non-special-file-newer-than-file-p ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should-not (file-newer-than-file-p nospecial tmpfile))
    (should-not (file-newer-than-file-p tmpfile nospecial))
    (should-not (file-newer-than-file-p nospecial nospecial)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-not (file-newer-than-file-p nospecial tmpfile))
    (should (file-newer-than-file-p tmpfile nospecial))
    (should-not (file-newer-than-file-p nospecial nospecial))))

(ert-deftest files-tests-file-name-non-special-notify-handlers ()
  (skip-unless file-notify--library)
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (let ((watch (file-notify-add-watch nospecial '(change) #'ignore)))
      (should (file-notify-valid-p watch))
      (file-notify-rm-watch watch)
      (should-not (file-notify-valid-p watch))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (let ((watch (file-notify-add-watch nospecial '(change) #'ignore)))
      (should (file-notify-valid-p watch))
      (file-notify-rm-watch watch)
      (should-not (file-notify-valid-p watch)))))

(ert-deftest files-tests-file-name-non-special-file-ownership-preserved-p ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (equal (file-ownership-preserved-p nospecial)
                   (file-ownership-preserved-p tmpfile))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should (equal (file-ownership-preserved-p nospecial)
                   (file-ownership-preserved-p tmpfile)))))

(ert-deftest files-tests-file-name-non-special-file-readable-p ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (file-readable-p nospecial)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-not (file-readable-p nospecial))))

(ert-deftest files-tests-file-name-non-special-file-regular-p ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (file-regular-p nospecial)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-not (file-regular-p nospecial))))

(ert-deftest files-tests-file-name-non-special-file-remote-p ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should-not (file-remote-p nospecial)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-not (file-remote-p nospecial))))

(ert-deftest files-tests-file-name-non-special-file-selinux-context ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (unless (equal (file-selinux-context tmpfile) '(nil nil nil nil))
      (should (equal (file-selinux-context nospecial)
                     (file-selinux-context tmpfile)))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (unless (equal (file-selinux-context tmpfile) '(nil nil nil nil))
      (should-not (equal (file-selinux-context nospecial)
                         (file-selinux-context tmpfile))))))

(ert-deftest files-tests-file-name-non-special-file-symlink-p ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should-not (file-symlink-p nospecial)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-not (file-symlink-p nospecial))))

(ert-deftest files-tests-file-name-non-special-file-truename ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (equal nospecial (file-truename nospecial))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should (equal nospecial (file-truename nospecial)))))

(ert-deftest files-tests-file-name-non-special-file-writable-p ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (file-writable-p nospecial)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should (file-writable-p nospecial))))

(ert-deftest files-tests-file-name-non-special-find-backup-file-name ()
  (let (version-control delete-old-versions
	(kept-old-versions (default-toplevel-value 'kept-old-versions))
	(kept-new-versions (default-toplevel-value 'kept-new-versions)))
    (files-tests--with-temp-non-special (tmpfile nospecial)
      (should (equal (find-backup-file-name nospecial)
                     (mapcar #'file-name-quote
                             (find-backup-file-name tmpfile)))))
    (files-tests--with-temp-non-special-and-file-name-handler
        (tmpfile nospecial)
      (should-not (equal (find-backup-file-name nospecial)
                         (mapcar #'file-name-quote
                                 (find-backup-file-name tmpfile)))))))

(ert-deftest files-tests-file-name-non-special-get-file-buffer ()
  ;; Make sure these buffers don't exist.
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (let ((fbuf (get-file-buffer nospecial)))
      (if fbuf (kill-buffer fbuf))
      (should-not (get-file-buffer nospecial))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (let ((fbuf (get-file-buffer nospecial)))
      (if fbuf (kill-buffer fbuf))
      (should-not (get-file-buffer nospecial)))))

(ert-deftest files-tests-file-name-non-special-insert-directory ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (should (equal (with-temp-buffer
                     (insert-directory nospecial-dir "")
                     (buffer-string))
                   (with-temp-buffer
                     (insert-directory tmpdir "")
                     (buffer-string)))))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (should-error (with-temp-buffer (insert-directory nospecial-dir "")))))

(ert-deftest files-tests-file-name-non-special-insert-file-contents ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (with-temp-buffer
      (insert-file-contents nospecial)
      (should (zerop (buffer-size)))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-error (with-temp-buffer (insert-file-contents nospecial)))))

(ert-deftest files-tests-file-name-non-special-load ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (load nospecial nil t)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-error (load nospecial nil t))))

(ert-deftest files-tests-file-name-non-special-make-auto-save-file-name ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (save-current-buffer
      (should (equal (prog2 (set-buffer (find-file-noselect nospecial))
                         (make-auto-save-file-name)
                       (kill-buffer))
                     (prog2 (set-buffer (find-file-noselect tmpfile))
                         (make-auto-save-file-name)
                       (kill-buffer))))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (save-current-buffer
      (should-not (equal (prog2 (set-buffer (find-file-noselect nospecial))
                             (make-auto-save-file-name)
                           (kill-buffer))
                         (prog2 (set-buffer (find-file-noselect tmpfile))
                             (make-auto-save-file-name)
                           (kill-buffer)))))))

(ert-deftest files-test-auto-save-name-default ()
  (with-temp-buffer
    (let ((auto-save-file-name-transforms nil)
          (name-start (if (memq system-type '(windows-nt ms-dos)) 2 nil)))
      (setq buffer-file-name "/tmp/foo.txt")
      (should (equal (substring (make-auto-save-file-name) name-start)
                     "/tmp/#foo.txt#")))))

(ert-deftest files-test-auto-save-name-transform ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/foo.txt")
    (let ((auto-save-file-name-transforms
           '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" nil)))
          (name-start (if (memq system-type '(windows-nt ms-dos)) 2 nil)))
      (should (equal (substring (make-auto-save-file-name) name-start)
                     "/var/tmp/#foo.txt#")))))

(ert-deftest files-test-auto-save-name-unique ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/foo.txt")
    (let ((auto-save-file-name-transforms
           '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))
          (name-start (if (memq system-type '(windows-nt ms-dos)) 2 nil)))
      (should (equal (substring (make-auto-save-file-name) name-start)
                     "/var/tmp/#!tmp!foo.txt#")))
    (let ((auto-save-file-name-transforms
           '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" sha1)))
          (name-start (if (memq system-type '(windows-nt ms-dos)) 2 nil)))
      (should (equal (substring (make-auto-save-file-name) name-start)
                     "/var/tmp/#b57c5a04f429a83305859d3350ecdab8315a9037#")))))

(ert-deftest files-test-lock-name-default ()
  (let ((lock-file-name-transforms nil)
        (name-start (if (memq system-type '(windows-nt ms-dos)) 2 nil)))
    (should (equal (substring (make-lock-file-name "/tmp/foo.txt") name-start)
                   "/tmp/.#foo.txt"))))

(ert-deftest files-test-lock-name-unique ()
  (let ((lock-file-name-transforms
         '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))
        (name-start (if (memq system-type '(windows-nt ms-dos)) 2 nil)))
    (should (equal (substring (make-lock-file-name "/tmp/foo.txt") name-start)
                   "/var/tmp/.#!tmp!foo.txt")))
  (let ((lock-file-name-transforms
         '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" sha1)))
        (name-start (if (memq system-type '(windows-nt ms-dos)) 2 nil)))
    (should (equal (substring (make-lock-file-name "/tmp/foo.txt") name-start)
                   "/var/tmp/.#b57c5a04f429a83305859d3350ecdab8315a9037"))))

(ert-deftest files-tests-file-name-non-special-make-directory ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (let ((default-directory nospecial-dir))
      (make-directory "dir")
      (should (file-directory-p "dir"))
      (delete-directory "dir")))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (let ((default-directory nospecial-dir))
      (should-error (make-directory "dir")))))

(ert-deftest files-tests-file-name-non-special-make-nearby-temp-file ()
  (let* ((default-directory (file-name-quote temporary-file-directory))
         (near-tmpfile (make-nearby-temp-file "file")))
    (should (file-exists-p near-tmpfile))
    (delete-file near-tmpfile)))

(ert-deftest files-tests-file-name-non-special-make-symbolic-link ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (files-tests--with-temp-non-special (tmpfile nospecial)
      (let* ((linkname (expand-file-name "link" tmpdir))
             (may-symlink (ignore-errors (make-symbolic-link tmpfile linkname)
                                         t)))
        (when may-symlink
          (should (file-symlink-p linkname))
          (delete-file linkname)
          (let ((linkname (expand-file-name "link" nospecial-dir)))
            (make-symbolic-link tmpfile linkname)
            (should (file-symlink-p linkname))
            (delete-file linkname))))))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (files-tests--with-temp-non-special-and-file-name-handler
        (tmpfile nospecial)
      (let* ((linkname (expand-file-name "link" tmpdir))
             (may-symlink (ignore-errors (make-symbolic-link tmpfile linkname)
                                         t)))
        (when may-symlink
          (should (file-symlink-p linkname))
          (delete-file linkname)
          (let ((linkname (expand-file-name "link" nospecial-dir)))
            (should-error (make-symbolic-link tmpfile linkname))))))))

;; See `files-tests-file-name-non-special--subprocess'.
;; (ert-deftest files-tests-file-name-non-special-process-file ())

(ert-deftest files-tests-file-name-non-special-rename-file ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (rename-file nospecial (files-tests--new-name nospecial "x"))
    (rename-file (files-tests--new-name nospecial "x") nospecial)
    (rename-file tmpfile (files-tests--new-name nospecial "x"))
    (rename-file (files-tests--new-name nospecial "x") nospecial)
    (rename-file nospecial (files-tests--new-name tmpfile "x"))
    (rename-file (files-tests--new-name nospecial "x") nospecial))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-error (rename-file nospecial (files-tests--new-name nospecial "x")))
    (rename-file tmpfile (files-tests--new-name nospecial "x"))
    (rename-file (files-tests--new-name nospecial "x") nospecial)
    (rename-file nospecial (files-tests--new-name tmpfile "x"))
    (should-error (rename-file (files-tests--new-name nospecial "x") nospecial))
    (delete-file (files-tests--new-name tmpfile "x"))
    (delete-file (files-tests--new-name nospecial "x"))))

(ert-deftest files-tests-file-name-non-special-set-file-acl ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (set-file-acl nospecial (file-acl nospecial)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (set-file-acl nospecial (file-acl nospecial))))

(ert-deftest files-tests-file-name-non-special-set-file-modes ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (set-file-modes nospecial (file-modes nospecial)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-error (set-file-modes nospecial (file-modes nospecial)))))

(ert-deftest files-tests-file-name-non-special-set-file-selinux-context ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (unless (equal (file-selinux-context tmpfile) '(nil nil nil nil))
      (set-file-selinux-context nospecial (file-selinux-context nospecial))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (unless (equal (file-selinux-context tmpfile) '(nil nil nil nil))
      (should-error
       (set-file-selinux-context nospecial (file-selinux-context nospecial))))))

(ert-deftest files-tests-file-name-non-special-set-file-times ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (set-file-times nospecial nil 'nofollow))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-error (set-file-times nospecial nil 'nofollow))))

(ert-deftest files-tests-file-name-non-special-set-visited-file-modtime ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (save-current-buffer
      (set-buffer (find-file-noselect nospecial))
      (set-visited-file-modtime)
      (kill-buffer)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (save-current-buffer
      (set-buffer (find-file-noselect nospecial))
      (set-visited-file-modtime)
      (kill-buffer))))

(ert-deftest files-tests-file-name-non-special-shell-command ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (with-temp-buffer
      (let ((default-directory nospecial-dir))
        (shell-command (concat (shell-quote-argument
                                (concat invocation-directory invocation-name))
                               " --version")
                       (current-buffer))
        (goto-char (point-min))
        (should (search-forward emacs-version nil t)))))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (with-temp-buffer
      (let ((default-directory nospecial-dir))
        (should-error
         (shell-command (concat (shell-quote-argument
                                 (concat invocation-directory invocation-name))
                                " --version")
                        (current-buffer)))))))

(ert-deftest files-tests-file-name-non-special-start-file-process ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (with-temp-buffer
      (let ((default-directory nospecial-dir))
        (let ((proc (start-file-process
                     "emacs" (current-buffer)
                     (concat invocation-directory invocation-name)
                     "--version")))
          (unwind-protect
              (progn
                (accept-process-output proc)
                (goto-char (point-min))
                (should (search-forward emacs-version nil t))
                ;; Don't stop the test run with a query, as the subprocess
                ;; may or may not be dead by the time we reach here.
                (set-process-query-on-exit-flag proc nil)
                ;; On MS-Windows, wait for the process to die, since the OS
                ;; will not let us delete a directory that is the cwd of a
                ;; running process.
                (when (eq system-type 'windows-nt)
                  (while (process-live-p proc)
                    (sleep-for 0.1))))
            (delete-process proc))))))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (with-temp-buffer
      (let ((default-directory nospecial-dir))
        (should-error (start-file-process
                       "emacs" (current-buffer)
                       (concat invocation-directory invocation-name)
                       "--version"))))))

(ert-deftest files-tests-file-name-non-special-substitute-in-file-name ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (let ((process-environment (cons "FOO=foo" process-environment))
          (nospecial-foo (files-tests--new-name nospecial "$FOO")))
      ;; The "/:" prevents substitution.
      (should (equal (substitute-in-file-name nospecial-foo) nospecial-foo))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (let ((process-environment (cons "FOO=foo" process-environment))
          (nospecial-foo (files-tests--new-name nospecial "$FOO")))
      ;; The "/:" prevents substitution.
      (should (equal (substitute-in-file-name nospecial-foo) nospecial-foo)))))

(ert-deftest files-tests-file-name-non-special-temporary-file-directory ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (let ((default-directory nospecial-dir))
      (should (equal (temporary-file-directory) temporary-file-directory))))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (let ((default-directory nospecial-dir))
      (should (equal (temporary-file-directory) temporary-file-directory)))))

(ert-deftest files-tests-file-name-non-special-unhandled-file-name-directory ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (should (equal (unhandled-file-name-directory nospecial-dir)
                   (file-name-as-directory tmpdir))))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (should-not (equal (unhandled-file-name-directory nospecial-dir)
                       (file-name-as-directory tmpdir)))))

(ert-deftest files-tests-file-name-non-special-vc-registered ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (equal (vc-registered nospecial) (vc-registered tmpfile))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should (equal (vc-registered nospecial) (vc-registered tmpfile)))))

;; See test `files-tests-file-name-non-special--buffers'.
;; (ert-deftest files-tests-file-name-non-special-verify-visited-file-modtime ())

(ert-deftest files-tests-file-name-non-special-write-region ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (with-temp-buffer
      (write-region nil nil nospecial nil :visit)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (with-temp-buffer
      (write-region nil nil nospecial nil :visit))))

(ert-deftest files-tests-file-name-non-special-make-process ()
  "Check that the ‘:file-handler’ argument of ‘make-process’
works as expected if the default directory is quoted."
  (let ((default-directory (file-name-quote invocation-directory))
        (program (file-name-quote
                  (expand-file-name invocation-name invocation-directory))))
    (should (processp (make-process :name "name"
                                    :command (list program "--version")
                                    :file-handler t)))))

(ert-deftest files-tests-insert-directory-wildcard-in-dir-p ()
  (let ((alist (list (cons "/home/user/*/.txt" (cons "/home/user/" "*/.txt"))
                     (cons "/home/user/.txt" nil)
                     (cons "/home/*/.txt" (cons "/home/" "*/.txt"))
                     (cons "/home/*/" (cons "/home/" "*/"))
                     (cons "/*/.txt" (cons "/" "*/.txt"))
                     ;;
                     (cons "c:/tmp/*/*.txt" (cons "c:/tmp/" "*/*.txt"))
                     (cons "c:/tmp/*.txt" nil)
                     (cons "c:/tmp/*/" (cons "c:/tmp/" "*/"))
                     (cons "c:/*/*.txt" (cons "c:/" "*/*.txt")))))
    (dolist (path-res alist)
      (should
       (equal
        (cdr path-res)
        (insert-directory-wildcard-in-dir-p (car path-res)))))))

(ert-deftest files-tests-make-directory ()
  (ert-with-temp-directory dir
    (let* ((dirname (file-name-as-directory dir))
           (file (concat dirname "file"))
           (subdir1 (concat dirname "subdir1"))
           (subdir2 (concat dirname "subdir2"))
           (a/b (concat dirname "a/b")))
      (write-region "" nil file)
      (should-error (make-directory "/"))
      (should (make-directory "/" t))
      (should-error (make-directory dir))
      (should (make-directory dir t))
      (should-error (make-directory dirname))
      (should (make-directory dirname t))
      (should-error (make-directory file))
      (should-error (make-directory file t))
      (should-not (make-directory subdir1))
      (should-not (make-directory subdir2 t))
      (should-error (make-directory a/b))
      (should-not (make-directory a/b t))
      (delete-directory dir 'recursive))))

(ert-deftest files-tests-file-modes-symbolic-to-number ()
  (let ((alist (list (cons "a=rwx" #o777)
                     (cons "o=t" #o1000)
                     (cons "o=xt" #o1001)
                     (cons "o=tx" #o1001) ; Order doesn't matter.
                     (cons "u=rwx,g=rx,o=rx" #o755)
                     (cons "u=rwx,g=,o=" #o700)
                     (cons "u=rwx" #o700) ; Empty permissions can be ignored.
                     (cons "u=rw,g=r,o=r" #o644)
                     (cons "u=rw,g=r,o=t" #o1640)
                     (cons "u=rw,g=r,o=xt" #o1641)
                     (cons "u=rwxs,g=rs,o=xt" #o7741)
                     (cons "u=rws,g=rs,o=t" #o7640)
                     (cons "u=rws,g=rs,o=r" #o6644)
                     (cons "a=r" #o444)
                     (cons "u=S" nil)
                     (cons "u=T" nil)
                     (cons "u=Z" nil))))
    (dolist (x alist)
      (if (cdr-safe x)
          (should (equal (cdr x) (file-modes-symbolic-to-number (car x))))
        (should-error (file-modes-symbolic-to-number (car x)))))))

(ert-deftest files-tests-file-modes-number-to-symbolic ()
  (let ((alist (list (cons #o755 "-rwxr-xr-x")
                     (cons #o700 "-rwx------")
                     (cons #o644 "-rw-r--r--")
                     (cons #o1640 "-rw-r----T")
                     (cons #o1641 "-rw-r----t")
                     (cons #o7741 "-rwsr-S--t")
                     (cons #o7640 "-rwSr-S--T")
                     (cons #o6644 "-rwSr-Sr--")
                     (cons #o444 "-r--r--r--"))))
    (dolist (x alist)
      (should (equal (cdr x) (file-modes-number-to-symbolic (car x)))))))

(ert-deftest files-tests-no-file-write-contents ()
  "Test that `write-contents-functions' permits saving a file.
Usually `basic-save-buffer' will prompt for a file name if the
current buffer has none.  It should first call the functions in
`write-contents-functions', and if one of them returns non-nil,
consider the buffer saved, without prompting for a file
name (Bug#28412)."
  (let ((read-file-name-function
         (lambda (&rest _ignore)
           (error "Prompting for file name")))
        require-final-newline)
    ;; With contents function, and no file.
    (with-temp-buffer
      (setq write-contents-functions (lambda () t))
      (set-buffer-modified-p t)
      (should (null (save-buffer))))
    ;; With no contents function and no file.  This should reach the
    ;; `read-file-name' prompt.
    (with-temp-buffer
      (set-buffer-modified-p t)
      (should-error (save-buffer) :type 'error))
    ;; Then a buffer visiting a file: should save normally.
    (ert-with-temp-file temp-file-name
      (with-current-buffer (find-file-noselect temp-file-name)
        (setq write-contents-functions nil)
        (insert "p")
        (should (null (save-buffer)))
        (should (eq (buffer-size) 1))))))

(ert-deftest files-tests-copy-directory ()
  (ert-with-temp-directory dir
    (let* ((dirname (file-name-as-directory dir))
           (source (concat dirname "source"))
           (dest (concat dirname "dest/new/directory/"))
           (file (concat (file-name-as-directory source) "file"))
           (source2 (concat dirname "source2"))
           (dest2 (concat dirname "dest/new2"))
           (source3 (concat dirname "source3/d"))
           (dest3 (concat dirname "dest3/d")))
      (make-directory source)
      (write-region "" nil file)
      (copy-directory source dest t t t)
      (should (file-exists-p (concat dest "file")))
      (make-directory (concat (file-name-as-directory source2) "a") t)
      (copy-directory source2 dest2)
      (should (file-directory-p (concat (file-name-as-directory dest2) "a")))
      (make-directory source3 t)
      (write-region "x\n" nil (concat (file-name-as-directory source3) "file"))
      (make-directory dest3 t)
      (write-region "y\n" nil (concat (file-name-as-directory dest3) "file"))
      (copy-directory source3 (file-name-directory dest3) t)
      (delete-directory dir 'recursive))))

(ert-deftest files-tests-abbreviate-file-name-homedir ()
  ;; Check homedir abbreviation.
  (let* ((homedir temporary-file-directory)
         (process-environment (cons (format "HOME=%s" homedir)
                                    process-environment))
         (abbreviated-home-dir nil))
    (should (equal "~/foo/bar"
                   (abbreviate-file-name (concat homedir "foo/bar")))))
  ;; Check that homedir abbreviation doesn't occur when homedir is just /.
  (let* ((homedir "/")
         (process-environment (cons (format "HOME=%s" homedir)
                                    process-environment))
         (abbreviated-home-dir nil))
    (should (equal "/foo/bar"
                   (abbreviate-file-name (concat homedir "foo/bar"))))))

(ert-deftest files-tests-abbreviate-file-name-directory-abbrev-alist ()
    ;; Check `directory-abbrev-alist' abbreviation.
    (let ((directory-abbrev-alist '(("\\`/nowhere/special" . "/nw/sp"))))
      (should (equal "/nw/sp/here"
                     (abbreviate-file-name "/nowhere/special/here"))))
    ;; Check homedir and `directory-abbrev-alist' abbreviation.
    (let* ((homedir temporary-file-directory)
           (process-environment (cons (format "HOME=%s" homedir)
                                      process-environment))
           (abbreviated-home-dir nil)
           (directory-abbrev-alist
            `((,(concat "\\`" (regexp-quote homedir) "nowhere/special")
              . ,(concat homedir "nw/sp")))))
      (should (equal "~/nw/sp/here"
                     (abbreviate-file-name
                      (concat homedir "nowhere/special/here"))))))

(ert-deftest files-tests-abbreviated-home-dir ()
  "Test that changing HOME does not confuse `abbreviate-file-name'.
See <https://debbugs.gnu.org/19657#20>."
  (let* ((homedir temporary-file-directory)
         (process-environment (cons (format "HOME=%s" homedir)
                                    process-environment))
         (abbreviated-home-dir nil)
         (testfile (expand-file-name "foo" homedir))
         (old (file-truename (abbreviate-file-name testfile)))
         (process-environment (cons (format "HOME=%s"
                                            (expand-file-name "bar" homedir))
                                    process-environment)))
    (should (equal old (file-truename (abbreviate-file-name testfile))))))

(ert-deftest files-tests-executable-find ()
  "Test that `executable-find' works also with a relative or remote PATH.
See <https://debbugs.gnu.org/35241>."
  (ert-with-temp-file tmpfile
    :suffix (car exec-suffixes)
    (set-file-modes tmpfile #o755)
    (let ((exec-path `(,temporary-file-directory)))
      (should
       (equal tmpfile
              (executable-find (file-name-nondirectory tmpfile)))))
    ;; An empty element of `exec-path' means `default-directory'.
    (let ((default-directory temporary-file-directory)
          (exec-path nil))
      (should
       (equal tmpfile
              (executable-find (file-name-nondirectory tmpfile)))))
    ;; The remote file name shall be quoted, and handled like a
    ;; non-existing directory.
    (let ((default-directory "/ssh::")
          (exec-path (append exec-path `("." ,temporary-file-directory))))
      (should
       (equal tmpfile
              (executable-find (file-name-nondirectory tmpfile)))))))

;; Note: we call this test "...-zzdont..." so that it runs near the
;; end, because otherwise the advice it adds to write-region doesn't
;; get removed(??) and breaks the revert-file tests on MS-Windows.
(ert-deftest files-tests-zzdont-rewrite-precious-files ()
  "Test that `file-precious-flag' forces files to be saved by
renaming only, rather than modified in-place."
  (ert-with-temp-file temp-file-name
    (let* ((advice (lambda (_start _end filename &rest _r)
                     (should-not (string= filename temp-file-name)))))
      (unwind-protect
          (with-current-buffer (find-file-noselect temp-file-name)
            (advice-add #'write-region :before advice)
            (setq-local file-precious-flag t)
            (insert "foobar")
            (should (null (save-buffer))))
        (ignore-errors (advice-remove #'write-region advice))))))

(ert-deftest files-test-file-size-human-readable ()
  (should (equal (file-size-human-readable 13) "13"))
  (should (equal (file-size-human-readable 13 'si) "13"))
  (should (equal (file-size-human-readable 13 'iec) "13B"))
  (should (equal (file-size-human-readable 10000) "9.8k"))
  (should (equal (file-size-human-readable 10000 'si) "10k"))
  (should (equal (file-size-human-readable 10000 'iec) "9.8KiB"))
  (should (equal (file-size-human-readable 4294967296 nil) "4G"))
  (should (equal (file-size-human-readable 4294967296 'si) "4.3G"))
  (should (equal (file-size-human-readable 4294967296 'iec) "4GiB"))
  (should (equal (file-size-human-readable 13 nil " ") "13"))
  (should (equal (file-size-human-readable 13 'si " ") "13"))
  (should (equal (file-size-human-readable 13 'iec " ") "13 B"))
  (should (equal (file-size-human-readable 10000 nil " ") "9.8 k"))
  (should (equal (file-size-human-readable 10000 'si " ") "10 k"))
  (should (equal (file-size-human-readable 10000 'iec " ") "9.8 KiB"))
  (should (equal (file-size-human-readable 4294967296 nil " ") "4 G"))
  (should (equal (file-size-human-readable 4294967296 'si " ") "4.3 G"))
  (should (equal (file-size-human-readable 4294967296 'iec " ") "4 GiB"))
  (should (equal (file-size-human-readable 10000 nil " " "bit") "9.8 kbit"))
  (should (equal (file-size-human-readable 10000 'si " " "bit") "10 kbit"))
  (should (equal (file-size-human-readable 10000 'iec " " "bit") "9.8 Kibit"))

  (should (equal (file-size-human-readable 2048) "2k"))
  (should (equal (file-size-human-readable 2046) "2k"))
  (should (equal (file-size-human-readable 2050) "2k"))
  (should (equal (file-size-human-readable 1950) "1.9k"))
  (should (equal (file-size-human-readable 2100) "2.1k"))

  (should (equal (file-size-human-readable-iec 0) "0 B"))
  (should (equal (file-size-human-readable-iec 1) "1 B"))
  (should (equal (file-size-human-readable-iec 9621) "9.4 KiB"))
  (should (equal (file-size-human-readable-iec 72528034765) "68 GiB")))

(ert-deftest files-test-magic-mode-alist-re-baseline ()
  "Test magic-mode-alist with RE, expected behavior for match."
  (let ((magic-mode-alist '(("my-tag" . text-mode))))
    (with-temp-buffer
      (insert "my-tag")
      (normal-mode)
      (should (eq major-mode 'text-mode)))))

(ert-deftest files-test-magic-mode-alist-re-no-match ()
  "Test magic-mode-alist with RE, expected behavior for no match."
  (let ((magic-mode-alist '(("my-tag" . text-mode))))
    (with-temp-buffer
      (insert "not-my-tag")
      (normal-mode)
      (should (not (eq major-mode 'text-mode))))))

(ert-deftest files-test-magic-mode-alist-re-case-diff ()
  "Test that regexps in magic-mode-alist are case-sensitive.
See <https://debbugs.gnu.org/36401>."
  (let ((case-fold-search t)
        (magic-mode-alist '(("my-tag" . text-mode))))
    (with-temp-buffer
      (goto-char (point-min))
      (insert "My-Tag")
      (normal-mode)
      (should (not (eq major-mode 'text-mode))))))

(ert-deftest files-colon-path ()
  (if (memq system-type '(windows-nt ms-dos))
      (should (equal (parse-colon-path "x:/foo//bar/baz")
                     '("x:/foo//bar/baz/")))
    (should (equal (parse-colon-path "/foo//bar/baz")
                   '("/foo//bar/baz/"))))
  (should (equal (parse-colon-path (concat "." path-separator "/tmp"))
                 '("./" "/tmp/")))
  (should (equal (parse-colon-path (concat "/foo" path-separator "///bar"))
                 (if (memq system-type '(windows-nt cygwin ms-dos))
                     '("/foo/" "//bar/")
                   '("/foo/" "/bar/")))))

(ert-deftest files-test-magic-mode-alist-doctype ()
  "Test that DOCTYPE and variants put files in mhtml-mode."
  (with-temp-buffer
    (goto-char (point-min))
    (insert "<!DOCTYPE html>")
    (normal-mode)
    (should (eq major-mode 'mhtml-mode))
    (erase-buffer)
    (insert "<!doctype html>")
    (normal-mode)
    (should (eq major-mode 'mhtml-mode))))

(defvar files-tests-lao "The Way that can be told of is not the eternal Way;
The name that can be named is not the eternal name.
The Nameless is the origin of Heaven and Earth;
The Named is the mother of all things.
Therefore let there always be non-being,
  so we may see their subtlety,
And let there always be being,
  so we may see their outcome.
The two are the same,
But after they are produced,
  they have different names.
")

(defvar files-tests-tzu "The Nameless is the origin of Heaven and Earth;
The named is the mother of all things.

Therefore let there always be non-being,
  so we may see their subtlety,
And let there always be being,
  so we may see their outcome.
The two are the same,
But after they are produced,
  they have different names.
They both may be called deep and profound.
Deeper and more profound,
The door of all subtleties!
")

(ert-deftest files-tests-revert-buffer ()
  "Test that revert-buffer is successful."
  (ert-with-temp-file temp-file-name
    (with-temp-buffer
      (insert files-tests-lao)
      (write-file temp-file-name)
      (erase-buffer)
      (insert files-tests-tzu)
      (revert-buffer t t t)
      (should (compare-strings files-tests-lao nil nil
                               (buffer-substring (point-min) (point-max))
                               nil nil)))))

(ert-deftest files-tests-revert-buffer-with-fine-grain ()
  "Test that revert-buffer-with-fine-grain is successful."
  (ert-with-temp-file temp-file-name
    (with-temp-buffer
      (insert files-tests-lao)
      (write-file temp-file-name)
      (erase-buffer)
      (insert files-tests-tzu)
      (should (revert-buffer-with-fine-grain t t))
      (should (compare-strings files-tests-lao nil nil
                               (buffer-substring (point-min) (point-max))
                               nil nil)))))

(ert-deftest files-tests-file-name-with-extension-good ()
  "Test that `file-name-with-extension' succeeds with reasonable input."
  (should (string= (file-name-with-extension "Jack" "css") "Jack.css"))
  (should (string= (file-name-with-extension "Jack" ".css") "Jack.css"))
  (should (string= (file-name-with-extension "Jack.scss" "css") "Jack.css"))
  (should (string= (file-name-with-extension "/path/to/Jack.md" "org") "/path/to/Jack.org")))

(ert-deftest files-tests-file-name-with-extension-bad ()
  "Test that `file-name-with-extension' fails on malformed input."
  (should-error (file-name-with-extension nil nil))
  (should-error (file-name-with-extension "Jack" nil))
  (should-error (file-name-with-extension nil "css"))
  (should-error (file-name-with-extension "" ""))
  (should-error (file-name-with-extension "" "css"))
  (should-error (file-name-with-extension "Jack" ""))
  (should-error (file-name-with-extension "Jack" "."))
  (should-error (file-name-with-extension "/is/a/directory/" "css")))

(ert-deftest files-tests-file-name-base ()
  (should (equal (file-name-base "") ""))
  (should (equal (file-name-base "/foo/") ""))
  (should (equal (file-name-base "/foo") "foo"))
  (should (equal (file-name-base "/foo/bar") "bar"))
  (should (equal (file-name-base "foo") "foo"))
  (should (equal (file-name-base "foo/bar") "bar")))

(defun files-tests--check-mode (filename)
  "Return the major mode found in `auto-mode-alist' for FILENAME."
  (set-auto-mode--find-matching-alist-entry
   auto-mode-alist
   (concat "/home/jrhacker/" filename)
   nil))

(ert-deftest files-tests-auto-mode-alist ()
  (should (eq (files-tests--check-mode ".gdbinit.in") #'gdb-script-mode))
  (should (eq (files-tests--check-mode ".gdbinit") #'gdb-script-mode))
  (should (eq (files-tests--check-mode "_gdbinit") #'gdb-script-mode)) ; for MS-DOS
  (should (eq (files-tests--check-mode "gdb.ini") #'gdb-script-mode)) ; likewise
  (should (eq (files-tests--check-mode "gdbinit") #'gdb-script-mode))
  (should (eq (files-tests--check-mode "gdbinit.in") #'gdb-script-mode))
  (should (eq (files-tests--check-mode "SOMETHING-gdbinit") #'gdb-script-mode))
  (should (eq (files-tests--check-mode ".gdbinit.loader") #'gdb-script-mode))
  (should-not (eq (files-tests--check-mode "gdbinit-history.exp") #'gdb-script-mode))
  (should-not (eq (files-tests--check-mode "gdbinit.c") #'gdb-script-mode))
  (should-not (eq (files-tests--check-mode "gdbinit.5") #'gdb-script-mode))
  (should-not (eq (files-tests--check-mode ".gdbinit.py.in") #'gdb-script-mode)))

(ert-deftest files-tests--bug75961 ()
  (let* ((auto-mode-alist (cons '("\\.text\\'" text-mode t) auto-mode-alist))
         (called-fun nil)
         (fun (lambda () (setq called-fun t))))
    (with-temp-buffer
     (setq buffer-file-name "foo.text")
     (normal-mode)
     (should (derived-mode-p 'text-mode))
     (add-hook 'text-mode-hook fun)
     (setq buffer-file-name "foo.html.text")
     (should (not called-fun))
     (normal-mode)
     (remove-hook 'text-mode-hook fun)
     (should called-fun)
     (should (derived-mode-p 'html-mode)))))

(defvar sh-shell)

(defun files-tests--check-shebang (shebang expected-mode &optional expected-dialect)
  "Assert that mode for SHEBANG derives from EXPECTED-MODE.

If EXPECTED-MODE is sh-base-mode, DIALECT says what `sh-shell' should be
set to."
  (ert-with-temp-file script-file
    :text shebang
    (find-file script-file)
    (let ((actual-mode (if (derived-mode-p expected-mode)
                           expected-mode
                         major-mode)))
      ;; Tuck all the information we need in the `should' form: input
      ;; shebang, expected mode vs actual.
      (should
       (equal (list shebang actual-mode)
              (list shebang expected-mode)))
      (when (eq expected-mode 'sh-base-mode)
        (should (eq sh-shell expected-dialect))))))

(ert-deftest files-tests-auto-mode-interpreter ()
  "Test that `set-auto-mode' deduces correct modes from shebangs."
  ;; Straightforward interpreter invocation.
  (files-tests--check-shebang "#!/bin/bash" 'sh-base-mode 'bash)
  (files-tests--check-shebang "#!/usr/bin/make -f" 'makefile-mode)
  ;; Invocation through env.
  (files-tests--check-shebang "#!/usr/bin/env bash" 'sh-base-mode 'bash)
  (files-tests--check-shebang "#!/usr/bin/env python" 'python-base-mode)
  (files-tests--check-shebang "#!/usr/bin/env python3" 'python-base-mode)
  ;; Invocation through env, with supplementary arguments.
  (files-tests--check-shebang "#!/usr/bin/env --split-string=bash -eux" 'sh-base-mode 'bash)
  (files-tests--check-shebang "#!/usr/bin/env --split-string=-iv --default-signal bash -eux" 'sh-base-mode 'bash)
  (files-tests--check-shebang "#!/usr/bin/env -S awk -v FS=\"\\t\" -v OFS=\"\\t\" -f" 'awk-mode)
  (files-tests--check-shebang "#!/usr/bin/env -S make -f" 'makefile-mode)
  (files-tests--check-shebang "#!/usr/bin/env -S-vi bash -eux" 'sh-base-mode 'bash)
  (files-tests--check-shebang "#!/usr/bin/env -ivS --default-signal=INT bash -eux" 'sh-base-mode 'bash)
  (files-tests--check-shebang "#!/usr/bin/env -ivS --default-signal bash -eux" 'sh-base-mode 'bash)
  (files-tests--check-shebang "#!/usr/bin/env -vS -uFOOBAR bash -eux" 'sh-base-mode 'bash)
  ;; Invocation through env, with modified environment.
  (files-tests--check-shebang "#!/usr/bin/env -S PYTHONPATH=/...:${PYTHONPATH} python" 'python-base-mode))

(ert-deftest files-test-dir-locals-2-solo ()
  "Ensure that solo `.dir-locals-2.el' is ignored."
  (with-current-buffer
      (find-file-noselect (ert-resource-file
                           (concat "dir-locals-2-solo/dir-locals-2-solo.txt")))
    (should-not (local-variable-p 'dir-locals-2-loaded))))

(ert-deftest files-test-dir-locals-2-paired ()
  "Ensure that `.dir-locals-2.el' is loaded, if paired."
  (let ((enable-local-variables :all))
    (with-current-buffer (find-file-noselect
                          (ert-resource-file (concat "dir-locals-and-2/dir-locals-and-2.txt")))
      (should (local-variable-p 'dir-locals-loaded))
      (should (local-variable-p 'dir-locals-2-loaded)))))

(ert-deftest files-test-dir-locals-auto-mode-alist ()
  "Test an `auto-mode-alist' entry in `.dir-locals.el'"
  (find-file (ert-resource-file "whatever.quux"))
  (should (eq major-mode 'tcl-mode))
  (find-file (ert-resource-file "auto-test.zot1"))
  (should (eq major-mode 'fundamental-mode))
  (find-file (ert-resource-file "auto-test.zot2"))
  (should (eq major-mode 'fundamental-mode))
  (find-file (ert-resource-file "auto-test.zot3"))
  (should (eq major-mode 'fundamental-mode)))

(defun files-tests--save-some-buffers (pred def-pred-bind exp-1 exp-2)
  "Helper function to test `save-some-buffers'.

This function creates two file-visiting buffers, BUF-1, BUF-2 in
different directories at the same level, i.e., none of them is a
subdir of the other; then it modifies both buffers; finally, it
calls `save-some-buffers' from BUF-1 with first arg t, second
arg PRED and `save-some-buffers-default-predicate' let-bound to
DEF-PRED-BIND.

EXP-1 and EXP-2 are the expected values of calling `buffer-modified-p'
on BUF-1 and BUF-2 after the `save-some-buffers' call.

The test is repeated with `save-some-buffers-default-predicate'
let-bound to PRED and passing nil as second arg of
`save-some-buffers'."
  (ert-with-temp-directory dir
    (let* ((file-1 (expand-file-name "subdir-1/file.foo" dir))
           (file-2 (expand-file-name "subdir-2/file.bar" dir))
           (inhibit-message t)
           buf-1 buf-2)
      (unwind-protect
          (progn
            (make-empty-file file-1 'parens)
            (make-empty-file file-2 'parens)
            (setq buf-1 (find-file file-1)
                  buf-2 (find-file file-2))
            (dolist (buf (list buf-1 buf-2))
              (with-current-buffer buf (insert "foobar\n")))
            ;; Run the test.
            (with-current-buffer buf-1
              (let ((save-some-buffers-default-predicate def-pred-bind))
                (save-some-buffers t pred))
              (should (eq exp-1 (buffer-modified-p buf-1)))
              (should (eq exp-2 (buffer-modified-p buf-2))))
            ;; Set both buffers as modified to run another test.
            (dolist (buf (list buf-1 buf-2))
              (with-current-buffer buf (set-buffer-modified-p t)))
            ;; The result of this test must be identical as the previous one.
            (with-current-buffer buf-1
              (let ((save-some-buffers-default-predicate (or pred def-pred-bind)))
                (save-some-buffers t nil))
              (should (eq exp-1 (buffer-modified-p buf-1)))
              (should (eq exp-2 (buffer-modified-p buf-2)))))
        ;; Clean up.
        (dolist (buf (list buf-1 buf-2))
          (with-current-buffer buf
            (set-buffer-modified-p nil)
            (kill-buffer buf)))))))

(defmacro files-tests--with-yes-or-no-p (reply &rest body)
  "Execute BODY, providing replies to `yes-or-no-p' queries.
REPLY should be a cons (PROMPT . VALUE), and during execution of
BODY this macro provides VALUE as return value to all
`yes-or-no-p' calls prompting for PROMPT and nil to all other
`yes-or-no-p' calls.  After execution of BODY, this macro ensures
that exactly one `yes-or-no-p' call prompting for PROMPT has been
executed during execution of BODY."
  (declare (indent 1) (debug (sexp body)))
  `(cl-letf*
       ((reply ,reply)
        (prompts nil)
        ((symbol-function 'yes-or-no-p)
         (lambda (prompt)
           (let ((reply (cdr (assoc prompt (list reply)))))
             (push (cons prompt reply) prompts)
             reply))))
     ,@body
     (should (equal prompts (list reply)))))

(ert-deftest files-tests-save-buffer-read-only-file ()
  "Test writing to write-protected files with `save-buffer'.
Ensure that the issues from bug#66546 are fixed."
  (ert-with-temp-directory dir
    (cl-flet (;; Define convenience functions.
              (file-contents (file)
                (if (file-exists-p file)
                    (condition-case err
                        (with-temp-buffer
                          (insert-file-contents-literally file)
                          (buffer-string))
                      (error err))
                  'missing))
              (signal-write-failed (&rest _)
                (signal 'file-error "Write failed")))

      (let* (;; Sanitize environment.
             ;; The tests below test text for equality, so we need to
             ;; disable any code- and EOL-conversions to avoid false
             ;; positives and false negatives.
             (coding-system-for-read 'no-conversion)
             (coding-system-for-write 'no-conversion)
             (auto-save-default nil)
             (backup-enable-predicate nil)
             (before-save-hook nil)
             (write-contents-functions nil)
             (write-file-functions nil)
             (after-save-hook nil)

             ;; Set the name of the game.
             (base "read-only-test")
             (file (expand-file-name base dir))
             (backup (make-backup-file-name file))

             (override-read-only-prompt
              (format "File %s is write-protected; try to save anyway? "
                      base)))

        ;; Ensure that set-file-modes renders our test file read-only,
        ;; otherwise skip this test.  Use `file-writable-p' to test
        ;; for read-only-ness, because that's what function
        ;; `save-buffer' uses as well.
        (with-temp-file file (insert "foo\n"))
        (skip-unless (file-writable-p file))
        (set-file-modes file (logand (file-modes file)
                                     (lognot #o0222)))
        (skip-unless (not (file-writable-p file)))

        (with-current-buffer (find-file-noselect file)
          ;; Prepare for tests backing up the file.
          (setq buffer-read-only nil)
          (goto-char (point-min))
          (insert "bar\n")

          ;; Save to read-only file with backup, declining prompt.
          (files-tests--with-yes-or-no-p
              (cons override-read-only-prompt nil)
            (should-error
             (save-buffer)
             ;; "Attempt to save to a file that you aren't allowed to write"
             :type 'error))
          (should-not buffer-backed-up)
          (should     (buffer-modified-p))
          (should-not (file-writable-p file))
          (should     (equal (file-contents file) "foo\n"))
          (should     (equal (file-contents backup) 'missing))

          ;; Save to read-only file with backup, accepting prompt,
          ;; experiencing a write error.
          (files-tests--with-yes-or-no-p
              (cons override-read-only-prompt t)
            (should-error
             (cl-letf (((symbol-function 'write-region)
                        #'signal-write-failed))
               (save-buffer))
             ;; "Write failed"
             :type 'file-error))
          (should-not buffer-backed-up)
          (should     (buffer-modified-p))
          (should-not (file-writable-p file))
          (should     (equal (file-contents file) "foo\n"))
          (should     (equal (file-contents backup) 'missing))

          ;; Save to read-only file with backup, accepting prompt.
          (files-tests--with-yes-or-no-p
              (cons override-read-only-prompt t)
            (save-buffer))
          (should     buffer-backed-up)
          (should-not (buffer-modified-p))
          (should-not (file-writable-p file))
          (should-not (file-writable-p backup))
          (should     (equal (file-contents file) "bar\nfoo\n"))
          (should     (equal (file-contents backup) "foo\n"))

          ;; Prepare for tests not backing up the file.
          (setq buffer-backed-up nil)
          (delete-file backup)
          (goto-char (point-min))
          (insert "baz\n")

          ;; Save to read-only file without backup, accepting prompt,
          ;; experiencing a write error.  This tests that issue B of
          ;; bug#66546 is fixed.  The results of the "with backup" and
          ;; "without backup" subtests are identical when a write
          ;; error occurs, but the code paths to reach these results
          ;; are not.  In other words, this subtest is not redundant.
          (files-tests--with-yes-or-no-p
              (cons override-read-only-prompt t)
            (should-error
             (cl-letf (((symbol-function 'write-region)
                        #'signal-write-failed))
               (save-buffer 0))
             ;; "Write failed"
             :type 'file-error))
          (should-not buffer-backed-up)
          (should     (buffer-modified-p))
          (should-not (file-writable-p file))
          (should     (equal (file-contents file) "bar\nfoo\n"))
          (should     (equal (file-contents backup) 'missing))

          ;; Save to read-only file without backup, accepting prompt.
          ;; This tests that issue A of bug#66546 is fixed.
          (files-tests--with-yes-or-no-p
              (cons override-read-only-prompt t)
            (save-buffer 0))
          (should-not buffer-backed-up)
          (should-not (buffer-modified-p))
          (should-not (file-writable-p file))
          (should     (equal (file-contents file) "baz\nbar\nfoo\n"))
          (should     (equal (file-contents backup) 'missing)))))))

(ert-deftest files-tests-save-some-buffers ()
  "Test `save-some-buffers'.
Test the 3 cases for the second argument PRED, i.e., nil, t, or
predicate.
The value of `save-some-buffers-default-predicate' is ignored unless
PRED is nil."
  (let* ((foo-file-p (lambda () (string-suffix-p ".foo" buffer-file-name)))
         (bar-file-p (lambda () (string-suffix-p ".bar" buffer-file-name)))
         (args-results `((nil         nil                      nil nil)
                         (nil         ,foo-file-p              nil t)
                         (nil         ,bar-file-p              t nil)
                         (,foo-file-p nil                      nil t)
                         (,bar-file-p nil                      t nil)

                         (buffer-modified-p nil                nil nil)
                         (t           nil                      nil nil)
                         (t           ,foo-file-p              nil nil)

                         (,foo-file-p save-some-buffers-root   nil t)
                         (nil         save-some-buffers-root   nil t)
                         (,bar-file-p save-some-buffers-root   t   nil)
                         (t           save-some-buffers-root   nil nil))))
    (pcase-dolist (`(,pred ,def-pred-bind ,exp-1 ,exp-2) args-results)
      (files-tests--save-some-buffers pred def-pred-bind exp-1 exp-2))))

(defun files-tests--with-buffer-offer-save (buffers-offer fn-test args-results)
  "Helper macro to test `save-some-buffers' and `save-buffers-kill-emacs'.

This macro creates several non-file-visiting buffers in different
directories at the same level, i.e., none of them is a subdir of the
other.  Then it modifies the buffers and sets their `buffer-offer-save'
as specified by BUFFERS-OFFER, a list of elements (BUFFER OFFER-SAVE).
Finally, it calls FN-TEST from the first buffer.

FN-TEST is the function to test: either `save-some-buffers' or
`save-buffers-kill-emacs'.  This function is called with
`save-some-buffers-default-predicate' let-bound to a value
specified inside ARGS-RESULTS.

During the call to FN-TEST,`read-key' is overridden with a function that
just returns `n' and `kill-emacs' is overridden to do nothing.

ARGS-RESULTS is a list of elements (FN-ARGS CALLERS-DIR EXPECTED), where
FN-ARGS are the arguments for FN-TEST;
CALLERS-DIR specifies the value to let-bind
\`save-some-buffers-default-predicate';
 EXPECTED is the expected result of the test."
  (let* ((dir (make-temp-file "testdir" 'dir))
         (inhibit-message t)
         (use-dialog-box nil)
         (y-or-n-p-use-read-key t)
         buffers)
    (pcase-dolist (`(,bufsym ,offer-save) buffers-offer)
      (let* ((buf (generate-new-buffer (symbol-name bufsym)))
             (subdir (expand-file-name
                      (format "subdir-%s" (buffer-name buf))
                      dir)))
        (make-directory subdir 'parens)
        (push buf buffers)
        (with-current-buffer buf
          (cd subdir)
          (setq buffer-offer-save offer-save)
          (insert "foobar\n"))))
    (setq buffers (nreverse buffers))
    (let ((nb-saved-buffers 0))
      (unwind-protect
          (pcase-dolist (`(,fn-test-args ,callers-dir ,expected)
                         args-results)
            (setq nb-saved-buffers 0)
            (with-current-buffer (car buffers)
              (cl-letf
                  (((symbol-function 'read-key)
                    ;; Increase counter and answer 'n' when prompted
                    ;; to save a buffer.
                    (lambda (&rest _) (incf nb-saved-buffers) ?n))
                   ;; Do not kill Emacs.
                   ((symbol-function 'kill-emacs) #'ignore)
                   (save-some-buffers-default-predicate callers-dir))
                (apply fn-test fn-test-args)
                (should (equal nb-saved-buffers expected)))))
        ;; Clean up.
        (dolist (buf buffers)
          (with-current-buffer buf
            (set-buffer-modified-p nil)
            (kill-buffer buf)))
        (delete-directory dir 'recursive)))))

(defmacro files-tests-with-all-permutations (permutation list &rest body)
  "Execute BODY forms for all permutations of LIST.
Execute the forms with the symbol PERMUTATION bound to the current
permutation."
  (declare (indent 2) (debug (symbol form body)))
  (let ((vec (gensym "vec")))
    `(let ((,vec (vconcat ,list)))
       (cl-labels ((swap (,vec i j)
                         (let ((tmp (aref ,vec j)))
                           (aset ,vec j (aref ,vec i))
                           (aset ,vec i tmp)))
                   (permute (,vec l r)
                            (if (= l r)
                                (let ((,permutation (append ,vec nil)))
                                  ,@body)
                              (cl-loop for idx from l below (1+ r) do
                                       (swap ,vec idx l)
                                       (permute ,vec (1+ l) r)
                                       (swap ,vec idx l)))))
         (permute ,vec 0 (1- (length ,vec)))))))

(ert-deftest files-tests-buffer-offer-save ()
  "Test `save-some-buffers' for non-file-visiting buffers.
Check the behavior of `save-some-buffers' for non-file-visiting
buffers under several values of `buffer-offer-save'.
The value of `save-some-buffers-default-predicate' is ignored unless
PRED is nil."
  (let* ((buffers-offer-init '((buf-1 t) (buf-2 always) (buf-3 nil)))
         (nb-might-save
          (length
           (cl-remove-if (lambda (l) (null (cadr l))) buffers-offer-init)))
         (nb-always-save
          (length
           (cl-remove-if-not (lambda (l) (eq 'always (cadr l))) buffers-offer-init))))
    (files-tests-with-all-permutations
        buffers-offer
        buffers-offer-init
      (dolist (pred `(nil t))
        (dolist (callers-dir `(nil save-some-buffers-root))
          (let* ((head-offer (cadar buffers-offer))
                 (res (cond ((null pred)
                             (if (null callers-dir) nb-always-save (or (and head-offer 1) 0)))
                            (t
                             ;; Save any buffer with `buffer-offer-save' non-nil.
                             (if (eq pred t) nb-might-save
                               ;; Restrict to caller's dir.
                               (or (and head-offer 1) 0)))))
                 (args-res `(((nil ,pred) ,callers-dir ,res))))
            (files-tests--with-buffer-offer-save
             buffers-offer
             #'save-some-buffers
             args-res)))))))

(ert-deftest files-tests-save-buffers-kill-emacs--asks-to-save-buffers ()
  "Test that `save-buffers-kill-emacs' asks to save buffers as expected.
Prompt users for any modified buffer with `buffer-offer-save' non-nil."
  (let* ((buffers-offer-init '((buf-1 t) (buf-2 always) (buf-3 nil)))
         (nb-might-save
          (length
           (cl-remove-if (lambda (l) (null (cadr l))) buffers-offer-init))))
    (files-tests-with-all-permutations
        buffers-offer
        buffers-offer-init
      (files-tests--with-buffer-offer-save
       buffers-offer
       #'save-buffers-kill-emacs
       `((nil nil ,nb-might-save)
         ;; `save-some-buffers-default-predicate' (i.e. the 2nd element) is ignored.
         (nil save-some-buffers-root ,nb-might-save))))))

(ert-deftest test-file-name-split ()
  (should (equal (file-name-split "foo/bar") '("foo" "bar")))
  (should (equal (file-name-split "/foo/bar") '("" "foo" "bar")))
  (should (equal (file-name-split "/foo/bar/zot") '("" "foo" "bar" "zot")))
  (should (equal (file-name-split "/foo/bar/") '("" "foo" "bar" "")))
  (should (equal (file-name-split "foo/bar/") '("foo" "bar" ""))))

(ert-deftest files-test-set-mode ()
  (find-file (ert-resource-file "file-mode"))
  (should (eq major-mode 'text-mode))
  (emacs-lisp-mode)
  ;; Check that the mode cookie doesn't override the explicit setting.
  (should (eq major-mode 'emacs-lisp-mode)))

(ert-deftest files-test-set-mode-multiple ()
  (find-file (ert-resource-file "file-mode-multiple"))
  (should (eq major-mode 'outline-mode)))

(ert-deftest files-test-set-mode-prop-line ()
  (find-file (ert-resource-file "file-mode-prop-line"))
  (should (eq major-mode 'text-mode)))

(ert-deftest files-load-elc-gz-file ()
  (skip-unless (executable-find "gzip"))
  (ert-with-temp-directory dir
    (let* ((pref (expand-file-name "compile-utf8" dir))
           (el (concat pref ".el")))
      (copy-file (ert-resource-file "compile-utf8.el") el)
      (push dir load-path)
      (should (load pref t))
      (should (fboundp 'foo))
      (should (documentation 'foo))
      (should (documentation 'bar))
      (should (documentation 'zot))

      (byte-compile-file el)
      (fmakunbound 'foo)
      (should (load (concat pref ".elc") t))
      (should (fboundp 'foo))
      (should (documentation 'foo))
      (should (documentation 'bar))
      (should (documentation 'zot))

      (dired-compress-file (concat pref ".elc"))
      (fmakunbound 'foo)
      (should (load (concat pref ".elc.gz") t))
      (should (fboundp 'foo))
      ;; This fails due to bug#12598.
      (should (documentation 'foo))
      (should (documentation 'bar))
      (should (documentation 'zot)))))

(ert-deftest files-tests--expand-wildcards ()
  (should (file-expand-wildcards
           (concat (directory-file-name default-directory) "*/"))))

(provide 'files-tests)
;;; files-tests.el ends here
