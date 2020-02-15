;;; files-tests.el --- tests for files.el.  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2020 Free Software Foundation, Inc.

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
      (eval (nth 2 test-settings)))))

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

(defvar files-test-bug-18141-file
  (expand-file-name "data/files-bug18141.el.gz" (getenv "EMACS_TEST_DIRECTORY"))
  "Test file for bug#18141.")

(ert-deftest files-tests-bug-18141 ()
  "Test for https://debbugs.gnu.org/18141 ."
  (skip-unless (executable-find "gzip"))
  ;; If called interactively, environment variable
  ;; $EMACS_TEST_DIRECTORY does not exist.
  (skip-unless (file-exists-p files-test-bug-18141-file))
  (let ((tempfile (make-temp-file "files-test-bug-18141" nil ".gz")))
    (unwind-protect
	(progn
	  (copy-file files-test-bug-18141-file tempfile t)
	  (with-current-buffer (find-file-noselect tempfile)
	    (set-buffer-modified-p t)
	    (save-buffer)
	    (should (eq buffer-file-coding-system 'iso-2022-7bit-unix))))
      (delete-file tempfile))))

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
  :expected-result :failed
  (let ((input-result
         '(("/foo/bar//baz/:/bar/foo/baz//" nil ("/foo/bar/baz/" "/bar/foo/baz/"))
           ("/foo/bar/:/bar/qux/:/qux/foo" nil ("/foo/bar/" "/bar/qux/" "/qux/foo/"))
           ("//foo/bar/:/bar/qux/:/qux/foo/" nil ("/foo/bar/" "/bar/qux/" "/qux/foo/"))
           ("/foo/bar/:/bar/qux/:/qux/foo/" nil ("/foo/bar/" "/bar/qux/" "/qux/foo/"))
           ("/foo//bar/:/bar/qux/:/qux/foo/" nil ("/foo/bar/" "/bar/qux/" "/qux/foo/"))
           ("/foo//bar/:/bar/qux/:/qux/foo" nil ("/foo/bar/" "/bar/qux/" "/qux/foo/"))
           ("/foo/bar" "$FOO/baz/:/qux/foo/" ("/foo/bar/baz/" "/qux/foo/"))
           ("//foo/bar/" "$FOO/baz/:/qux/foo/" ("/foo/bar/baz/" "/qux/foo/"))))
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
              (lambda (&optional arg) (push arg kill-emacs-args)))
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
    (should (equal kill-emacs-args '(nil)))))

(ert-deftest files-tests-read-file-in-~ ()
  "Test file prompting in directory named `~'.
If we are in a directory named `~', the default value should not
be $HOME."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _coll &optional _pred _req init _hist def _)
               (or def init)))
            (dir (make-temp-file "read-file-name-test" t)))
    (unwind-protect
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
                         (concat "/:/:" subdir)))))
      (delete-directory dir 'recursive))))

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
  "Check that Bug#25949 is fixed."
  (skip-unless (executable-find "true"))
  (let ((default-directory (file-name-quote temporary-file-directory)))
    (should (zerop (process-file "true")))
    (should (processp (start-file-process "foo" nil "true")))
    (should (zerop (shell-command "true")))))

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

(defmacro files-tests--with-temp-file (name &rest body)
  (declare (indent 1) (debug (symbolp body)))
  (cl-check-type name symbol)
  `(let ((,name (make-temp-file "emacs")))
     (unwind-protect
         (progn ,@body)
       (delete-file ,name))))

(ert-deftest files-tests-file-name-non-special--buffers ()
  "Check that Bug#25951 is fixed.
We call `verify-visited-file-modtime' on a buffer visiting a file
with a quoted name.  We use two different variants: first with
the buffer current and a nil argument, second passing the buffer
object explicitly.  In both cases no error should be raised and
the `file-name-non-special' handler for quoted file names should
be invoked with the right arguments."
  (files-tests--with-temp-file temp-file-name
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
          (,name (make-temp-file "files-tests" ,dir-flag))
          (,non-special-name (file-name-quote ,name)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,name)
         (if ,dir-flag (delete-directory ,name t)
           (delete-file ,name)))
       (when (file-exists-p ,non-special-name)
         (if ,dir-flag (delete-directory ,non-special-name t)
           (delete-file ,non-special-name))))))

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
           (delete-file ,non-special-name))))))

(defun files-tests--new-name (name part)
  (let (file-name-handler-alist)
    (concat (file-name-sans-extension name) part (file-name-extension name t))))

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
  (when (string-match-p ":" (expand-file-name temporary-file-directory))
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
      (should-not (equal (file-name-all-completions
                          nospecial-file nospecial-tempdir)
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
      (should-not (equal (file-name-completion nospecial-file nospecial-tempdir)
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
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should-not (get-file-buffer nospecial)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-not (get-file-buffer nospecial))))

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
    (should (load nospecial nil t))))

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

(ert-deftest files-tests-file-name-non-special-make-directory-internal ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (let ((default-directory nospecial-dir))
      (make-directory-internal "dir")
      (should (file-directory-p "dir"))
      (delete-directory "dir")))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (let ((default-directory nospecial-dir))
      (should-error (make-directory-internal "dir")))))

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

;; See `files-tests--file-name-non-special--subprocess'.
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
    (set-file-times nospecial))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should-error (set-file-times nospecial))))

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
              (sleep-for 0.1)))))))
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
      (equal (substitute-in-file-name nospecial-foo) nospecial-foo)))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (let ((process-environment (cons "FOO=foo" process-environment))
          (nospecial-foo (files-tests--new-name nospecial "$FOO")))
      ;; The "/:" prevents substitution.
      (equal (substitute-in-file-name nospecial-foo) nospecial-foo))))

(ert-deftest files-tests-file-name-non-special-temporary-file-directory ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (let ((default-directory nospecial-dir))
      (equal (temporary-file-directory) temporary-file-directory)))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (let ((default-directory nospecial-dir))
      (equal (temporary-file-directory) temporary-file-directory))))

(ert-deftest files-tests-file-name-non-special-unhandled-file-name-directory ()
  (files-tests--with-temp-non-special (tmpdir nospecial-dir t)
    (equal (unhandled-file-name-directory nospecial-dir)
           (file-name-as-directory tmpdir)))
  (files-tests--with-temp-non-special-and-file-name-handler
      (tmpdir nospecial-dir t)
    (equal (unhandled-file-name-directory nospecial-dir)
           (file-name-as-directory tmpdir))))

(ert-deftest files-tests-file-name-non-special-vc-registered ()
  (files-tests--with-temp-non-special (tmpfile nospecial)
    (should (equal (vc-registered nospecial) (vc-registered tmpfile))))
  (files-tests--with-temp-non-special-and-file-name-handler (tmpfile nospecial)
    (should (equal (vc-registered nospecial) (vc-registered tmpfile)))))

;; See test `files-tests--file-name-non-special--buffers'.
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
  (let* ((dir (make-temp-file "files-mkdir-test" t))
	 (dirname (file-name-as-directory dir))
	 (file (concat dirname "file"))
	 (subdir1 (concat dirname "subdir1"))
	 (subdir2 (concat dirname "subdir2"))
	 (a/b (concat dirname "a/b")))
    (write-region "" nil file)
    (should-error (make-directory "/"))
    (should-not (make-directory "/" t))
    (should-error (make-directory dir))
    (should-not (make-directory dir t))
    (should-error (make-directory dirname))
    (should-not (make-directory dirname t))
    (should-error (make-directory file))
    (should-error (make-directory file t))
    (should-not (make-directory subdir1))
    (should-not (make-directory subdir2 t))
    (should-error (make-directory a/b))
    (should-not (make-directory a/b t))
    (delete-directory dir 'recursive)))

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
    (files-tests--with-temp-file temp-file-name
      (with-current-buffer (find-file-noselect temp-file-name)
        (setq write-contents-functions nil)
        (insert "p")
        (should (null (save-buffer)))
        (should (eq (buffer-size) 1))))))

(ert-deftest files-tests-copy-directory ()
  (let* ((dir (make-temp-file "files-mkdir-test" t))
	 (dirname (file-name-as-directory dir))
	 (source (concat dirname "source"))
	 (dest (concat dirname "dest/new/directory/"))
	 (file (concat (file-name-as-directory source) "file"))
	 (source2 (concat dirname "source2"))
	 (dest2 (concat dirname "dest/new2")))
    (make-directory source)
    (write-region "" nil file)
    (copy-directory source dest t t t)
    (should (file-exists-p (concat dest "file")))
    (make-directory (concat (file-name-as-directory source2) "a") t)
    (copy-directory source2 dest2)
    (should (file-directory-p (concat (file-name-as-directory dest2) "a")))
    (delete-directory dir 'recursive)))

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
  (let ((tmpfile (make-temp-file "files-test" nil (car exec-suffixes))))
    (unwind-protect
        (progn
          (set-file-modes tmpfile #o777)
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
                    (executable-find (file-name-nondirectory tmpfile))))))
      (delete-file tmpfile))))

(ert-deftest files-tests-dont-rewrite-precious-files ()
  "Test that `file-precious-flag' forces files to be saved by
renaming only, rather than modified in-place."
  (let* ((temp-file-name (make-temp-file "files-tests"))
         (advice (lambda (_start _end filename &rest _r)
                   (should-not (string= filename temp-file-name)))))
    (unwind-protect
        (with-current-buffer (find-file-noselect temp-file-name)
          (advice-add #'write-region :before advice)
          (setq-local file-precious-flag t)
          (insert "foobar")
          (should (null (save-buffer))))
      (ignore-errors (advice-remove #'write-region advice))
      (ignore-errors (delete-file temp-file-name)))))

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
  (should (equal (file-size-human-readable-iec 72528034765) "67.5 GiB")))

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

(provide 'files-tests)
;;; files-tests.el ends here
