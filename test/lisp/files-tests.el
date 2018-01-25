;;; files-tests.el --- tests for files.el.  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2018 Free Software Foundation, Inc.

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

(ert-deftest files-test-local-variables ()
  "Test the file-local variables implementation."
  (unwind-protect
      (progn
	(defadvice hack-local-variables-confirm (around files-test activate)
	  (setq files-test-result 'query)
	  nil)
	(dolist (test files-test-local-variable-data)
	  (let ((str (concat "text\n\n;; Local Variables:\n;; "
			     (mapconcat 'identity (car test) "\n;; ")
			     "\n;; End:\n")))
	    (dolist (subtest (cdr test))
	      (should (file-test--do-local-variables-test str subtest))))))
    (ad-disable-advice 'hack-local-variables-confirm 'around 'files-test)))

(defvar files-test-bug-18141-file
  (expand-file-name "data/files-bug18141.el.gz" (getenv "EMACS_TEST_DIRECTORY"))
  "Test file for bug#18141.")

(ert-deftest files-test-bug-18141 ()
  "Test for https://debbugs.gnu.org/18141 ."
  (skip-unless (executable-find "gzip"))
  (let ((tempfile (make-temp-file "files-test-bug-18141" nil ".gz")))
    (unwind-protect
	(progn
	  (copy-file files-test-bug-18141-file tempfile t)
	  (with-current-buffer (find-file-noselect tempfile)
	    (set-buffer-modified-p t)
	    (save-buffer)
	    (should (eq buffer-file-coding-system 'iso-2022-7bit-unix))))
      (delete-file tempfile))))

(ert-deftest files-test-make-temp-file-empty-prefix ()
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


(ert-deftest files-test-bug-21454 ()
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

(ert-deftest files-test--save-buffers-kill-emacs--confirm-kill-processes ()
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

(ert-deftest files-test-read-file-in-~ ()
  "Test file prompting in directory named '~'.
If we are in a directory named '~', the default value should not
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

(ert-deftest files-tests--file-name-non-special--subprocess ()
  "Check that Bug#25949 is fixed."
  (skip-unless (executable-find "true"))
  (let ((defdir (if (memq system-type '(ms-dos windows-nt)) "/:c:/" "/:/")))
    (should (eq (let ((default-directory defdir)) (process-file "true")) 0))
    (should (processp (let ((default-directory defdir))
                        (start-file-process "foo" nil "true"))))
    (should (eq (let ((default-directory defdir)) (shell-command "true")) 0))))

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

(defmacro files-tests--with-temp-dir (name &rest body)
  (declare (indent 1) (debug (symbolp body)))
  (cl-check-type name symbol)
  `(let ((,name (make-temp-file "emacs" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,name t))))

(ert-deftest files-tests--file-name-non-special--buffers ()
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
       (insert-file-contents (concat "/:" temp-file-name) :visit)
       (should (stringp buffer-file-name))
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

(ert-deftest files-file-name-non-special-notify-handlers ()
  :expected-result :failed
  (files-tests--with-temp-file tmpfile
    (let* ((nospecial (concat "/:" tmpfile))
           (watch (file-notify-add-watch nospecial '(change) #'ignore)))
      (should (file-notify-valid-p watch))
      (file-notify-rm-watch watch))))


(ert-deftest files-file-name-non-special-handlers ()
  (files-tests--with-temp-file tmpfile
    (files-tests--with-temp-dir tmpdir
      (let ((nospecial (concat "/:" tmpfile))
            (nospecial-dir (concat "/:" tmpdir)))
        (should (null (access-file nospecial "test")))
        (let ((newname (concat nospecial "add-name")))
          (add-name-to-file nospecial newname)
          (should (file-exists-p newname)))
        (should (equal (byte-compiler-base-file-name nospecial)
                       (byte-compiler-base-file-name tmpfile)))
        (let ((newname (concat (directory-file-name nospecial-dir)
                               "copy-dir")))
          (copy-directory nospecial-dir newname)
          (should (file-directory-p newname))
          (delete-directory newname)
          (should-not (file-directory-p newname)))
        (let ((newname (concat (directory-file-name nospecial)
                               "copy-file")))
          (copy-file nospecial newname)
          (should (file-exists-p newname))
          (delete-file newname)
          (should-not (file-exists-p newname)))
        (should (equal (diff-latest-backup-file nospecial)
                       (diff-latest-backup-file tmpfile)))
        (should (equal (directory-file-name nospecial-dir)
                       (concat "/:" (directory-file-name tmpdir))))
        (should (equal (directory-files nospecial-dir)
                       (directory-files tmpdir)))
        (should (equal (directory-files-and-attributes nospecial-dir)
                       (directory-files-and-attributes tmpdir)))
        (dired-compress-file (dired-compress-file nospecial))
        (dired-uncache nospecial-dir)
        (should (equal (expand-file-name nospecial)
                       nospecial))
        (should (file-accessible-directory-p nospecial-dir))
        (should (equal (file-acl nospecial)
                       (file-acl tmpfile)))
        (should (equal (file-attributes nospecial)
                       (file-attributes tmpfile)))
        (should (equal (file-directory-p nospecial-dir)
                       (file-directory-p tmpdir)))
        (should (file-equal-p nospecial tmpfile))
        (should (file-equal-p tmpfile nospecial))
        (should-not (file-executable-p nospecial))
        (should (file-exists-p nospecial))
        (should (file-in-directory-p nospecial temporary-file-directory))
        (should-not (file-in-directory-p nospecial nospecial-dir))
        (should-not (file-in-directory-p tmpfile nospecial-dir))
        (should-not (file-local-copy nospecial)) ; Already local.
        (should (equal (file-modes nospecial)
                       (file-modes tmpfile)))
        (should (equal (file-name-all-completions nospecial nospecial-dir)
                       (file-name-all-completions tmpfile tmpdir)))
        (file-name-as-directory nospecial)
        (should (equal (file-name-case-insensitive-p nospecial)
                       (file-name-case-insensitive-p tmpfile)))
        (should (equal (file-name-completion nospecial nospecial-dir)
                       (file-name-completion tmpfile tmpdir)))
        (should (equal (file-name-directory nospecial)
                       (concat "/:" temporary-file-directory)))
        (should (equal (file-name-nondirectory nospecial)
                       (file-name-nondirectory tmpfile)))
        (should (equal (file-name-sans-versions nospecial)
                       nospecial))
        (should-not (file-newer-than-file-p nospecial tmpfile))
        (should (equal (file-ownership-preserved-p nospecial)
                       (file-ownership-preserved-p tmpfile)))
        (should (file-readable-p nospecial))
        (should (file-regular-p nospecial))
        (should-not (file-remote-p nospecial))
        (should (equal (file-selinux-context nospecial)
                       (file-selinux-context tmpfile)))
        (should-not (file-symlink-p nospecial))
        (file-truename nospecial)
        (should (file-writable-p nospecial))
        (should (equal (find-backup-file-name nospecial)
                       (mapcar (lambda (f) (concat "/:" f))
                               (find-backup-file-name tmpfile))))
        (should-not (get-file-buffer nospecial))
        (should (equal (with-temp-buffer
                         (insert-directory nospecial-dir nil)
                         (buffer-string))
                       (with-temp-buffer
                         (insert-directory tmpdir nil)
                         (buffer-string))))
        (with-temp-buffer
          (insert-file-contents nospecial)
          (should (zerop (buffer-size))))
        (should (load nospecial))
        (save-current-buffer
          (should (equal (prog2 (set-buffer (find-file-noselect nospecial))
                             (make-auto-save-file-name)
                           (kill-buffer))
                         (prog2 (set-buffer (find-file-noselect tmpfile))
                             (make-auto-save-file-name)
                           (kill-buffer)))))
        (let ((default-directory nospecial-dir))
          (make-directory "dir")
          (should (file-directory-p "dir"))
          (delete-directory "dir")
          (make-directory-internal "dir")
          (should (file-directory-p "dir"))
          (delete-directory "dir")
          (let ((near-tmpfile (make-nearby-temp-file "file")))
            (should (file-exists-p near-tmpfile))
            (delete-file near-tmpfile)))
        (let* ((linkname (expand-file-name "link" tmpdir))
               (may-symlink (ignore-errors (make-symbolic-link tmpfile linkname)
                                           t)))
          (when may-symlink
            (should (file-symlink-p linkname))
            (delete-file linkname)
            (let ((linkname (expand-file-name "link" nospecial-dir)))
              (make-symbolic-link tmpfile linkname)
              (should (file-symlink-p linkname))
              (delete-file linkname))))
        ;; `files-tests--file-name-non-special--subprocess' already
        ;; tests `process-file'.
        (rename-file nospecial (concat nospecial "x"))
        (rename-file (concat nospecial "x") nospecial)
        (rename-file tmpfile (concat nospecial "x"))
        (rename-file (concat nospecial "x") nospecial)
        (rename-file nospecial (concat tmpfile "x"))
        (rename-file (concat nospecial "x") nospecial)
        (set-file-acl nospecial (file-acl nospecial))
        (set-file-modes nospecial (file-modes nospecial))
        (set-file-selinux-context nospecial (file-selinux-context nospecial))
        (set-file-times nospecial)
        ;; `files-tests--file-name-non-special--buffers' already tests
        ;; `verify-visited-file-modtime'.
        (with-temp-buffer
          (write-region nil nil nospecial nil :visit))
        (save-current-buffer
          (set-buffer (find-file-noselect nospecial))
          (set-visited-file-modtime)
          (kill-buffer))
        (with-temp-buffer
          (let ((default-directory nospecial-dir))
            (shell-command (concat (shell-quote-argument
                                    (concat invocation-directory invocation-name))
                                   " --version")
                           (current-buffer))
            (goto-char (point-min))
            (should (search-forward emacs-version nil t))))
        (with-temp-buffer
          (let ((default-directory nospecial-dir))
            (let ((proc (start-file-process
                         "emacs" (current-buffer)
                         (concat invocation-directory invocation-name)
                         "--version")))
              (accept-process-output proc)
              (goto-char (point-min))
              (should (search-forward emacs-version nil t))
              (kill-process proc)
              (accept-process-output proc ))))
        (let ((process-environment (cons "FOO=foo" process-environment)))
          ;; The "/:" prevents substitution.
          (equal (substitute-in-file-name nospecial) nospecial))
        (let ((default-directory nospecial-dir))
          (equal (temporary-file-directory) temporary-file-directory))
        (equal (unhandled-file-name-directory nospecial-dir)
               (file-name-as-directory tmpdir))
        (should (equal (vc-registered nospecial)
                       (vc-registered tmpfile)))))))

(ert-deftest files-tests--insert-directory-wildcard-in-dir-p ()
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

(ert-deftest files-tests--make-directory ()
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

(ert-deftest files-test-no-file-write-contents ()
  "Test that `write-contents-functions' permits saving a file.
Usually `basic-save-buffer' will prompt for a file name if the
current buffer has none.  It should first call the functions in
`write-contents-functions', and if one of them returns non-nil,
consider the buffer saved, without prompting for a file
name (Bug#28412)."
  (let ((read-file-name-function
         (lambda (&rest _ignore)
           (error "Prompting for file name"))))
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

(ert-deftest files-tests--copy-directory ()
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

(ert-deftest files-test-abbreviated-home-dir ()
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

(provide 'files-tests)
;;; files-tests.el ends here
