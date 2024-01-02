;;; mh-utils-tests.el --- tests for mh-utils.el -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

;; This test suite runs tests that use and depend on MH programs
;; installed on the system.

;; When running such tests, MH-E can use a particular MH variant
;; installed on the system, or it can use the mocks provided here.
;; (Setup is done by the `with-mh-test-env' macro.)

;; By setting environment variable TEST_MH_PATH, you can select which of
;; the installed MH variants to use, or ignore them all and use mocks.
;; See also the script test-all-mh-variants.sh in this directory.

;; 1.  To run these tests against the default MH variant installed on
;;     this system:
;; cd ../.. && make lisp/mh-e/mh-utils-tests

;; 2.  To run these tests against an MH variant installed in a
;;     specific directory, set TEST_MH_PATH, as in this example:
;; cd ../.. && make lisp/mh-e/mh-utils-tests TEST_MH_PATH=/usr/local/nmh/bin

;; 3.  To search for and run these tests against all MH variants
;;     installed on this system:
;; ./test-all-mh-variants.sh

;; Setting the environment variable TEST_MH_DEBUG or the Lisp variable
;; mh-test-utils-debug-mocks logs access to the file system during the test.

;;; Code:

(require 'ert)
(require 'mh-utils)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(ert-deftest mh-quote-pick-expr ()
  "Test `mh-quote-pick-expr'."
  (should (equal nil (mh-quote-pick-expr nil)))
  (should (equal '() (mh-quote-pick-expr '())))
  (should (equal '("foo") (mh-quote-pick-expr '("foo"))))
  (should (equal '("^\\[foo]?\\*+\\.\\$")
                 (mh-quote-pick-expr '("^[foo]?*+.$"))))
  (should (equal '("^\\[foo]?\\*+\\.\\$" "bar" "baz\\$")
                 (mh-quote-pick-expr '("^[foo]?*+.$" "bar" "baz$")))))

(ert-deftest mh-normalize-folder-name ()
  "Test `mh-normalize-folder-name'."
  (should (equal nil (mh-normalize-folder-name nil)))
  (should (equal "+" (mh-normalize-folder-name "")))
  (should (equal "" (mh-normalize-folder-name "" t)))
  (should (equal nil (mh-normalize-folder-name "" nil nil t)))
  (should (equal nil (mh-normalize-folder-name "+" nil nil t)))
  (should (equal nil (mh-normalize-folder-name "+" t t t)))
  (should (equal "+inbox" (mh-normalize-folder-name "inbox")))
  (should (equal "+inbox" (mh-normalize-folder-name "+inbox")))
  (should (equal "+inbox" (mh-normalize-folder-name "+inbox/")))
  (should (equal "+inbox/" (mh-normalize-folder-name "+inbox/" t t t)))
  (should (equal "+inbox/" (mh-normalize-folder-name "+inbox/" nil t)))
  (should (equal "+news" (mh-normalize-folder-name "+inbox////../news")))
  (should (equal "+news" (mh-normalize-folder-name "+inbox////../news/")))
  (should (equal "+news/"
                 (mh-normalize-folder-name "+inbox////../news/" nil t)))
  (should (equal "+inbox/news" (mh-normalize-folder-name "+inbox////./news"))))

(ert-deftest mh-sub-folders-parse-no-folder ()
  "Test `mh-sub-folders-parse' with no starting folder."
  (let (others-position)
    (with-temp-buffer
      (insert "lines without has-string are ignored\n")
      (insert "onespace has no messages.\n")
      (insert "twospace  has no messages.\n")
      (insert "  precedingblanks  has no messages.\n")
      (insert ".leadingdot  has no messages.\n")
      (insert "#leadinghash  has no messages.\n")
      (insert ",leadingcomma  has no messages.\n")
      (insert "withothers  has no messages ; (others)")
      (setq others-position (point))
      (insert ".\n")
      (insert "curf   has  no messages.\n")
      (insert "curf+  has 123 messages.\n")
      (insert "curf2+ has  17 messages.\n")
      (insert "\ntotal after blank line is ignored  has no messages.\n")
      (should (equal
               (mh-sub-folders-parse nil "curf+")
               (list '("onespace") '("twospace") '("precedingblanks")
                     (cons "withothers" others-position)
                     '("curf") '("curf") '("curf2+")))))))

(ert-deftest mh-sub-folders-parse-relative-folder ()
  "Test `mh-sub-folders-parse' with folder."
  (let (others-position)
    (with-temp-buffer
      (insert "testf+  has no messages.\n")
      (insert "testf/sub1  has no messages.\n")
      (insert "testf/sub2  has no messages ; (others)")
      (setq others-position (point))
      (insert ".\n")
      (should (equal
               (mh-sub-folders-parse "+testf" "testf+")
               (list '("sub1") (cons "sub2" others-position)))))))

(ert-deftest mh-sub-folders-parse-root-folder ()
  "Test `mh-sub-folders-parse' with root folder."
  (with-temp-buffer
    (insert "/+  has no messages.\n")
    (insert "/   has no messages.\n")
    (insert "//nmh-style  has no messages.\n")
    (insert "/mu-style  has no messages.\n")
    (should (equal
             (mh-sub-folders-parse "+/" "inbox+")
             '(("") ("nmh-style") ("mu-style"))))))


;; Folder names that are used by the following tests.
(defvar mh-test-rel-folder "rela-folder")
(defvar mh-test-abs-folder "/abso-folder")
(defvar mh-test-no-such-folder "/testdir/none" "A folder that does not exist.")

(defvar mh-test-utils-variants nil
  "The value of `mh-variants' used for these tests.
This variable allows setting `mh-variants' to a limited set for targeted
testing.  Its value can be different from the normal value when
environment variable TEST_MH_PATH is set.  By remembering the value, we
can log the choice only once, which makes the batch log easier to read.")

(defvar mh-test-variant-logged-already nil
  "Whether `with-mh-test-env' has written the MH variant to the log.")

(defvar mh-test-utils-debug-mocks (> (length (getenv "TEST_MH_DEBUG")) 0)
  "Whether to log detailed behavior of mock functions.")

(defvar mh-test-call-process-real (symbol-function 'call-process))
(defvar mh-test-file-directory-p-real (symbol-function 'file-directory-p))

;;; The macro with-mh-test-env wraps tests that touch the file system
;;; and/or run programs.

(defmacro with-mh-test-env (&rest body)
  "Evaluate BODY with a test mail environment.
Functions that touch the file system or run MH programs are either
mocked out or pointed at a test tree.  Uses `mh-test-utils-setup' to
select which."
  (declare (indent 0) (debug t))
  `(cl-letf ((temp-home-dir nil)
             ;; make local bindings for things we will modify for test env
             (mh-user-path)
             (mh-test-abs-folder)
             ((symbol-function 'call-process))
             ((symbol-function 'file-directory-p))
             ;; the test always gets its own sub-folders cache
             (mh-sub-folders-cache (make-hash-table :test #'equal))
             ;; Allow envvar TEST_MH_PATH to control mh-variants.
             (mh-variants mh-test-utils-variants)
             ;; remember the original value
             (original-mh-test-variant-logged mh-test-variant-logged-already)
             (original-mh-path mh-path)
             (original-mh-sys-path mh-sys-path)
             (original-exec-path exec-path)
             (original-mh-variant-in-use mh-variant-in-use)
             (original-mh-progs mh-progs)
             (original-mh-lib mh-lib)
             (original-mh-lib-progs mh-lib-progs)
             (original-mh-envvar (getenv "MH")))
     (unwind-protect
         (progn
           (setq temp-home-dir (mh-test-utils-setup))
           ,@body)
       (unless noninteractive
         ;; If interactive, forget that we logged the variant and
         ;; restore any changes TEST_MH_PATH made.
         (setq mh-test-variant-logged-already original-mh-test-variant-logged
               mh-path original-mh-path
               mh-sys-path original-mh-sys-path
               exec-path original-exec-path
               mh-variant-in-use original-mh-variant-in-use
               mh-progs original-mh-progs
               mh-lib original-mh-lib
               mh-lib-progs original-mh-lib-progs))
       (if temp-home-dir (delete-directory temp-home-dir t))
       (setenv "MH" original-mh-envvar))))

(defun mh-test-utils-setup ()
  "Set dynamically bound variables needed by mock and/or variants.
Call `mh-variant-set' to look through the directories named by
environment variable `TEST_MH_PATH' (default: `mh-path' and `mh-sys-path')
to find the MH variant to use, if any.
Return the name of the root of the created directory tree, if any."
  (when (getenv "TEST_MH_PATH")
    ;; force mh-variants to use only TEST_MH_PATH
    (setq mh-path (split-string (getenv "TEST_MH_PATH") path-separator t)
          mh-sys-path nil
          exec-path '("/bin" "/usr/bin")))
  (unless mh-test-variant-logged-already
    (mh-variant-set mh-variant)
    (setq mh-test-utils-variants mh-variants)
    (setq mh-test-variant-logged-already t))
  (when (native-comp-available-p)
    ;; As `call-process'' and `file-directory-p' will be redefined, the
    ;; native compiler will invoke `call-process' to compile the
    ;; respective trampolines.  To avoid interference with the
    ;; `call-process' mocking, we build these ahead of time.
    (mapc #'comp-subr-trampoline-install '(call-process file-directory-p)))
  (if mh-variant-in-use
      (mh-test-utils-setup-with-variant)
    (mh-test-utils-setup-with-mocks)))

(defun mh-test-utils-setup-with-mocks ()
  "Set dynamically bound variables so that MH programs are mocked out.
The tests use this method if no configured MH variant is found."
  (setq mh-user-path "/testdir/Mail/")
  (mh-populate-sub-folders-cache "+")
  (mh-populate-sub-folders-cache "+rela-folder")
  (mh-populate-sub-folders-cache "+rela-folder/bar")
  (mh-populate-sub-folders-cache "+rela-folder/foo")
  (mh-populate-sub-folders-cache "+rela-folder/food")
  (fset 'call-process #'mh-test-utils-mock-call-process)
  (fset 'file-directory-p #'mh-test-utils-mock-file-directory-p)
  ;; no temp directory created
  nil)

(defun mh-test-utils-mock-call-process (program
                                        &optional _infile _destination _display
                                        &rest args)
  "A mocked version of `call-process' that calls no processes."
  (let ((argument-responses
         ;; assoc list of program arguments and lines to output.
         '((("folder" "-fast") . ("rela-folder"))
           (("folders" "-noheader" "-norecurse" "-nototal") .
            ("rela-folder  has no messages."))
           (("folders" "-noheader" "-norecurse" "-nototal" "+rela-folder") .
            ("rela-folder+      has no messages."
             "rela-folder/bar   has no messages."
             "rela-folder/foo   has no messages."
             "rela-folder/food  has no messages."))
           (("folders" "-noheader" "-norecurse" "-nototal" "+rela-folder/foo") .
            ("rela-folder/foo+ has no messages."))
           (("folders" "-noheader" "-norecurse" "-nototal" "+") .
            ("+ has no messages."))
           (("folders" "-noheader" "-norecurse" "-nototal" "+/abso-folder") .
            ("/abso-folder+      has no messages."
             "/abso-folder/bar   has no messages."
             "/abso-folder/foo   has no messages."
             "/abso-folder/food  has no messages."))
           (("folders" "-noheader" "-norecurse" "-nototal" "+/") .
            ("/+             has no messages ; (others)."
             "/abso-folder   has no messages ; (others)."
             "/tmp           has no messages ; (others)."))
           ))
        (arglist (cons (file-name-base program) args)))
    (let ((response-list-cons (assoc arglist argument-responses)))
      (cond (response-list-cons
             (let ((response-list (cdr response-list-cons)))
               (when mh-test-utils-debug-mocks
                 (message "call-process mock arglist %s" arglist)
                 (message " -> response %S" response-list))
               (while response-list
                 (insert (car response-list) "\n")
                 (setq response-list (cdr response-list))))
             0)
            (t
             (message "call-process mock unexpected arglist %s" arglist)
             1)))))

(defun mh-test-utils-mock-file-directory-p (filename)
  "A mocked version of `file-directory-p' that does not access the file system."
  (let ((directories '("" "/" "/tmp" "/abso-folder" "/abso-folder/foo"
                       "/testdir/Mail" "/testdir/Mail/rela-folder"
                       "/testdir/Mail/rela-folder/foo"
                       "rela-folder" "rela-folder/foo"))
        (non-directories '("/abso-folder/fo" "rela-folder/fo"
                           "/testdir/Mail/rela-folder/fo"
                           "/testdir/Mail/nosuchfolder"
                           "/nosuchfolder" "nosuchfolder")))
    (cond ((member (directory-file-name filename) directories)
           (when mh-test-utils-debug-mocks
             (message "file-directory-p mock: %S -> t" filename))
           t)
          ((member (directory-file-name filename) non-directories)
           (when mh-test-utils-debug-mocks
             (message "file-directory-p mock: %S -> nil" filename))
           nil)
          (t
           (message "file-directory-p mock unexpected filename: %S" filename)
           nil))))

(defun mh-test-utils-setup-with-variant ()
  "Create a temporary directory structure for actual MH programs to read.
Return the name of the root of the created directory tree.
Set dynamically bound variables so that MH programs may log.
The tests use this method if a configured MH variant is found."
  (let* ((temp-home-dir
          (make-temp-file "emacs-mh-e-unit-test-" t))
         (profile (expand-file-name
                   ".mh_profile" temp-home-dir))
         (mail-dir (expand-file-name "Mail" temp-home-dir))
         (rela-folder (expand-file-name
                       "rela-folder" mail-dir))
         (abso-folder (expand-file-name
                       "abso-folder" temp-home-dir)))
    (with-temp-file profile
      (insert "Path: " mail-dir "\n" "Welcome: disable\n"))
    (setenv "MH" profile)
    (make-directory (expand-file-name "bar" rela-folder) t)
    (make-directory (expand-file-name "foo" rela-folder) t)
    (make-directory (expand-file-name "food" rela-folder) t)
    (setq mh-user-path (file-name-as-directory mail-dir))
    (make-directory (expand-file-name "bar" abso-folder) t)
    (make-directory (expand-file-name "foo" abso-folder) t)
    (make-directory (expand-file-name "food" abso-folder) t)
    (setq mh-test-abs-folder abso-folder)
    (fset 'call-process #'mh-test-utils-log-call-process)
    (fset 'file-directory-p #'mh-test-utils-log-file-directory-p)
    temp-home-dir))

(defun mh-test-utils-log-call-process (program
                                       &optional infile destination display
                                       &rest args)
  "A wrapper around `call-process' that can log the program args and output.
Both args and output are written with `message' if `mh-test-utils-debug-mocks'
is non-nil."
  (let (process-output)
    (when mh-test-utils-debug-mocks
      (message "call-process arglist %s" (cons program args)))
    (with-temp-buffer
      (apply mh-test-call-process-real program infile destination display args)
      (setq process-output (buffer-string)))
    (when mh-test-utils-debug-mocks
      (message " -> response:\n%s" process-output))
    (insert process-output)))

(defun mh-test-utils-log-file-directory-p (filename)
  "A wrapper around `file-directory-p' that can log calls.
Both FILENAME and the return value are written with `message'
if `mh-test-utils-debug-mocks' is non-nil."
  (let ((result (funcall mh-test-file-directory-p-real filename)))
    (when mh-test-utils-debug-mocks
      (message "file-directory-p: %S -> %s" filename result))
    result))

(defun mh-test-variant-handles-plus-slash (variant)
  "Returns non-nil if this MH variant handles \"folders +/\".
Mailutils 3.5, 3.7, and 3.13 are known not to."
  (cond ((not (stringp variant)))       ;our mock handles it
        ((string-search "GNU Mailutils" variant)
         (let ((mu-version (string-remove-prefix "GNU Mailutils " variant)))
           (version<= "3.13.91" mu-version)))
        (t)))                           ;no other known failures


(ert-deftest mh-sub-folders-actual ()
  "Test `mh-sub-folders-actual'."
  ;; Note that mh-sub-folders-actual expects the folder to have
  ;; already been normalized with
  ;; (mh-normalize-folder-name folder nil nil t)
  (with-mh-test-env
    (should (member
             mh-test-rel-folder
             (mapcar (lambda (x) (car x)) (mh-sub-folders-actual nil))))
    ;; Empty string and "+" not tested since mh-normalize-folder-name
    ;; would change them to nil.
    (should (member "foo"
                    (mapcar (lambda (x) (car x))
                            (mh-sub-folders-actual
                             (format "+%s" mh-test-rel-folder)))))
    ;; Folder with trailing slash not tested since
    ;; mh-normalize-folder-name would strip it.
    (should (equal
             nil
             (mh-sub-folders-actual (format "+%s/foo" mh-test-rel-folder))))

    (should (equal
             (list (list "bar") (list "foo") (list "food"))
             (mh-sub-folders-actual (format "+%s" mh-test-abs-folder))))

    (when (mh-test-variant-handles-plus-slash mh-variant-in-use)
      (should (member "tmp" (mapcar (lambda (x) (car x))
                                    (mh-sub-folders-actual "+/")))))

    ;; FIXME: mh-sub-folders-actual doesn't (yet) expect to be given a
    ;; nonexistent folder.
    ;;   (should (equal nil
    ;;                  (mh-sub-folders-actual "+nosuchfolder")))
    ;;   (should (equal nil
    ;;                  (mh-sub-folders-actual "+/nosuchfolder")))
    ))

(ert-deftest mh-sub-folders ()
  "Test `mh-sub-folders'."
  (with-mh-test-env
    (should (member mh-test-rel-folder
                   (mapcar (lambda (x) (car x)) (mh-sub-folders nil))))
    (should (member mh-test-rel-folder
                    (mapcar (lambda (x) (car x)) (mh-sub-folders ""))))
    (should-not (member mh-test-no-such-folder
                        (mapcar (lambda (x) (car x)) (mh-sub-folders "+"))))
    (should (equal (list (list "bar") (list "foo") (list "food"))
                   (mh-sub-folders (format "+%s" mh-test-rel-folder))))
    (should (equal (list (list "bar") (list "foo") (list "food"))
                   (mh-sub-folders (format "+%s/" mh-test-rel-folder))))
    (should (equal nil
                   (mh-sub-folders (format "+%s/foo/" mh-test-rel-folder))))
    (should (equal nil
                   (mh-sub-folders (format "+%s/foo" mh-test-rel-folder))))
    (should (equal (list (list "bar") (list "foo") (list "food"))
                   (mh-sub-folders (format "+%s" mh-test-abs-folder))))
    (when (mh-test-variant-handles-plus-slash mh-variant-in-use)
      (should (member "tmp"
                      (mapcar (lambda (x) (car x)) (mh-sub-folders "+/")))))

    ;; FIXME: mh-sub-folders doesn't (yet) expect to be given a
    ;; nonexistent folder.
    ;;   (should (equal nil
    ;;                  (mh-sub-folders "+nosuchfolder")))
    ;;   (should (equal nil
    ;;                  (mh-sub-folders "+/nosuchfolder")))
    ))


(defmacro mh-test-folder-completion-1 (name
                                       nil-expected t-expected lambda-expected)
  "Helper for testing `mh-folder-completion-function'.
Ask for completion on NAME three times, with three different
values for the FLAG argument of `mh-folder-completion-function'.
NIL-EXPECTED is the expected value with FLAG nil.
T-EXPECTED is the expected value with FLAG t.
LAMBDA-EXPECTED is the expected value with FLAG lambda."
  (declare (debug t))
  `(with-mh-test-env
     (mh-test-folder-completion-2 ,nil-expected ;case "a"
                                  (mh-folder-completion-function ,name nil nil))
     (mh-test-folder-completion-2 ,t-expected ;case "b"
                                  (mh-folder-completion-function ,name nil t))
     (mh-test-folder-completion-2 ,lambda-expected ;case "c"
                                  (mh-folder-completion-function ,name nil
                                                                 'lambda))))

(defmacro mh-test-folder-completion-2 (expected actual)
  "Inner helper for testing `mh-folder-completion-function'.
ACTUAL should evaluate to either EXPECTED or to a list containing EXPECTED.
ACTUAL may be evaluated twice, but this gives a clearer error on failure,
and the `should' macro requires idempotent evaluation anyway."
  (declare (debug t))
  `(if (and (not (consp ,expected)) (consp ,actual))
       (should (member ,expected ,actual))
     (should (equal ,expected ,actual))))


(ert-deftest mh-folder-completion-function-02-empty ()
  "Test `mh-folder-completion-function' with empty name."
  (mh-test-folder-completion-1 "" "+" (format "%s/" mh-test-rel-folder) nil))

(ert-deftest mh-folder-completion-function-03-plus ()
  "Test `mh-folder-completion-function' with `+'."
  (mh-test-folder-completion-1 "+" "+" (format "%s/" mh-test-rel-folder) nil))

(ert-deftest mh-folder-completion-function-04-rel-folder ()
  "Test `mh-folder-completion-function' with `+rela-folder'."
  (mh-test-folder-completion-1 (format "+%s" mh-test-rel-folder)
                               (format "+%s/" mh-test-rel-folder)
                               (list (format "%s/" mh-test-rel-folder))
                               t))

(ert-deftest mh-folder-completion-function-05-rel-folder-slash ()
  "Test `mh-folder-completion-function' with `+rela-folder/'."
  (mh-test-folder-completion-1 (format "+%s/" mh-test-rel-folder)
                               (format "+%s/" mh-test-rel-folder)
                               (list "bar" "foo" "food")
                               t))

(ert-deftest mh-folder-completion-function-06-rel-folder-slash-foo ()
  "Test `mh-folder-completion-function' with `+rela-folder/foo'."
  (mh-test-folder-completion-1 (format "+%s/foo" mh-test-rel-folder)
                               (format "+%s/foo" mh-test-rel-folder)
                               (list "foo" "food")
                               t)
  (with-mh-test-env
    (should (equal nil
                   (mh-folder-completion-function
                    (format "+%s/fo" mh-test-rel-folder) nil 'lambda)))))

(ert-deftest mh-folder-completion-function-07-rel-folder-slash-foo-slash ()
  "Test `mh-folder-completion-function' with `+rela-folder/foo/'."
  (mh-test-folder-completion-1 (format "+%s/foo/" mh-test-rel-folder)
                               nil
                               nil
                               t))

(ert-deftest mh-folder-completion-function-08-plus-slash ()
  "Test `mh-folder-completion-function' with `+/'."
  (with-mh-test-env
    (skip-unless (mh-test-variant-handles-plus-slash mh-variant-in-use)))
  (mh-test-folder-completion-1 "+/" "+/" "tmp/" t)
  ;; case "bb"
  (with-mh-test-env
    (should (equal nil
                   (member (format "+%s/" mh-test-rel-folder)
                           (mh-folder-completion-function "+/" nil t))))))

(ert-deftest mh-folder-completion-function-09-plus-slash-tmp ()
  "Test `mh-folder-completion-function' with `+/tmp'."
  (with-mh-test-env
    (skip-unless (mh-test-variant-handles-plus-slash mh-variant-in-use)))
  (mh-test-folder-completion-1 "+/tmp" "+/tmp/" "tmp/" t))

(ert-deftest mh-folder-completion-function-10-plus-slash-abs-folder ()
  "Test `mh-folder-completion-function' with `+/abso-folder'."
  (mh-test-folder-completion-1 (format "+%s/" mh-test-abs-folder)
                               (format "+%s/" mh-test-abs-folder)
                               (list "bar" "foo" "food")
                               t))

(ert-deftest mh-folder-completion-function-11-plus-slash-abs-folder-slash-foo ()
  "Test `mh-folder-completion-function' with `+/abso-folder/foo'."
  (mh-test-folder-completion-1 (format "+%s/foo" mh-test-abs-folder)
                               (format "+%s/foo" mh-test-abs-folder)
                               (list "foo" "food")
                               t)
  (with-mh-test-env
    (should (equal nil
                   (mh-folder-completion-function
                    (format "+%s/fo" mh-test-abs-folder) nil 'lambda)))))

(ert-deftest mh-folder-completion-function-12-plus-nosuchfolder ()
  "Test `mh-folder-completion-function' with `+nosuchfolder'."
  (mh-test-folder-completion-1 "+nosuchfolder" nil nil nil))

(ert-deftest mh-folder-completion-function-13-plus-slash-nosuchfolder ()
  "Test `mh-folder-completion-function' with `+/nosuchfolder'."
  (mh-test-folder-completion-1 "+/nosuchfolder" nil nil nil))

;;; mh-utils-tests.el ends here
