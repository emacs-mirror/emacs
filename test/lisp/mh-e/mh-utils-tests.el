;;; mh-utils-tests.el --- tests for mh-utils.el -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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
(eval-when-compile (require 'cl-lib))
(require 'mh-utils)

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


;; Folder names that are used by the following tests.
(defvar mh-test-rel-folder "rela-folder")
(defvar mh-test-abs-folder "/abso-folder")
(defvar mh-test-no-such-folder "/testdir/none"
  "Name of a folder that the user does not have.")

(defvar mh-test-variant-logged-already nil
  "Whether `with-mh-test-env' has written the MH variant to the log.")
(setq mh-test-variant-logged-already nil) ;reset if buffer is re-evaluated

(defvar mh-test-utils-debug-mocks nil
  "Whether to log detailed behavior of mock functions.")

(defvar mh-test-call-process-real (symbol-function 'call-process))
(defvar mh-test-file-directory-p-real (symbol-function 'file-directory-p))


;;; This macro wraps tests that touch the file system and/or run programs.
;;; When running such tests, MH-E can use a particular MH variant
;;; installed on the system, or it can use the mocks provided below.

;;; By setting PATH and mh-sys-path, you can select which of the
;;; installed MH variants to use or ignore them all and use mocks.

(defmacro with-mh-test-env (&rest body)
  "Evaluate BODY with a test mail environment.
Functions that touch the file system or run MH programs are either
mocked out or pointed at a test tree.  When called from Emacs's batch
testing infrastructure, this will use mocks and thus run on systems
that do not have any MH variant installed.  MH-E developers can
install an MH variant and test it interactively."
  (declare (indent defun))
  `(cl-letf ((temp-home-dir nil)
             ;; make local bindings for things we will modify for test env
             (mh-user-path)
             (mh-test-abs-folder)
             ((symbol-function 'call-process))
             ((symbol-function 'file-directory-p))
             ;; the test always gets its own sub-folders cache
             (mh-sub-folders-cache (make-hash-table :test #'equal))
             ;; remember the original value
             (original-mh-envvar (getenv "MH")))
     (unless mh-test-variant-logged-already
       (mh-variant-set mh-variant)
       (setq mh-test-variant-logged-already t))
     (unwind-protect
         (progn
           (if mh-variant-in-use
               (setq temp-home-dir (mh-test-utils-setup-with-variant))
             (mh-test-utils-setup-with-mocks))
           ,@body)
       (if temp-home-dir (delete-directory temp-home-dir t))
       (setenv "MH" original-mh-envvar))))

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
  (fset 'file-directory-p #'mh-test-utils-mock-file-directory-p))

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
          (make-temp-file "emacs-mh-e-unit-test" t))
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


(ert-deftest mh-sub-folders-actual ()
  "Test `mh-sub-folders-actual'."
  ;; Note that mh-sub-folders-actual expects the folder to have
  ;; already been normalized with
  ;; (mh-normalize-folder-name folder nil nil t)
  (with-mh-test-env
    (should (equal
             mh-test-rel-folder
             (car (assoc mh-test-rel-folder (mh-sub-folders-actual nil)))))
    ;; Empty string and "+" not tested since mh-normalize-folder-name
    ;; would change them to nil.
    (should (equal "foo"
                   (car (assoc "foo" (mh-sub-folders-actual
                                      (format "+%s" mh-test-rel-folder))))))
    ;; Folder with trailing slash not tested since
    ;; mh-normalize-folder-name would strip it.
    (should (equal
             nil
             (mh-sub-folders-actual (format "+%s/foo" mh-test-rel-folder))))

    (should (equal
             (list (list "bar") (list "foo") (list "food"))
             (mh-sub-folders-actual (format "+%s" mh-test-abs-folder))))

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
    (should (equal mh-test-rel-folder
                   (car (assoc mh-test-rel-folder (mh-sub-folders nil)))))
    (should (equal mh-test-rel-folder
                   (car (assoc mh-test-rel-folder (mh-sub-folders "")))))
    (should (equal nil
                   (car (assoc mh-test-no-such-folder (mh-sub-folders
                                                       "+")))))
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
  :expected-result :failed              ;to be fixed in a patch by mkupfer
  (mh-test-folder-completion-1 "+/" "+/" "tmp/" nil)
    ;; case "bb"
    (with-mh-test-env
      (should (equal nil
                     (member (format "+%s/" mh-test-rel-folder)
                             (mh-folder-completion-function "+/" nil t))))))

(ert-deftest mh-folder-completion-function-09-plus-slash-tmp ()
  "Test `mh-folder-completion-function' with `+/tmp'."
  :expected-result :failed              ;to be fixed in a patch by mkupfer
  (mh-test-folder-completion-1 "+/tmp" "+/tmp" "tmp/" t))

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
