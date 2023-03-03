;;; em-cmpl-tests.el --- em-cmpl test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

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

;; Tests for Eshell's interactive completion.

;;; Code:

(require 'ert)
(require 'eshell)
(require 'em-cmpl)
(require 'em-dirs)
(require 'em-hist)
(require 'em-tramp)
(require 'em-unix)

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))

(defvar eshell-test-value nil)

(defun eshell-insert-and-complete (input)
  "Insert INPUT and invoke completion, returning the result."
  (insert input)
  (completion-at-point)
  (eshell-get-old-input))

(defun eshell-arguments-equal (actual expected)
  "Return t if ACTUAL and EXPECTED are equal, including properties of strings.
ACTUAL and EXPECTED should both be lists of strings."
  (when (length= actual (length expected))
    (catch 'not-equal
      (cl-mapc (lambda (i j)
                 (unless (equal-including-properties i j)
                   (throw 'not-equal nil)))
               actual expected)
      t)))

(defun eshell-arguments-equal--equal-explainer (actual expected)
  "Explain the result of `eshell-arguments-equal'."
  `(nonequal-result
    (actual ,actual)
    (expected ,expected)))

(put 'eshell-arguments-equal 'ert-explainer
     #'eshell-arguments-equal--equal-explainer)

;;; Tests:

(ert-deftest em-cmpl-test/parse-arguments/pipeline ()
  "Test that parsing arguments for completion discards earlier commands."
  (with-temp-eshell
   (let ((eshell-test-value '("foo" "bar")))
     (insert "echo hi | cat")
     (should (eshell-arguments-equal
              (car (eshell-complete-parse-arguments))
              '("cat"))))))

(ert-deftest em-cmpl-test/parse-arguments/multiple-dots ()
  "Test parsing arguments with multiple dots like \".../\"."
  (with-temp-eshell
   (insert "echo .../file.txt")
   (should (eshell-arguments-equal
            (car (eshell-complete-parse-arguments))
            `("echo" ,(propertize "../../file.txt"
                                  'pcomplete-arg-value
                                  ".../file.txt"))))))

(ert-deftest em-cmpl-test/parse-arguments/variable/numeric ()
  "Test parsing arguments with a numeric variable interpolation."
  (with-temp-eshell
   (let ((eshell-test-value 42))
     (insert "echo $eshell-test-value")
     (should (eshell-arguments-equal
              (car (eshell-complete-parse-arguments))
              `("echo" ,(propertize "42" 'pcomplete-arg-value 42)))))))

(ert-deftest em-cmpl-test/parse-arguments/variable/nil ()
  "Test parsing arguments with a nil variable interpolation."
  (with-temp-eshell
   (let ((eshell-test-value nil))
     (insert "echo $eshell-test-value")
     (should (eshell-arguments-equal
              (car (eshell-complete-parse-arguments))
              `("echo" ,(propertize "" 'pcomplete-arg-value nil)))))))

(ert-deftest em-cmpl-test/parse-arguments/variable/list ()
  "Test parsing arguments with a list variable interpolation."
  (with-temp-eshell
   (let ((eshell-test-value '("foo" "bar")))
     (insert "echo $eshell-test-value")
     (should (eshell-arguments-equal
              (car (eshell-complete-parse-arguments))
              `("echo" ,(propertize "(\"foo\" \"bar\")"
                                    'pcomplete-arg-value
                                    eshell-test-value)))))))

(ert-deftest em-cmpl-test/parse-arguments/variable/splice ()
  "Test parsing arguments with a spliced variable interpolation."
  (with-temp-eshell
   (let ((eshell-test-value '("foo" "bar")))
     (insert "echo $@eshell-test-value")
     (should (eshell-arguments-equal
              (car (eshell-complete-parse-arguments))
              '("echo" "foo" "bar"))))))

(ert-deftest em-cmpl-test/file-completion/unique ()
  "Test completion of file names when there's a unique result."
  (with-temp-eshell
   (ert-with-temp-directory default-directory
     (write-region nil nil (expand-file-name "file.txt"))
     (should (equal (eshell-insert-and-complete "echo fi")
                    "echo file.txt ")))))

(ert-deftest em-cmpl-test/file-completion/non-unique ()
  "Test completion of file names when there are multiple results."
  (with-temp-eshell
   (ert-with-temp-directory default-directory
     (write-region nil nil (expand-file-name "file.txt"))
     (write-region nil nil (expand-file-name "file.el"))
     (should (equal (eshell-insert-and-complete "echo fi")
                    "echo file."))
     ;; Now try completing again.
     (let ((minibuffer-message-timeout 0)
           (inhibit-message t))
       (completion-at-point))
     ;; FIXME: We can't use `current-message' here.
     (with-current-buffer (messages-buffer)
       (save-excursion
         (goto-char (point-max))
         (forward-line -1)
         (should (looking-at "Complete, but not unique")))))))

(ert-deftest em-cmpl-test/file-completion/after-list ()
  "Test completion of file names after previous list arguments.
See bug#59956."
  (with-temp-eshell
   (ert-with-temp-directory default-directory
     (write-region nil nil (expand-file-name "file.txt"))
     (should (equal (eshell-insert-and-complete "echo (list 1 2) fi")
                    "echo (list 1 2) file.txt ")))))

(ert-deftest em-cmpl-test/lisp-symbol-completion ()
  "Test completion of Lisp forms like \"#'symbol\" and \"`symbol\".
See <lisp/eshell/esh-cmd.el>."
  (with-temp-eshell
   (should (equal (eshell-insert-and-complete "echo #'system-nam")
                  "echo #'system-name ")))
  (with-temp-eshell
   (should (equal (eshell-insert-and-complete "echo `system-nam")
                  "echo `system-name "))))

(ert-deftest em-cmpl-test/lisp-function-completion ()
  "Test completion of Lisp forms like \"(func)\".
See <lisp/eshell/esh-cmd.el>."
  (with-temp-eshell
   (should (equal (eshell-insert-and-complete "echo (eshell/ech")
                  "echo (eshell/echo"))))

(ert-deftest em-cmpl-test/variable-ref-completion ()
  "Test completion of variable references like \"$var\".
See <lisp/eshell/esh-var.el>."
  (with-temp-eshell
   (should (equal (eshell-insert-and-complete "echo $system-nam")
                  "echo $system-name "))))

(ert-deftest em-cmpl-test/quoted-variable-ref-completion ()
  "Test completion of variable references like \"$'var'\".
See <lisp/eshell/esh-var.el>."
  (with-temp-eshell
   (should (equal (eshell-insert-and-complete "echo $'system-nam")
                  "echo $'system-name' ")))
  (with-temp-eshell
   (should (equal (eshell-insert-and-complete "echo $\"system-nam")
                  "echo $\"system-name\" "))))

(ert-deftest em-cmpl-test/variable-ref-completion/directory ()
  "Test completion of variable references that expand to directories.
See <lisp/eshell/esh-var.el>."
  (with-temp-eshell
   (should (equal (eshell-insert-and-complete "echo $PW")
                  "echo $PWD/")))
  (with-temp-eshell
   (let ((minibuffer-message-timeout 0)
         (inhibit-message t))
     (should (equal (eshell-insert-and-complete "echo $PWD")
                    "echo $PWD/"))))
  (with-temp-eshell
   (should (equal (eshell-insert-and-complete "echo $'PW")
                  "echo $'PWD'/"))))

(ert-deftest em-cmpl-test/variable-assign-completion ()
  "Test completion of variable assignments like \"var=value\".
See <lisp/eshell/esh-var.el>."
  (with-temp-eshell
   (ert-with-temp-directory default-directory
     (write-region nil nil (expand-file-name "file.txt"))
     (should (equal (eshell-insert-and-complete "VAR=f")
                    "VAR=file.txt ")))))

(ert-deftest em-cmpl-test/variable-assign-completion/non-assignment ()
  "Test completion of things that look like variable assignment, but aren't.
For example, the second argument in \"tar --directory=dir\" looks
like it could be a variable assignment, but it's not.  We should
let `pcomplete-tar' handle it instead.

See <lisp/eshell/esh-var.el>."
  (with-temp-eshell
   (ert-with-temp-directory default-directory
     (write-region nil nil (expand-file-name "file.txt"))
     (make-directory "dir")
     (should (equal (eshell-insert-and-complete "tar --directory=")
                    "tar --directory=dir/")))))

(ert-deftest em-cmpl-test/user-ref-completion ()
  "Test completion of user references like \"~user\".
See <lisp/eshell/em-dirs.el>."
  (unwind-protect
      (with-temp-eshell
       (cl-letf (((symbol-function 'eshell-read-user-names)
                  (lambda () (setq eshell-user-names '((1234 . "user"))))))
         (should (equal (eshell-insert-and-complete "echo ~us")
                        "echo ~user/"))))
    ;; Clear the cached user names we set above.
    (setq eshell-user-names nil)))

;;; em-cmpl-tests.el ends here
