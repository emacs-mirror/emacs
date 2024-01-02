;;; em-extpipe-tests.el --- em-extpipe test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author: Sean Whitton <spwhitton@spwhitton.name>

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

(require 'cl-lib)
(require 'ert)
(require 'ert-x)
(require 'em-extpipe)
(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))

(defmacro em-extpipe-tests--deftest (name input &rest body)
  (declare (indent 2))
  `(ert-deftest ,name ()
     (cl-macrolet
         ((should-parse (expected)
            `(let ((shell-file-name "sh")
                   (shell-command-switch "-c"))
               ;; Strip `eshell-trap-errors'.
               (should (equal ,expected
                              (cadr (eshell-parse-command input))))))
          (with-substitute-for-temp (&rest body)
            ;; Substitute name of an actual temporary file and/or
            ;; buffer into `input'.  The substitution logic is
            ;; appropriate for only the use we put it to in this file.
            `(ert-with-temp-file temp
               (let ((temp-buffer (generate-new-buffer " *temp*" t)))
                 (unwind-protect
                     (let ((input
                            (replace-regexp-in-string
                             "temp\\([^>]\\|\\'\\)" temp
                             (string-replace "#<buffer temp>"
                                             (buffer-name temp-buffer)
                                             input))))
                       ,@body)
                   (when (buffer-name temp-buffer)
                     (kill-buffer temp-buffer))))))
          (temp-should-string= (expected)
            `(string= ,expected (string-trim-right
                                 (with-temp-buffer
                                   (insert-file-contents temp)
                                   (buffer-string)))))
          (temp-buffer-should-string= (expected)
            `(string= ,expected (string-trim-right
                                 (with-current-buffer temp-buffer
                                   (buffer-string))))))
       (skip-unless shell-file-name)
       (skip-unless shell-command-switch)
       (skip-unless (executable-find shell-file-name))
       (let ((input ,input))
         (with-temp-eshell ,@body)))))

(em-extpipe-tests--deftest em-extpipe-test-1
    "echo \"bar\" *| rev >temp"
  (skip-unless (executable-find "rev"))
  (should-parse '(eshell-named-command
                  "sh" (list "-c" "echo \"bar\" | rev >temp")))
  (with-substitute-for-temp
   (eshell-match-command-output input "^$")
   (temp-should-string= "rab")))

(em-extpipe-tests--deftest em-extpipe-test-2
    "echo \"bar\" | rev *>temp"
  (skip-unless (executable-find "rev"))
  (should-parse
   '(eshell-execute-pipeline
     '((eshell-named-command "echo" (list (eshell-escape-arg "bar")))
       (eshell-named-command "sh" (list "-c" "rev >temp")))))
  (with-substitute-for-temp
   (eshell-match-command-output input "^$")
   (temp-should-string= "rab")))

(em-extpipe-tests--deftest em-extpipe-test-3 "foo *| bar | baz -d"
  (should-parse
   '(eshell-execute-pipeline
     '((eshell-named-command "sh" (list "-c" "foo | bar"))
       (eshell-named-command "baz" (list "-d"))))))

(em-extpipe-tests--deftest em-extpipe-test-4
    "echo \"bar\" *| rev >#<buffer temp>"
  (skip-unless (executable-find "rev"))
  (should-parse
   '(progn
      (ignore
       (eshell-set-output-handle 1 'overwrite
				 (get-buffer-create "temp")))
      (eshell-named-command "sh"
			    (list "-c" "echo \"bar\" | rev"))))
  (with-substitute-for-temp
   (eshell-match-command-output input "^$")
   (temp-buffer-should-string= "rab")))

(em-extpipe-tests--deftest em-extpipe-test-5
    "foo *| bar >#<buffer quux> baz"
  (should-parse '(eshell-named-command
                  "sh" (list "-c" "foo | bar >#<buffer quux> baz"))))

(em-extpipe-tests--deftest em-extpipe-test-6
    "foo >#<buffer quux> *| bar baz"
  (should-parse '(eshell-named-command
                  "sh" (list "-c" "foo >#<buffer quux> | bar baz"))))

(em-extpipe-tests--deftest em-extpipe-test-7
    "foo *| bar >#<buffer quux> >>#<process other>"
  (should-parse
   '(progn
      (ignore
       (eshell-set-output-handle 1 'overwrite
				 (get-buffer-create "quux")))
      (ignore
       (eshell-set-output-handle 1 'append
				 (get-process "other")))
      (eshell-named-command "sh"
			    (list "-c" "foo | bar")))))

(em-extpipe-tests--deftest em-extpipe-test-8
    "foo *| bar >/dev/kill | baz"
  (should-parse
   '(eshell-execute-pipeline
     '((progn
	 (ignore
	  (eshell-set-output-handle 1 'overwrite "/dev/kill"))
	 (eshell-named-command "sh"
			       (list "-c" "foo | bar")))
       (eshell-named-command "baz")))))

(em-extpipe-tests--deftest em-extpipe-test-9 "foo \\*| bar"
  (should-parse
   '(eshell-execute-pipeline
     '((eshell-named-command "foo"
                             (list (eshell-escape-arg "*")))
       (eshell-named-command "bar")))))

(em-extpipe-tests--deftest em-extpipe-test-10 "foo \"*|\" *>bar"
  (should-parse
   '(eshell-named-command "sh" (list "-c" "foo \"*|\" >bar"))))

(em-extpipe-tests--deftest em-extpipe-test-11 "foo '*|' bar"
  (should-parse '(eshell-named-command
                  "foo" (list (eshell-escape-arg "*|") "bar"))))

(em-extpipe-tests--deftest em-extpipe-test-12 ">foo bar *| baz"
  (should-parse
   '(eshell-named-command "sh" (list "-c" ">foo bar | baz"))))

(em-extpipe-tests--deftest em-extpipe-test-13 "foo*|bar"
  (should-parse '(eshell-execute-pipeline
                  '((eshell-named-command (eshell-concat nil "foo" "*"))
                    (eshell-named-command "bar")))))

(em-extpipe-tests--deftest em-extpipe-test-14 "tac *<temp"
  (skip-unless (executable-find "tac"))
  (should-parse '(eshell-named-command "sh" (list "-c" "tac <temp")))
  (with-substitute-for-temp
   (with-temp-buffer (insert "bar\nbaz\n") (write-file temp))
   (eshell-match-command-output input "baz\nbar")))

(em-extpipe-tests--deftest em-extpipe-test-15 "echo \"bar\" *| cat"
  (skip-unless (executable-find "cat"))
  (should-parse
   '(eshell-named-command "sh" (list "-c" "echo \"bar\" | cat")))
  (cl-letf (((symbol-function 'eshell/cat)
             (lambda (&rest _args) (eshell-print "nonsense"))))
    (eshell-match-command-output input "bar")
    (eshell-match-command-output "echo \"bar\" | cat" "nonsense")))

(em-extpipe-tests--deftest em-extpipe-test-16 "echo \"bar\" *| rev"
  (skip-unless (executable-find "rev"))
  (should-parse
   '(eshell-named-command "sh" (list "-c" "echo \"bar\" | rev")))
  (let ((eshell-prefer-lisp-functions t))
    (cl-letf (((symbol-function 'rev)
               (lambda (&rest _args) (eshell-print "nonsense"))))
      (eshell-match-command-output input "rab")
      (eshell-match-command-output "echo \"bar\" | rev" "nonsense"))))

;; Confirm we don't break input of sharp-quoted symbols (Bug#53518).
(em-extpipe-tests--deftest em-extpipe-test-17 "funcall #'upcase foo"
  (eshell-match-command-output input "FOO"))

;;; em-extpipe-tests.el ends here
