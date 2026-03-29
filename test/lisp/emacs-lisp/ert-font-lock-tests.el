;;; ert-font-lock-tests.el --- ERT Font Lock tests  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

;; Author: Vladimir Kazanov

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

;; This file is part of ERT Font Lock, an extension to the Emacs Lisp
;; Regression Test library (ERT) providing a convenient way to check
;; syntax highlighting provided by font-lock.
;;
;; See ert-font-lock.el for details, and below for example usage of
;; ert-font-lock facilities.

(require 'ert)
(require 'ert-x)
(require 'ert-font-lock)

;;; Helpers
;;

(defmacro with-temp-buffer-str-mode (mode str &rest body)
  "Create a buffer with STR contents and MODE. "
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (,mode)
     (insert ,str)
     (goto-char (point-min))
     ,@body))

(defun ert-font-lock--wrap-begin-end (re)
  (rx bol (regexp re) eol))

;;; Regexp tests
;;;

(ert-deftest test-regexp--face-symbol-re ()
  (let ((re (ert-font-lock--wrap-begin-end
             ert-font-lock--face-symbol-re)))
    (should (string-match-p re "font-lock-keyword-face"))
    (should (string-match-p re "-face"))
    (should (string-match-p re "weird-package/-face"))
    (should (string-match-p re "-"))
    (should (string-match-p re "font-lock.face"))
    (should-not (string-match-p re "face suffix-with"))
    (should-not (string-match-p re "("))))

(ert-deftest test-regexp--face-symbol-list-re ()
  (let ((re (ert-font-lock--wrap-begin-end
             ert-font-lock--face-symbol-list-re)))
    (should (string-match-p re "(face1 face2)"))
    (should (string-match-p re "(face1)"))
    (should (string-match-p re "()"))
    (should-not (string-match-p re ")"))
    (should-not (string-match-p re "("))))

(ert-deftest test-regexp--assertion-line-re ()
  (let ((re (ert-font-lock--wrap-begin-end
             ert-font-lock--assertion-line-re)))
    (should (string-match-p re "^   something-face"))
    (should (string-match-p re "^   !something-face"))
    (should (string-match-p re "^   (face1 face2)"))
    (should (string-match-p re "^   !(face1 face2)"))
    (should (string-match-p re "^   ()"))
    (should (string-match-p re "^   !()"))
    (should (string-match-p re "^   nil"))
    (should (string-match-p re "^   !nil"))
    (should (string-match-p re "<-   something-face"))
    (should (string-match-p re "<- ^  something-face"))
    (should (string-match-p re "^^ ^  something-face"))
    (should (string-match-p re "^     ^something-face"))
    (should-not (string-match-p re "^   <-  ^something-face"))))

;;; Comment parsing tests
;;

(ert-deftest test-line-comment-p--fundamental ()
  (with-temp-buffer-str-mode fundamental-mode
    "// comment\n"
    (should-not (ert-font-lock--line-comment-p))))

(ert-deftest test-line-comment-p--emacs-lisp ()
  (with-temp-buffer-str-mode emacs-lisp-mode
    "not comment
;; comment
"
    (should-not (ert-font-lock--line-comment-p))
    (forward-line)
    (should (ert-font-lock--line-comment-p))
    (forward-line)
    (should-not (ert-font-lock--line-comment-p))))

(ert-deftest test-line-comment-p--shell-script ()
  (with-temp-buffer-str-mode shell-script-mode
    "echo Not a comment
# comment
"
    (should-not (ert-font-lock--line-comment-p))
    (forward-line)
    (should (ert-font-lock--line-comment-p))))

(declare-function php-mode "php-mode")
(ert-deftest test-line-comment-p--php ()
  (skip-unless (fboundp 'php-mode))

  (with-temp-buffer-str-mode php-mode
    "echo 'Not a comment'
// comment
/* comment */
"
    (should-not (ert-font-lock--line-comment-p))
    (forward-line)
    (should (ert-font-lock--line-comment-p))
    (forward-line)
    (should (ert-font-lock--line-comment-p))))


(ert-deftest test-line-comment-p--javascript ()
  (with-temp-buffer-str-mode javascript-mode
    "// comment

   // comment, after a blank line

var abc = function(d) {};
"
    (should (ert-font-lock--line-comment-p))

    (forward-line)
    (should-not (ert-font-lock--line-comment-p))

    (forward-line)
    (should (ert-font-lock--line-comment-p))

    (forward-line)
    (should-not (ert-font-lock--line-comment-p))

    (forward-line)
    (should-not (ert-font-lock--line-comment-p))))

(ert-deftest test-line-comment-p--python ()

  (with-temp-buffer-str-mode python-mode
    "# comment

   # comment
print(\"Hello, world!\")"
    (should (ert-font-lock--line-comment-p))

    (forward-line)
    (should-not (ert-font-lock--line-comment-p))

    (forward-line)
    (should (ert-font-lock--line-comment-p))

    (forward-line)
    (should-not (ert-font-lock--line-comment-p))))

(ert-deftest test-line-comment-p--c ()

  (with-temp-buffer-str-mode c-mode
    "// comment
/* also comment */"
    (should (ert-font-lock--line-comment-p))

    (forward-line)
    (should (ert-font-lock--line-comment-p))))

(ert-deftest test-parse-comments--no-assertion-error ()
  (let* ((str "
not_an_assertion
random_symbol
"))
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (should-error (ert-font-lock--parse-comments) :type 'user-error))))

(ert-deftest test-parse-comments--single-line-error ()
  (let* ((str "// ^ face.face1"))
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (should-error (ert-font-lock--parse-comments) :type 'user-error))))

(ert-deftest test-parse-comments--single-line-single-caret ()
  (let* ((str "
first
// ^ face.face1
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 1))
      (should (equal (car asserts)
                     '(:line-checked 2 :line-assert 3 :column-checked 3 :face face.face1 :negation nil))))))

(ert-deftest test-parse-comments--single-line-many-carets ()
  (let* ((str "
multiplecarets
//^^^ ^^ ^ face.face1
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 6))
      (should (equal asserts
                     '((:line-checked 2 :line-assert 3 :column-checked 2 :face face.face1 :negation nil)
                       (:line-checked 2 :line-assert 3 :column-checked 3 :face face.face1 :negation nil)
                       (:line-checked 2 :line-assert 3 :column-checked 4 :face face.face1 :negation nil)
                       (:line-checked 2 :line-assert 3 :column-checked 6 :face face.face1 :negation nil)
                       (:line-checked 2 :line-assert 3 :column-checked 7 :face face.face1 :negation nil)
                       (:line-checked 2 :line-assert 3 :column-checked 9 :face face.face1 :negation nil)))))))

(ert-deftest test-parse-comments--face-list ()
  (let* ((str "
facelist
// ^ (face1 face2)
// ^ !(face3 face4)
// ^ (face5)
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 3))
      (should (equal asserts
                     '((:line-checked 2 :line-assert 3 :column-checked 3 :face (face1 face2) :negation nil)
                       (:line-checked 2 :line-assert 4 :column-checked 3 :face (face3 face4) :negation t)
                       (:line-checked 2 :line-assert 5 :column-checked 3 :face (face5) :negation nil)))))))

(ert-deftest test-parse-comments--caret-negation ()
  (let* ((str "
first
// ^ !face
// ^ face
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 2))
      (should (equal asserts
                     '((:line-checked 2 :line-assert 3 :column-checked 3 :face face :negation t)
                       (:line-checked 2 :line-assert 4 :column-checked 3 :face face :negation nil)))))))


(ert-deftest test-parse-comments--single-line-multiple-assert-lines ()
  (let* ((str "
first
// ^ face1
//     ^ face.face2
//     ^ face-face.face3
   //  ^ face_face.face4
")
         asserts)

    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 4))
      (should (equal asserts
                     '((:line-checked 2 :line-assert 3 :column-checked 3 :face face1 :negation nil)
                       (:line-checked 2 :line-assert 4 :column-checked 7 :face face.face2 :negation nil)
                       (:line-checked 2 :line-assert 5 :column-checked 7 :face face-face.face3 :negation nil)
                       (:line-checked 2 :line-assert 6 :column-checked 7 :face face_face.face4 :negation nil)))))))

(ert-deftest test-parse-comments--multiple-line-multiple-assert-lines ()
  (let* ((str "
first
// ^ face1
second
// ^ face2
//   ^ face3
third
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 3))
      (should (equal asserts
                     '((:line-checked 2  :line-assert 3 :column-checked 3 :face face1 :negation nil)
                       (:line-checked 4  :line-assert 5 :column-checked 3 :face face2 :negation nil)
                       (:line-checked 4  :line-assert 6 :column-checked 5 :face face3 :negation nil)))))))


(ert-deftest test-parse-comments--arrow-single-line-single ()
  (let* ((str "
first
// <- face1
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 1))
      (should (equal (car asserts)
                     '(:line-checked 2 :line-assert 3 :column-checked 0 :face face1 :negation nil))))))


(ert-deftest test-parse-comments-arrow-multiple-line-single ()
  (let* ((str "
first
// <- face1
  // <- face2
    // <- face3
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 3))
      (should (equal asserts
                     '((:line-checked 2 :line-assert 3 :column-checked 0 :face face1 :negation nil)
                       (:line-checked 2 :line-assert 4 :column-checked 2 :face face2 :negation nil)
                       (:line-checked 2 :line-assert 5 :column-checked 4 :face face3 :negation nil)))))))

(ert-deftest test-parse-comments--non-assert-comment-single ()
  (let* ((str "
// first
//  ^ comment-face
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 1))
      (should (equal (car asserts)
                     '(:line-checked 2 :line-assert 3 :column-checked 4 :face comment-face :negation nil))))))

(ert-deftest test-parse-comments--non-assert-comment-multiple ()
  (let* ((str "
// first second third
//  ^ comment-face
//        ^ comment-face
//                ^ comment-face
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 3))
      (should (equal asserts
                     '((:line-checked 2 :line-assert 3 :column-checked 4 :face comment-face :negation nil)
                       (:line-checked 2 :line-assert 4 :column-checked 10 :face comment-face :negation nil)
                       (:line-checked 2 :line-assert 5 :column-checked 18 :face comment-face :negation nil)))))))


(ert-deftest test-parse-comments--multiline-comment-single ()
  (let* ((str "
/*
  this is a comment
   ^ comment-face
 */
")
         asserts)
    (with-temp-buffer
      (insert str)
      (c-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 1))
      (should (equal (car asserts)
                     '(:line-checked 3 :line-assert 4 :column-checked 3 :face comment-face :negation nil))))))

(ert-deftest test-parse-comments--multiline-comment-multiple ()
  (let* ((str "
/*
  this is a comment
   ^ comment-face
  another comment
    ^ comment-face
 */
")
         asserts)
    (with-temp-buffer
      (insert str)
      (c-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 2))
      (should (equal asserts
                     '((:line-checked 3 :line-assert 4 :column-checked 3 :face comment-face :negation nil)
                       (:line-checked 5 :line-assert 6 :column-checked 4 :face comment-face :negation nil)))))))

;;; Syntax highlighting assertion tests
;;

(ert-deftest test-syntax-highlight-inline--nil-list ()
  (let ((str "
var abc = function(d) {
// ^ nil
//   ^ !nil
};

"))
    (with-temp-buffer
      (insert str)
      (javascript-mode)
      (font-lock-ensure)

      (ert-font-lock--check-faces
       (ert-font-lock--parse-comments)))))

(ert-deftest test-syntax-highlight-inline--face-list ()
  (let ((str "
var abc = function(d) {
//   ^ (test-face-2 test-face-1 font-lock-variable-name-face)
};

"))
    (with-temp-buffer
      (insert str)
      (javascript-mode)
      (font-lock-ensure)

      (add-face-text-property (point-min) (point-max) 'test-face-1)
      (add-face-text-property (point-min) (point-max) 'test-face-2)

      (ert-font-lock--check-faces
       (ert-font-lock--parse-comments)))))

(ert-deftest test-syntax-highlight-inline--caret-multiple-assertions ()
  (let ((str "
var abc = function(d) {
//   ^ font-lock-variable-name-face
    //        ^ font-lock-keyword-face
    //             ^ font-lock-variable-name-face
};

"))
    (with-temp-buffer
      (insert str)
      (javascript-mode)
      (font-lock-ensure)

      (ert-font-lock--check-faces
       (ert-font-lock--parse-comments)))))

(ert-deftest test-syntax-highlight-inline--caret-wrong-face ()
  (let* ((str "
var abc = function(d) {
//   ^ not-a-face
};
"))
    (with-temp-buffer
      (insert str)
      (javascript-mode)
      (font-lock-ensure)

      (should-error (ert-font-lock--check-faces
                     (ert-font-lock--parse-comments))))))

(ert-deftest test-syntax-highlight-inline--caret-negated-wrong-face ()
  (let* ((str "
var abc = function(d) {
//   ^ !not-a-face
};
"))
    (with-temp-buffer
      (insert str)
      (javascript-mode)
      (font-lock-ensure)

      (ert-font-lock--check-faces
       (ert-font-lock--parse-comments)))))

(ert-deftest test-syntax-highlight-inline--comment-face ()
  (let* ((str "
// this is a comment
//   ^ font-lock-comment-face
//       ^ font-lock-comment-face
//            ^ font-lock-comment-face
"))
    (with-temp-buffer
      (insert str)
      (javascript-mode)
      (font-lock-ensure)

      (ert-font-lock--check-faces
       (ert-font-lock--parse-comments)))))


(ert-deftest test-syntax-highlight-inline--multiline-comment-face ()
  (let* ((str "
/*
  this is a comment
   ^ font-lock-comment-face
  another comment
  more comments
    ^ font-lock-comment-face
 */
"))
    (with-temp-buffer
      (insert str)
      (c-mode)
      (font-lock-ensure)

      (ert-font-lock--check-faces
       (ert-font-lock--parse-comments)))))


(ert-deftest test-font-lock-test-string--correct ()
  (ert-font-lock-test-string
   "
var abc = function(d) {
// <- font-lock-keyword-face
//   ^ font-lock-variable-name-face
    //        ^ font-lock-keyword-face
    //             ^ font-lock-variable-name-face
};

"
   'javascript-mode))

(ert-deftest test-font-lock-test-file--correct ()
  (ert-font-lock-test-file
   (ert-resource-file "correct.js")
   'javascript-mode))

(ert-deftest test-font-lock-test-file--wrong ()
  :expected-result :failed
  (ert-font-lock-test-file
   (ert-resource-file "broken.js")
   'javascript-mode))

;;; Macro tests
;;

(ert-font-lock-deftest test-macro-test--correct-highlighting
  emacs-lisp-mode
  "
(defun fun ())
;; ^ font-lock-keyword-face
;;      ^ font-lock-function-name-face")

(ert-font-lock-deftest test-macro-test--docstring
  "A test with a docstring."
  emacs-lisp-mode
  "
(defun fun ())
;; ^ font-lock-keyword-face"
  )

(ert-font-lock-deftest test-macro-test--failing
  "A failing test."
  :expected-result :failed
  emacs-lisp-mode
  "
(defun fun ())
;; ^ wrong-face")

(ert-font-lock-deftest-file test-macro-test--file
  "Test reading correct assertions from a file."
  javascript-mode
  "correct.js")

(ert-font-lock-deftest-file test-macro-test--file-no-asserts
  "Check failing on files without assertions."
  :expected-result :failed
  javascript-mode
  "no-asserts.js")

(ert-font-lock-deftest-file test-macro-test--file-failing
  "Test reading wrong assertions from a file."
  :expected-result :failed
  javascript-mode
  "broken.js")

;;; ert-font-lock-tests.el ends here
