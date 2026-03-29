;;; syntax-tests.el --- tests for syntax.c functions -*- lexical-binding: t -*-

;; Copyright (C) 2017-2026 Free Software Foundation, Inc.

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
(require 'cl-lib)

(ert-deftest parse-partial-sexp-continue-over-comment-marker ()
  "Continue a parse that stopped in the middle of a comment marker."
  (with-temp-buffer
    (let ((table (make-syntax-table)))
      (modify-syntax-entry ?/ ". 124")
      (modify-syntax-entry ?* ". 23b")
      (set-syntax-table table))
    (insert "/*C*/\nX")
    (goto-char (point-min))
    (let* ((pointC (progn (search-forward "C") (1- (point))))
           (preC (1- pointC))
           (pointX (progn (search-forward "X") (1- (point))))
           (aftC (+ 2 pointC))
           (ppsC (parse-partial-sexp (point-min) pointC))
           (pps-preC (parse-partial-sexp (point-min) preC))
           (pps-aftC (parse-partial-sexp (point-min) aftC))
           (ppsX (parse-partial-sexp (point-min) pointX)))
      ;; C should be inside comment.
      (should (= (nth 0 ppsC) 0))
      (should (eq (nth 4 ppsC) t))
      (should (= (nth 8 ppsC) (- pointC 2)))
      ;; X should not be in comment or list.
      (should (= (nth 0 ppsX) 0))
      (should-not (nth 4 ppsX))
      ;; Try using OLDSTATE.
      (should (equal (parse-partial-sexp preC pointC nil nil pps-preC)
                     ppsC))
      (should (equal (parse-partial-sexp pointC aftC nil nil ppsC)
                     pps-aftC))
      (should (equal (parse-partial-sexp preC aftC nil nil pps-preC)
                     pps-aftC))
      (should (equal (parse-partial-sexp aftC pointX nil nil pps-aftC)
                     ppsX)))))

(ert-deftest syntax-class-character-test ()
  (cl-loop for char across " .w_()'\"$\\/<>@!|"
           for i from 0
           do (should (= char (syntax-class-to-char i)))
           when (string-to-syntax (string char))
           do (should (= char (syntax-class-to-char
                               (car (string-to-syntax (string char)))))))
  (should-error (syntax-class-to-char -1))
  (should-error (syntax-class-to-char 200)))

(ert-deftest parse-partial-sexp-paren-comments ()
  "Test syntax parsing with paren comment markers.
Specifically, where the first character of the comment marker is
also has open paren syntax (see Bug#24870)."
  (with-temp-buffer
    (let ((table (make-syntax-table)))
      (modify-syntax-entry ?\{  "(}1nb" table)
      (modify-syntax-entry ?\}  "){4nb" table)
      (modify-syntax-entry ?-  ". 123" table)
      (set-syntax-table table))
    (insert "{-C-}\nX")
    (goto-char (point-min))
    (let* ((pointC (progn (search-forward "C") (1- (point))))
           (pointX (progn (search-forward "X") (1- (point))))
           (ppsC (parse-partial-sexp (point-min) pointC))
           (ppsX (parse-partial-sexp (point-min) pointX)))
      ;; C should be inside nestable comment, not list.
      (should (= (nth 0 ppsC) 0))
      (should (= (nth 4 ppsC) 1))
      (should (= (nth 8 ppsC) (- pointC 2)))
      ;; X should not be in comment or list.
      (should (= (nth 0 ppsX) 0))
      (should-not (nth 4 ppsX))
      ;; Try using OLDSTATE.
      (should (equal (parse-partial-sexp pointC pointX nil nil ppsC)
                     ppsX)))))


;;; Commentary:
;; The next bit tests the handling of comments in syntax.c, in
;; particular the functions `forward-comment' and `scan-lists' and
;; `parse-partial-sexp' (in so far as they relate to comments).

;; It is intended to enhance this bit to test nested comments
;; (2020-10-01).

;; This bit uses the data file syntax-resources/syntax-comments.txt.

(defun syntax-comments-point (n forw)
  "Return the buffer offset corresponding to the \"label\" N.
N is a decimal number which appears in the data file, usually
twice, as \"labels\".  It can also be a negative number or zero.
FORW is t when we're using the label at BOL, nil for the one at EOL.

If the label N doesn't exist in the current buffer, an exception
is thrown.

When FORW is t and N positive, we return the position after the
first occurrence of label N at BOL in the data file.  With FORW
nil, we return the position before the last occurrence of the
label at EOL in the data file.

When N is negative, we return instead the position of the end of
line that the -N label is on.  When it is zero, we return POINT."
  (if (zerop n)
      (point)
    (let ((str (format "%d" (abs n))))
      (save-excursion
	(if forw
	    (progn
	      (goto-char (point-min))
	      (re-search-forward
	       (concat "^\\(" str "\\)\\([^0-9\n]\\|$\\)"))
	      (if (< n 0)
		  (progn (end-of-line) (point))
		(match-end 1)))
	  (goto-char (point-max))
	  (re-search-backward
	   (concat "\\(^\\|[^0-9]\\)\\(" str "\\)$"))
	  (if (< n 0)
	      (progn (end-of-line) (point))
	    (match-beginning 2)))))))

(defun syntax-comments-midpoint (n)
  "Return the buffer offset corresponding to the \"label\" N.
N is a positive decimal number which should appear in the buffer
exactly once.  The label need not be at the beginning or end of a
line.

The return value is the position just before the label.

If the label N doesn't exist in the current buffer, an exception
is thrown."
  (let ((str (format "%d" n)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward
       (concat "\\(^\\|[^0-9]\\)\\(" str "\\)\\([^0-9\n]\\|$\\)"))
      (match-beginning 2))))

(eval-and-compile
  (defvar syntax-comments-section))

(defmacro syntax-comments (-type- -dir- res start &optional stop)
  "Create an ERT test to test (forward-comment 1/-1).
The test uses a fixed name data file, which it visits.  It calls
entry and exit functions to set up and tear down syntax entries
for comment characters.  The test is given a name based on the
global variable `syntax-comments-section', the direction of
movement and the value of START.

-TYPE- (unquoted) is a symbol from whose name the entry and exit
function names are derived by appending \"-in\" and \"-out\".

-DIR- (unquoted) is `forward' or `backward', the direction
`forward-comment' is attempted.

RES, t or nil, is the expected result from `forward-comment'.

START and STOP are decimal numbers corresponding to labels in the
data file marking the start and expected stop positions.  See
`syntax-comments-point' for a precise specification.  If STOP is
missing or nil, the value of START is assumed for it."
  (declare (debug t))
  (let ((forw
	 (cond
	  ((eq -dir- 'forward) t)
	  ((eq -dir- 'backward) nil)
	  (t (error "Invalid -dir- argument \"%s\" to `syntax-comments'" -dir-))))
	(start-str (format "%d" (abs start)))
	(type -type-))
    `(ert-deftest ,(intern (concat "syntax-comments-"
				   syntax-comments-section
				   (if forw "-f" "-b") start-str))
	 ()
       (with-current-buffer
	   (find-file
            ,(ert-resource-file "syntax-comments.txt"))
	 (,(intern (concat (symbol-name type) "-in")))
	 (goto-char (syntax-comments-point ,start ,forw))
	 (let ((stop (syntax-comments-point ,(or stop start) ,(not forw))))
	   (should (eq (forward-comment ,(if forw 1 -1)) ,res))
	   (should (eq (point) stop)))
	 (,(intern (concat (symbol-name type) "-out")))))))

(defmacro syntax-br-comments (-type- -dir- res -start- &optional stop)
  "Create an ERT test to test (scan-lists <position> 1/-1 0).
This is to test the interface between scan-lists and the internal
comment routines in syntax.c.

The test uses a fixed name data file, which it visits.  It calls
entry and exit functions to set up and tear down syntax entries
for comment and paren characters.  The test is given a name based
on the global variable `syntax-comments-section', the direction
of movement and the value of -START-.

-TYPE- (unquoted) is a symbol from whose name the entry and exit
function names are derived by appending \"-in\" and \"-out\".

-DIR- (unquoted) is `forward' or `backward', the direction
`scan-lists' is attempted.

RES is t if `scan-lists' is expected to return, nil if it is
expected to raise a `scan-error' exception.

-START- and STOP are decimal numbers corresponding to labels in the
data file marking the start and expected stop positions.  See
`syntax-comments-point' for a precise specification.  If STOP is
missing or nil, the value of -START- is assumed for it."
  (declare (debug t))
  (let* ((forw
	  (cond
	   ((eq -dir- 'forward) t)
	   ((eq -dir- 'backward) nil)
	   (t (error "Invalid -dir- argument \"%s\" to `syntax-br-comments'" -dir-))))
         (start -start-)
	 (start-str (format "%d" (abs start)))
	 (type -type-))
    `(ert-deftest ,(intern (concat "syntax-br-comments-"
				   syntax-comments-section
				   (if forw "-f" "-b") start-str))
	 ()
       (with-current-buffer
	   (find-file
            ,(ert-resource-file "syntax-comments.txt"))
	 (,(intern (concat (symbol-name type) "-in")))
         (let ((start-pos (syntax-comments-point ,start ,forw))
               ,@(if res
                     `((stop-pos (syntax-comments-point
                                  ,(or stop start) ,(not forw))))))
           ,(if res
                `(should
                  (eq (scan-lists start-pos ,(if forw 1 -1) 0)
                      stop-pos))
              `(should-error (scan-lists start-pos ,(if forw 1 -1) 0)
                             :type 'scan-error)))
	 (,(intern (concat (symbol-name type) "-out")))))))

(defmacro syntax-pps-comments (-type- -start- open close &optional -stop-)
  "Create an ERT test to test `parse-partial-sexp' with comments.
This is to test the interface between `parse-partial-sexp' and
the internal comment routines in syntax.c.

The test uses a fixed name data file, which it visits.  It calls
entry and exit functions to set up and tear down syntax entries
for comment and paren characters.  The test is given a name based
on the global variable `syntax-comments-section', and the value
of -START-.

The generated test calls `parse-partial-sexp' three times, the
first two with COMMENTSTOP set to `syntax-table' so as to stop
after the start and end of the comment.  The third call is
expected to stop at the brace/paren matching the one where the
test started.

-TYPE- (unquoted) is a symbol from whose name the entry and exit
function names are derived by appending \"-in\" and \"-out\".

-START- and -STOP- are decimal numbers corresponding to labels in
the data file marking the start and expected stop positions.  See
`syntax-comments-point' for a precise specification.  If -STOP-
is missing or nil, the value of -START- is assumed for it.

OPEN and CLOSE are decimal numbers corresponding to labels in the
data file marking just after the comment opener and closer where
the `parse-partial-sexp's are expected to stop.  See
`syntax-comments-midpoint' for a precise specification."
  (declare (debug t))
  (let* ((type -type-)
         (start -start-)
         (start-str (format "%d" start))
         (stop (or -stop- start)))
    `(ert-deftest ,(intern (concat "syntax-pps-comments-"
                                   syntax-comments-section
                                   "-" start-str))
         ()
       (with-current-buffer
           (find-file
            ,(ert-resource-file "syntax-comments.txt"))
         (,(intern (concat (symbol-name type) "-in")))
         (let ((start-pos (syntax-comments-point ,start t))
               (open-pos (syntax-comments-midpoint ,open))
               (close-pos (syntax-comments-midpoint ,close))
               (stop-pos (syntax-comments-point ,stop nil))
               s)
           (setq s (parse-partial-sexp
                    start-pos (point-max) 0 nil nil 'syntax-table))
           (should (eq (point) open-pos))
           (setq s (parse-partial-sexp
                    (point) (point-max) 0 nil s 'syntax-table))
           (should (eq (point) close-pos))
           (setq s (parse-partial-sexp (point) (point-max) 0 nil s))
           (should (eq (point) stop-pos)))
         (,(intern (concat (symbol-name type) "-out")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Pascal" style comments - single character delimiters, the closing
;; delimiter not being newline.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun {-in ()
  (setq parse-sexp-ignore-comments t)
  (setq comment-end-can-be-escaped nil)
  (modify-syntax-entry ?{ "<")
  (modify-syntax-entry ?} ">"))
(defun {-out ()
  (modify-syntax-entry ?{ "(}")
  (modify-syntax-entry ?} "){"))
(eval-and-compile
  (setq syntax-comments-section "pascal"))

(syntax-comments { forward nil 20 0)
(syntax-comments { backward nil 20 0)
(syntax-comments { forward t 21)
(syntax-comments { backward t 21)
(syntax-comments { forward t 22)
(syntax-comments { backward t 22)

(syntax-comments { forward t 23)
(syntax-comments { backward t 23)
(syntax-comments { forward t 24)
(syntax-comments { backward t 24)
(syntax-comments { forward t 26)
(syntax-comments { backward t 26)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Lisp" style comments - single character opening delimiters on line
;; comments.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun \;-in ()
  (setq parse-sexp-ignore-comments t)
  (setq comment-end-can-be-escaped nil)
  (modify-syntax-entry ?\n ">")
  (modify-syntax-entry ?\; "<")
  (modify-syntax-entry ?{ ".")
  (modify-syntax-entry ?} "."))
(defun \;-out ()
  (modify-syntax-entry ?\n " ")
  (modify-syntax-entry ?\; ".")
  (modify-syntax-entry ?{ "(}")
  (modify-syntax-entry ?} "){"))
(eval-and-compile
  (setq syntax-comments-section "lisp"))

(syntax-comments \; backward nil 30 30)
(syntax-comments \; forward t 31)
(syntax-comments \; backward t 31)
(syntax-comments \; forward t 32)
(syntax-comments \; backward t 32)
(syntax-comments \; forward t 33)
(syntax-comments \; backward t 33)

;; "Lisp" style comments inside lists.
(syntax-br-comments \; backward nil 40)
(syntax-br-comments \; forward t 41)
(syntax-br-comments \; backward t 41)
(syntax-br-comments \; forward t 42)
(syntax-br-comments \; backward t 42)
(syntax-br-comments \; forward nil 43)

;; "Lisp" style comments parsed by `parse-partial-sexp'.
(syntax-pps-comments \; 41 90 91)
(syntax-pps-comments \; 42 92 93)
(syntax-pps-comments \; 43 94 95 -999)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Lisp" style nested comments: between delimiters #|  |#.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun \#|-in ()
  (setq parse-sexp-ignore-comments t)
  (modify-syntax-entry ?# ". 14")
  (modify-syntax-entry ?| ". 23n")
  (modify-syntax-entry ?\; "< b")
  (modify-syntax-entry ?\n "> b"))
(defun \#|-out ()
  (modify-syntax-entry ?# ".")
  (modify-syntax-entry ?| ".")
  (modify-syntax-entry ?\; ".")
  (modify-syntax-entry ?\n " "))
(eval-and-compile
  (setq syntax-comments-section "lisp-n"))

(syntax-comments \#| forward nil 100 0)
(syntax-comments \#| backward nil 100 0)
(syntax-comments \#| forward nil 101 -999)
(syntax-comments \#| forward t 102)
(syntax-comments \#| backward t 102)

(syntax-comments \#| forward t 103)
(syntax-comments \#| backward t 103)
(syntax-comments \#| forward t 104)
(syntax-comments \#| backward t 104)

(syntax-comments \#| forward nil 105 -999)
(syntax-comments \#| backward t 105)
(syntax-comments \#| forward t 106)
(syntax-comments \#| backward t 106)
(syntax-comments \#| forward t 107)
(syntax-comments \#| backward t 107)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mixed "Lisp" style (nested and unnested) comments.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(syntax-comments \#| forward t 110)
(syntax-comments \#| backward t 110)
(syntax-comments \#| forward t 111)
(syntax-comments \#| backward t 111)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs 27 "C" style comments - `comment-end-can-be-escaped' is non-nil.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun /*-in ()
  (setq parse-sexp-ignore-comments t)
  (setq comment-end-can-be-escaped t)
  (modify-syntax-entry ?/ ". 124b")
  (modify-syntax-entry ?* ". 23")
  (modify-syntax-entry ?\n "> b"))
(defun /*-out ()
  (setq comment-end-can-be-escaped nil)
  (modify-syntax-entry ?/ ".")
  (modify-syntax-entry ?* ".")
  (modify-syntax-entry ?\n " "))
(eval-and-compile
  (setq syntax-comments-section "c"))

(syntax-comments /* forward t 1)
(syntax-comments /* backward t 1)
(syntax-comments /* forward t 2)
(syntax-comments /* backward t 2)
(syntax-comments /* forward t 3)
(syntax-comments /* backward t 3)

(syntax-comments /* forward t 4)
(syntax-comments /* backward t 4)
(syntax-comments /* forward t 5 6)
(syntax-comments /* backward nil 5 0)
(syntax-comments /* forward nil 6 0)
(syntax-comments /* backward t 6 5)

(syntax-comments /* forward t 7 8)
(syntax-comments /* backward nil 7 0)
(syntax-comments /* forward nil 8 0)
(syntax-comments /* backward t 8 7)
(syntax-comments /* forward t 9)
(syntax-comments /* backward t 9)

(syntax-comments /* forward nil 10 0)
(syntax-comments /* backward nil 10 0)
(syntax-comments /* forward t 11)
(syntax-comments /* backward t 11)

(syntax-comments /* forward t 13 14)
(syntax-comments /* backward nil 13 -14)
(syntax-comments /* forward t 15)
(syntax-comments /* backward t 15)

;; Emacs 27 "C" style comments inside brace lists.
(syntax-br-comments /* forward t 50)
(syntax-br-comments /* backward t 50)
(syntax-br-comments /* forward t 51)
(syntax-br-comments /* backward t 51)
(syntax-br-comments /* forward t 52)
(syntax-br-comments /* backward t 52)

(syntax-br-comments /* forward t 53)
(syntax-br-comments /* backward t 53)
(syntax-br-comments /* forward t 54 20)
(syntax-br-comments /* backward t 54)
(syntax-br-comments /* forward t 55)
(syntax-br-comments /* backward t 55)

(syntax-br-comments /* forward t 56 58)
(syntax-br-comments /* backward t 58 56)
(syntax-br-comments /* backward nil 59)
(syntax-br-comments /* forward t 60)
(syntax-br-comments /* backward t 60)

;; Emacs 27 "C" style comments parsed by `parse-partial-sexp'.
(syntax-pps-comments /* 50 70 71)
(syntax-pps-comments /* 52 72 73)
(syntax-pps-comments /* 54 74 55 20)
(syntax-pps-comments /* 56 76 77 58)
(syntax-pps-comments /* 60 78 79)

(ert-deftest test-from-to-parse-partial-sexp ()
  (with-temp-buffer
    (insert "foo")
    (should (parse-partial-sexp 1 1))
    (should-error (parse-partial-sexp 2 1))))

(ert-deftest syntax-char-syntax ()
  ;; Verify that char-syntax behaves identically in interpreted and
  ;; byte-compiled code (bug#53260).
  (let ((cs (byte-compile (lambda (x) (char-syntax x)))))
    ;; Use a unibyte buffer with a syntax table using symbol syntax
    ;; for raw byte 128.
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((st (make-syntax-table)))
        (modify-syntax-entry (unibyte-char-to-multibyte 128) "_" st)
        (set-syntax-table st)
        (should (equal (eval '(char-syntax 128) t) ?_))
        (should (equal (funcall cs 128) ?_))))))

;;; syntax-tests.el ends here
