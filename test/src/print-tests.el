;;; print-tests.el --- tests for src/print.c         -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2024 Free Software Foundation, Inc.

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

;; Support sharing test code with cl-print-tests.

(defalias 'print-tests--prin1-to-string #'identity
  "The function to print to a string which is under test.")

(defmacro print-tests--deftest (name arg &rest docstring-keys-and-body)
  "Test both print.c and cl-print.el at once."
  (declare (debug ert-deftest)
           (doc-string 3)
           (indent 2))
  (let ((clname (intern (concat (symbol-name name) "-cl-print")))
        (doc (when (stringp (car-safe docstring-keys-and-body))
               (list (pop docstring-keys-and-body))))
        (keys-and-values nil))
    (while (keywordp (car-safe docstring-keys-and-body))
      (let ((key (pop docstring-keys-and-body))
            (val (pop docstring-keys-and-body)))
        (push val keys-and-values)
        (push key keys-and-values)))
    `(progn
       ;; Set print-tests--prin1-to-string at both declaration and
       ;; runtime, so that it can be used by the :expected-result
       ;; keyword.
       (cl-letf (((symbol-function #'print-tests--prin1-to-string)
                  #'prin1-to-string))
         (ert-deftest ,name ,arg
           ,@doc
           ,@keys-and-values
           (cl-letf (((symbol-function #'print-tests--prin1-to-string)
                      #'prin1-to-string))
             ,@docstring-keys-and-body)))
       (cl-letf (((symbol-function #'print-tests--prin1-to-string)
                  #'cl-prin1-to-string))
         (ert-deftest ,clname ,arg
           ,@doc
           ,@keys-and-values
           (cl-letf (((symbol-function #'print-tests--prin1-to-string)
                      #'cl-prin1-to-string))
             ,@docstring-keys-and-body))))))

(print-tests--deftest print-hex-backslash ()
  (should (string= (let ((print-escape-multibyte t)
                         (print-escape-newlines t))
                     (print-tests--prin1-to-string "\u00A2\ff"))
                   "\"\\x00a2\\ff\"")))

(defun print-tests--prints-with-charset-p (ch odd-charset)
  "Return t if print function being tested prints CH with the `charset' property.
CH is propertized with a `charset' value according to
ODD-CHARSET: if nil, then use the one returned by `char-charset',
otherwise, use a different charset."
  (integerp
   (string-match
    "charset"
    (print-tests--prin1-to-string
     (propertize (string ch)
                 'charset
                 (if odd-charset
                     (cl-find (char-charset ch) charset-list :test-not #'eq)
                   (char-charset ch)))))))

(print-tests--deftest print-charset-text-property-nil ()
  :expected-result (if (eq (symbol-function #'print-tests--prin1-to-string)
                           #'cl-prin1-to-string) :failed :passed)
  (let ((print-charset-text-property nil))
    (should-not (print-tests--prints-with-charset-p ?\xf6 t)) ; Bug#31376.
    (should-not (print-tests--prints-with-charset-p ?a t))
    (should-not (print-tests--prints-with-charset-p ?\xf6 nil))
    (should-not (print-tests--prints-with-charset-p ?a nil))))

(print-tests--deftest print-charset-text-property-default ()
  :expected-result (if (eq (symbol-function #'print-tests--prin1-to-string)
                           #'cl-prin1-to-string) :failed :passed)
  (let ((print-charset-text-property 'default))
    (should (print-tests--prints-with-charset-p ?\xf6 t))
    (should-not (print-tests--prints-with-charset-p ?a t))
    (should-not (print-tests--prints-with-charset-p ?\xf6 nil))
    (should-not (print-tests--prints-with-charset-p ?a nil))))

(print-tests--deftest print-charset-text-property-t ()
  (let ((print-charset-text-property t))
    (should (print-tests--prints-with-charset-p ?\xf6 t))
    (should (print-tests--prints-with-charset-p ?a t))
    (should (print-tests--prints-with-charset-p ?\xf6 nil))
    (should (print-tests--prints-with-charset-p ?a nil))))

(ert-deftest terpri ()
  (should (string= (with-output-to-string
                     (princ 'abc)
                     (should (terpri nil t)))
                   "abc\n"))
  (should (string= (with-output-to-string
                     (should-not (terpri nil t))
                     (princ 'xyz))
                   "xyz"))
  (message nil)
  (if noninteractive
      (progn (should            (terpri nil t))
             (should-not        (terpri nil t))
             (princ 'abc)
             (should            (terpri nil t))
             (should-not        (terpri nil t)))
    (should (string= (progn (should-not (terpri nil t))
                            (princ 'abc)
                            (should (terpri nil t))
                            (current-message))
                     "abc\n")))
  (let ((standard-output
         (with-current-buffer (get-buffer-create "*terpri-test*")
           (insert "--------")
           (point-max-marker))))
    (should     (terpri nil t))
    (should-not (terpri nil t))
    (should (string= (with-current-buffer (marker-buffer standard-output)
                       (buffer-string))
                     "--------\n"))))

(print-tests--deftest print-read-roundtrip ()
  (let ((syms (list '## '& '* '+ '- '/ '0E '0e '< '= '> 'E 'E0 'NaN '\"
                    '\# '\#x0 '\' '\'\' '\( '\) '\+00 '\, '\-0 '\. '\.0
                    '\0 '\0.0 '\0E0 '\0e0 '\1E+ '\1E+NaN '\1e+ '\1e+NaN
                    '\; '\? '\[ '\\ '\] '\` '_ 'a 'e 'e0 'x
                    '{ '| '} '~ : '\’ '\’bar
                    (intern "\t") (intern "\n") (intern " ")
                    (intern "\N{NO-BREAK SPACE}")
                    (intern "\N{ZERO WIDTH SPACE}")
                    (intern "\0"))))
    (dolist (sym syms)
      (should (eq (read (print-tests--prin1-to-string sym)) sym))
      (dolist (sym1 syms)
        (let ((sym2 (intern (concat (symbol-name sym) (symbol-name sym1)))))
          (should (eq (read (print-tests--prin1-to-string sym2)) sym2)))))))

(print-tests--deftest print-bignum ()
  (let* ((str "999999999999999999999999999999999")
         (val (read str)))
    (should (> val most-positive-fixnum))
    (should (equal (print-tests--prin1-to-string val) str))))

(print-tests--deftest print-tests-print-gensym ()
  "Printing observes `print-gensym'."
  (let* ((sym1 (gensym))
         (syms (list sym1 (gensym "x") (make-symbol "y") sym1)))
    (let* ((print-circle nil)
           (printed-with (let ((print-gensym t))
                           (print-tests--prin1-to-string syms)))
           (printed-without (let ((print-gensym nil))
                              (print-tests--prin1-to-string syms))))
      (should (string-match
               "(#:\\(g[[:digit:]]+\\) #:x[[:digit:]]+ #:y #:\\(g[[:digit:]]+\\))$"
               printed-with))
      (should (string= (match-string 1 printed-with)
                       (match-string 2 printed-with)))
      (should (string-match "(g[[:digit:]]+ x[[:digit:]]+ y g[[:digit:]]+)$"
                            printed-without)))
    (let* ((print-circle t)
           (printed-with (let ((print-gensym t))
                           (print-tests--prin1-to-string syms)))
           (printed-without (let ((print-gensym nil))
                              (print-tests--prin1-to-string syms))))
      (should (string-match "(#1=#:g[[:digit:]]+ #:x[[:digit:]]+ #:y #1#)$"
                            printed-with))
      (should (string-match "(g[[:digit:]]+ x[[:digit:]]+ y g[[:digit:]]+)$"
                            printed-without)))))

(print-tests--deftest print-tests-continuous-numbering ()
  "Printing observes `print-continuous-numbering'."
  ;; cl-print does not support print-continuous-numbering.
  :expected-result (if (eq (symbol-function #'print-tests--prin1-to-string)
                           #'cl-prin1-to-string)
                       :failed :passed)
  (let* ((x (list 1))
         (y "hello")
         (g (gensym))
         (g2 (gensym))
         (print-circle t)
         (print-gensym t))
    (let ((print-continuous-numbering t)
          (print-number-table nil))
      (should (string-match
               "(#1=(1) #1# #2=\"hello\" #2#)(#3=#:g[[:digit:]]+ #3#)(#1# #2# #3#)#2#$"
               (mapconcat #'print-tests--prin1-to-string
                          `((,x ,x ,y ,y) (,g ,g) (,x ,y ,g) ,y)))))

    ;; This is the special case for byte-compile-output-docform
    ;; mentioned in a comment in print_preprocess.  When
    ;; print-continuous-numbering and print-circle and print-gensym
    ;; are all non-nil, print all gensyms with numbers even if they
    ;; only occur once.
    (let ((print-continuous-numbering t)
          (print-number-table nil))
      (should (string-match
               "(#1=#:g[[:digit:]]+ #2=#:g[[:digit:]]+)$"
               (print-tests--prin1-to-string (list g g2)))))))

(cl-defstruct print--test a b)

(print-tests--deftest print-tests-1 ()
  "Test print code."
  (let ((x (make-print--test :a 1 :b 2))
        (rec (cond
              ((eq (symbol-function #'print-tests--prin1-to-string) 'prin1-to-string)
               "#s(print--test 1 2)")
              ((eq (symbol-function #'print-tests--prin1-to-string) 'cl-prin1-to-string)
               "#s(print--test :a 1 :b 2)")
              (t (cl-assert nil)))))

    (let ((print-circle nil))
      (should (equal (print-tests--prin1-to-string `((x . ,x) (y . ,x)))
                     (format "((x . %s) (y . %s))" rec rec))))
    (let ((print-circle t))
      (should (equal (print-tests--prin1-to-string `((x . ,x) (y . ,x)))
                     (format "((x . #1=%s) (y . #1#))" rec))))))

(print-tests--deftest print-tests-2 ()
  (let ((x (record 'foo 1 2 3)))
    (should (equal
             x
             (car (read-from-string (with-output-to-string (prin1 x))))))
    (let ((print-circle t))
      (should (string-match
               "\\`(#1=#s(foo 1 2 3) #1#)\\'"
               (print-tests--prin1-to-string (list x x)))))))

(cl-defstruct (print-tests-struct
               (:constructor print-tests-con))
  a b c d e)

(print-tests--deftest print-tests-3 ()
  "Printing observes `print-length'."
  (let ((long-list (make-list 5 'a))
        (long-vec (make-vector 5 'b))
        ;; (long-struct (print-tests-con))
        ;; (long-string (make-string 5 ?a))
        (print-length 4))
    (should (equal "(a a a a ...)" (print-tests--prin1-to-string long-list)))
    (should (equal "[b b b b ...]" (print-tests--prin1-to-string long-vec)))
    ;; This one only prints 3 nils. Should it print 4?
    ;; (should (equal "#s(print-tests-struct nil nil nil nil ...)"
    ;;                (print-tests--prin1-to-string long-struct)))
    ;; This one is only supported by cl-print
    ;; (should (equal "\"aaaa...\"" (cl-print-tests--prin1-to-string long-string)))
    ))

(print-tests--deftest print-tests-4 ()
  "Printing observes `print-level'."
  (let* ((deep-list '(a (b (c (d (e))))))
         (buried-vector '(a (b (c (d [e])))))
         (deep-struct (print-tests-con))
         (buried-struct `(a (b (c (d ,deep-struct)))))
         (buried-string '(a (b (c (d #("hello" 0 5 (print-test t)))))))
         (buried-simple-string '(a (b (c (d "hello")))))
         (print-level 4))
    (setf (print-tests-struct-a deep-struct) deep-list)
    (should (equal "(a (b (c (d ...))))" (print-tests--prin1-to-string deep-list)))
    (should (equal "(a (b (c (d \"hello\"))))"
                   (print-tests--prin1-to-string buried-simple-string)))
    (cond
     ((eq (symbol-function #'print-tests--prin1-to-string) #'prin1-to-string)
      (should (equal "(a (b (c (d [e]))))" (print-tests--prin1-to-string buried-vector)))
      (should (equal "(a (b (c (d #s(print-tests-struct ... nil nil nil nil)))))"
                     (print-tests--prin1-to-string buried-struct)))
      (should (equal "(a (b (c (d #(\"hello\" 0 5 ...)))))"
                     (print-tests--prin1-to-string buried-string)))
      (should (equal "#s(print-tests-struct (a (b (c ...))) nil nil nil nil)"
                     (print-tests--prin1-to-string deep-struct))))

     ((eq (symbol-function #'print-tests--prin1-to-string) #'cl-prin1-to-string)
      (should (equal "(a (b (c (d ...))))" (print-tests--prin1-to-string buried-vector)))
      (should (equal "(a (b (c (d ...))))" (print-tests--prin1-to-string buried-struct)))
      (should (equal "(a (b (c (d ...))))" (print-tests--prin1-to-string buried-string)))
      (should (equal "#s(print-tests-struct :a (a (b (c ...))) :b nil :c nil :d nil :e nil)"
                     (print-tests--prin1-to-string deep-struct))))
     (t (cl-assert nil)))))

(print-tests--deftest print-tests-5 ()
  "Printing observes `print-quoted'."
  (let ((quoted-stuff '('a #'b `(,c ,@d))))
    (let ((print-quoted t))
      (should (equal "('a #'b `(,c ,@d))"
                     (print-tests--prin1-to-string quoted-stuff))))
    (let ((print-quoted nil))
      (should (equal "((quote a) (function b) (\\` ((\\, c) (\\,@ d))))"
                     (print-tests--prin1-to-string quoted-stuff))))))

(print-tests--deftest print-tests-strings ()
  "Can print strings and propertized strings."
  (let* ((str1 "abcdefghij")
         (str2 #("abcdefghij" 3 6 (bold t) 7 9 (italic t)))
         (str3 #("abcdefghij" 0 10 (test t)))
         (obj '(a b))
         ;; Since the byte compiler reuses string literals,
         ;; and the put-text-property call is destructive, use
         ;; copy-sequence to make a new string.
         (str4 (copy-sequence "abcdefghij")))
    (put-text-property 0 5 'test obj str4)
    (put-text-property 7 10 'test obj str4)

    (should (equal "\"abcdefghij\"" (print-tests--prin1-to-string str1)))
    (should (equal "#(\"abcdefghij\" 3 6 (bold t) 7 9 (italic t))"
                   (print-tests--prin1-to-string str2)))
    (should (equal "#(\"abcdefghij\" 0 10 (test t))"
                   (print-tests--prin1-to-string str3)))
    (let ((print-circle nil))
      (should
       (equal
        "#(\"abcdefghij\" 0 5 (test (a b)) 7 10 (test (a b)))"
        (print-tests--prin1-to-string str4))))
    (let ((print-circle t))
      (should
       (equal
        "#(\"abcdefghij\" 0 5 (test #1=(a b)) 7 10 (test #1#))"
        (print-tests--prin1-to-string str4))))))

(print-tests--deftest print-circle ()
  (let ((x '(#1=(a . #1#) #1#)))
    (let ((print-circle nil))
      (should (string-match "\\`((a . #[0-9]) (a . #[0-9]))\\'"
                            (print-tests--prin1-to-string x))))
    (let ((print-circle t))
      (should (equal "(#1=(a . #1#) #1#)" (print-tests--prin1-to-string x))))))

(print-tests--deftest print-circle-2 ()
   ;; Bug#31146.
  (let ((x '(0 . #1=(0 . #1#))))
    (let ((print-circle nil))
      (should (string-match "\\`(0\\( 0\\)* . #[0-9]+)\\'"
                            (print-tests--prin1-to-string x))))
    (let ((print-circle t))
      (should (equal "(0 . #1=(0 . #1#))" (print-tests--prin1-to-string x))))))

(print-tests--deftest error-message-string-circular ()
  (let ((err (list 'error)))
    (setcdr err err)
    (should-error (error-message-string err) :type 'circular-list)))

(print-tests--deftest print-hash-table-test ()
  (should
   (string-match
    "data (2 3)"
    (let ((h (make-hash-table)))
      (puthash 1 2 h)
      (puthash 2 3 h)
      (remhash 1 h)
      (format "%S" h))))

  (should
   (string-match
    "data ()"
    (let ((h (make-hash-table)))
      (let ((print-length 0))
        (format "%S" h)))))

  (should
   (string-match
    "data (99 99)"
    (let ((h (make-hash-table)))
      (dotimes (i 100)
        (puthash i i h))
      (dotimes (i 99)
        (remhash i h))
      (let ((print-length 1))
        (format "%S" h))))))

(print-tests--deftest print-integers-as-characters ()
  ;; Bug#44155.
  (let* ((print-integers-as-characters t)
         (chars '(?? ?\; ?\( ?\) ?\{ ?\} ?\[ ?\] ?\" ?\' ?\\ ?f ?~ ?Á 32
                  ?\n ?\r ?\t ?\b ?\f ?\a ?\v ?\e ?\d))
         (nums '(-1 -65 0 1 31 #x80 #x9f #x110000 #x3fff80 #x3fffff))
         (nonprints '(#xd800 #xdfff #x030a #xffff #x2002 #x200c))
         (printed-chars (print-tests--prin1-to-string chars))
         (printed-nums (print-tests--prin1-to-string nums))
         (printed-nonprints (print-tests--prin1-to-string nonprints)))
    (should (equal (read printed-chars) chars))
    (should (equal
             printed-chars
             (concat
              "(?? ?\\; ?\\( ?\\) ?\\{ ?\\} ?\\[ ?\\] ?\\\" ?\\' ?\\\\"
              " ?f ?~ ?Á ?\\s ?\\n ?\\r ?\\t ?\\b ?\\f 7 11 27 127)")))
    (should (equal (read printed-nums) nums))
    (should (equal printed-nums
                   "(-1 -65 0 1 31 128 159 1114112 4194176 4194303)"))
    (should (equal (read printed-nonprints) nonprints))
    (should (equal printed-nonprints
                   "(55296 57343 778 65535 8194 8204)"))))

(ert-deftest test-unreadable ()
  (should (equal (prin1-to-string (make-marker)) "#<marker in no buffer>"))
  (let ((print-unreadable-function
         (lambda (_object _escape)
           "hello")))
    (should (equal (prin1-to-string (make-marker)) "hello")))
  (let ((print-unreadable-function
         (lambda (_object _escape)
           t)))
    (should (equal (prin1-to-string (make-marker)) ""))))

(ert-deftest test-dots ()
  (should (equal (prin1-to-string 'foo.bar) "foo.bar"))
  (should (equal (prin1-to-string '.foo) "\\.foo"))
  (should (equal (prin1-to-string '.foo.) "\\.foo."))
  (should (equal (prin1-to-string 'bar?bar) "bar?bar"))
  (should (equal (prin1-to-string '\?bar) "\\?bar"))
  (should (equal (prin1-to-string '\?bar?) "\\?bar?")))

(ert-deftest test-prin1-overrides ()
  (with-temp-buffer
    (let ((print-length 10))
      (prin1 (make-list 20 t) (current-buffer) t)
      (should (= print-length 10)))
    (goto-char (point-min))
    (should (= (length (read (current-buffer))) 20)))

  (with-temp-buffer
    (let ((print-length 10))
      (prin1 (make-list 20 t) (current-buffer) '((length . 5)))
      (should (= print-length 10)))
    (goto-char (point-min))
    (should (= (length (read (current-buffer))) 6)))

  (with-temp-buffer
    (let ((print-length 10))
      (prin1 (make-list 20 t) (current-buffer) '(t (length . 5)))
      (should (= print-length 10)))
    (goto-char (point-min))
    (should (= (length (read (current-buffer))) 6))))

(ert-deftest test-prin1-to-string-overrides ()
  (let ((print-length 10))
    (should
     (= (length (car (read-from-string
                      (prin1-to-string (make-list 20 t) nil t))))
        20)))

  (let ((print-length 10))
    (should
     (= (length (car (read-from-string
                      (prin1-to-string (make-list 20 t) nil
                                       '((length . 5))))))
        6)))

  (should-error (prin1-to-string 'foo nil 'a))
  (should-error (prin1-to-string 'foo nil '(a)))
  (should-error (prin1-to-string 'foo nil '(t . b)))
  (should-error (prin1-to-string 'foo nil '(t b)))
  (should-error (prin1-to-string 'foo nil '((a . b) b)))
  (should-error (prin1-to-string 'foo nil '((length . 10) . b))))

(ert-deftest print-deeply-nested ()
  ;; Check that we can print a deeply nested data structure correctly.
  (let ((print-circle t))
    (let ((levels 10000)
          (x 'a)
          (prefix nil)
          (suffix nil))
      (dotimes (_ levels)
        (setq x (list (vector (record 'r x))))
        (push "([#s(r " prefix)
        (push ")])" suffix))
      (let ((expected (concat (apply #'concat prefix)
                              "a"
                              (apply #'concat suffix))))
        (should (equal (prin1-to-string x) expected))))))

(defun print-test-rho (lead loop)
  "A circular iota list with LEAD elements followed by LOOP in circle."
 (let ((l (number-sequence 1 (+ lead loop))))
   (setcdr (nthcdr (+ lead loop -1) l) (nthcdr lead l))
   l))

(ert-deftest print-circular ()
  ;; Check printing of rho-shaped circular lists such as (1 2 3 4 5 4 5 4 . #6)
  ;; when `print-circle' is nil.  The exact output may differ since the number
  ;; of elements printed of the looping part can vary depending on when the
  ;; circularity was detected.
  (dotimes (lead 7)
    (ert-info ((prin1-to-string lead) :prefix "lead: ")
      (dolist (loop (number-sequence 1 7))
        (ert-info ((prin1-to-string loop) :prefix "loop: ")
          (let* ((rho (print-test-rho lead loop))
                 (print-circle nil)
                 (str (prin1-to-string rho)))
            (should (string-match (rx "("
                                      (group (+ (+ digit) " "))
                                      ". #" (group (+ digit)) ")")
                                  str))
            (let* ((g1 (match-string 1 str))
                   (g2 (match-string 2 str))
                   (numbers (mapcar #'string-to-number (split-string g1)))
                   (loopback-index (string-to-number g2)))
              ;; Split the numbers in the lead and loop part.
              (should (< lead (length numbers)))
              (should (<= lead loopback-index))
              (should (< loopback-index (length numbers)))
              (let ((lead-part (take lead numbers))
                    (loop-part (nthcdr lead numbers)))
                ;; The lead part must match exactly.
                (should (equal lead-part (number-sequence 1 lead)))
                ;; The loop part is at least LOOP long: make sure it matches.
                (should (>= (length loop-part) loop))
                (let ((expected-loop-part
                       (mapcar (lambda (x) (+ lead 1 (% x loop)))
                               (number-sequence 0 (1- (length loop-part))))))
                  (should (equal loop-part expected-loop-part))
                  ;; The loopback index must match the length of the
                  ;; loop part.
                  (should (equal (% (- (length numbers) loopback-index) loop)
                                 0)))))))))))

(ert-deftest test-print-unreadable-function-buffer ()
  (let* ((buffer nil)
         (callback-buffer nil)
         (str (with-temp-buffer
                (setq buffer (current-buffer))
                (let ((print-unreadable-function
                       (lambda (_object _escape)
                         (setq callback-buffer (current-buffer))
                         "tata")))
                  (prin1-to-string (make-marker))))))
      (should (eq callback-buffer buffer))
      (should (equal str "tata"))))

(provide 'print-tests)
;;; print-tests.el ends here
