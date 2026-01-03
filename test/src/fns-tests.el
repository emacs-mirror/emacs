;;; fns-tests.el --- tests for src/fns.c  -*- lexical-binding:t -*-

;; Copyright (C) 2014-2026 Free Software Foundation, Inc.

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

(ert-deftest fns-tests-identity ()
  (let ((num 12345)) (should (eq (identity num) num)))
  (let ((str "foo")) (should (eq (identity str) str)))
  (let ((lst '(11))) (should (eq (identity lst) lst))))

(ert-deftest fns-tests-random ()
  (unwind-protect
      (progn
        (should-error (random -1) :type 'args-out-of-range)
        (should-error (random 0) :type 'args-out-of-range)
        (should (integerp (random)))
        (should (= (random 1) 0))
        (should (>= (random 10) 0))
        (should (< (random 10) 10))
        ;; On OpenBSD random is always non-deterministic.
        (unless (and (eq system-type 'berkeley-unix)
                     (string-match-p "openbsd" system-configuration))
          (should (equal (random "seed") (random "seed"))))
        ;; The probability of four calls being the same is low.
        ;; This makes sure that the value isn't constant.
        (should (not (= (random t) (random t) (random t) (random t))))
        ;; Handle bignums.
        (should (integerp (random (1+ most-positive-fixnum)))))
    ;; Reset the PRNG seed after testing.
    (random t)))

(ert-deftest fns-tests-length ()
  (should (= (length nil) 0))
  (should (= (length '(1 2 3)) 3))
  (should (= (length '[1 2 3]) 3))
  (should (= (length "foo") 3))
  (should-error (length t))
  (should (= (length (make-char-table 'fns-tests)) (1+ (max-char)))))

(ert-deftest fns-tests-safe-length ()
  (should (= (safe-length '(1 2 3)) 3)))

(ert-deftest fns-tests-string-bytes ()
  (should (= (string-bytes "abc") 3)))

;; Test that equality predicates work correctly on NaNs when combined
;; with hash tables based on those predicates.  This was not the case
;; for eql in Emacs 26.
(ert-deftest fns-tests-equality-nan ()
  (dolist (test (list #'eq #'eql #'equal))
    (let* ((h (make-hash-table :test test))
           (nan 0.0e+NaN)
           (-nan (- nan)))
      (puthash nan t h)
      (should (eq (funcall test nan -nan) (gethash -nan h))))))

(ert-deftest fns-tests-equal-including-properties ()
  (should (equal-including-properties "" ""))
  (should (equal-including-properties "foo" "foo"))
  (should (equal-including-properties #("foo" 0 3 (a b))
                                      (propertize "foo" 'a 'b)))
  (should (equal-including-properties #("foo" 0 3 (a b c d))
                                      (propertize "foo" 'a 'b 'c 'd)))
  (should (equal-including-properties #("a" 0 1 (k v))
                                      #("a" 0 1 (k v))))
  (should-not (equal-including-properties #("a" 0 1 (k v))
                                          #("a" 0 1 (k x))))
  (should-not (equal-including-properties #("a" 0 1 (k v))
                                          #("b" 0 1 (k v))))
  (should-not (equal-including-properties #("foo" 0 3 (a b c e))
                                          (propertize "foo" 'a 'b 'c 'd))))

(ert-deftest fns-tests-equal-including-properties/string-prop-vals ()
  "Handle string property values.  (Bug#6581)"
  (should (equal-including-properties #("a" 0 1 (k "v"))
                                      #("a" 0 1 (k "v"))))
  (should (equal-including-properties #("foo" 0 3 (a (t)))
                                      (propertize "foo" 'a (list t))))
  (should-not (equal-including-properties #("a" 0 1 (k "v"))
                                          #("a" 0 1 (k "x"))))
  (should-not (equal-including-properties #("a" 0 1 (k "v"))
                                          #("b" 0 1 (k "v")))))

(ert-deftest fns-tests-equal-symbols-with-position ()
  "Test `eq' and `equal' on symbols with position."
  (let ((foo1 (position-symbol 'foo 42))
        (foo2 (position-symbol 'foo 666))
        (foo3 (position-symbol 'foo 42)))
    (let (symbols-with-pos-enabled)
      (should (eq foo1 foo1))
      (should (equal foo1 foo1))
      (should-not (eq foo1 foo2))
      (should-not (equal foo1 foo2))
      (should-not (eq foo1 foo3))
      (should (equal foo1 foo3)))
    (let ((symbols-with-pos-enabled t))
      (should (eq foo1 foo1))
      (should (equal foo1 foo1))
      (should (eq foo1 foo2))
      (should (equal foo1 foo2))
      (should (eq foo1 foo3))
      (should (equal foo1 foo3)))))

(ert-deftest fns-tests-reverse ()
  (should-error (reverse))
  (should-error (reverse 1))
  (should-error (reverse (make-char-table 'foo)))
  (should (equal [] (reverse [])))
  (should (equal [0] (reverse [0])))
  (should (equal [1 2 3 4] (reverse (reverse [1 2 3 4]))))
  (should (equal '(a b c d) (reverse (reverse '(a b c d)))))
  (should (equal "xyzzy" (reverse (reverse "xyzzy"))))
  (should (equal "こんにちは / ｺﾝﾆﾁﾊ" (reverse (reverse "こんにちは / ｺﾝﾆﾁﾊ")))))

(ert-deftest fns-tests-nreverse ()
  (should-error (nreverse))
  (should-error (nreverse 1))
  (should-error (nreverse (make-char-table 'foo)))
  (should (equal (nreverse (copy-sequence "xyzzy")) "yzzyx"))
  (let* ((A (vector))
         (B (nreverse A)))
    (should (equal A []))
    (should (eq B A)))
  (let* ((A (vector 0))
         (B (nreverse A)))
    (should (equal A [0]))
    (should (eq B A)))
  (let* ((A (vector 1 2 3 4))
         (B (nreverse A)))
    (should (equal A [4 3 2 1]))
    (should (eq B A)))
  (let* ((A (vector 1 2 3 4))
         (B (nreverse A))
         (C (nreverse A)))
    (should (equal A [1 2 3 4]))
    (should (eq B A))
    (should (eq C A))))

(ert-deftest fns-tests-reverse-bool-vector ()
  (let ((A (make-bool-vector 10 nil)))
    (dotimes (i 5) (aset A i t))
    (should (equal [nil nil nil nil nil t t t t t] (vconcat (reverse A))))
    (should (equal A (reverse (reverse A))))))

(ert-deftest fns-tests-nreverse-bool-vector ()
  (let ((A (make-bool-vector 10 nil)))
    (dotimes (i 5) (aset A i t))
    (let ((B (nreverse A)))
      (should (eq B A))
      (should (equal [nil nil nil nil nil t t t t t] (vconcat A)))
      (should (equal [t t t t t nil nil nil nil nil] (vconcat (nreverse A)))))))

(defconst fns-tests--string-lessp-cases
  `(("abc" < "abd")
    (abc < "abd")
    (abc < abd)
    ("" = "")
    ("" < " ")
    ("abc" < "abcd")
    ("abc" = "abc")
    (abc = abc)
    ("" < "\0")
    ("~" < "\x80")
    ("\x80" = "\x80")
    ("\xfe" < "\xff")
    ("Munchen" < "München")
    ("München" = "München")
    ("Ré" < "Réunion")
    ("abc" = ,(string-to-multibyte "abc"))
    (,(string-to-multibyte "abc") = ,(string-to-multibyte "abc"))
    ("abc" < ,(string-to-multibyte "abd"))
    (,(string-to-multibyte "abc") < "abd")
    (,(string-to-multibyte "abc") < ,(string-to-multibyte "abd"))
    (,(string-to-multibyte "\x80") = ,(string-to-multibyte "\x80"))
    ("Liberté, Égalité, Fraternité" = "Liberté, Égalité, Fraternité")
    ("Liberté, Égalité, Fraternité" < "Liberté, Égalité, Sororité")

    ;; Cases concerning the ordering of raw bytes: these are
    ;; troublesome because the current `string<' order is not very useful as
    ;; it equates unibyte 80..FF with multibyte U+0080..00FF, and is also
    ;; inconsistent with `string=' (see bug#58168).
    ;;("\x80" < ,(string-to-multibyte "\x80"))
    ;;("\xff" < ,(string-to-multibyte "\x80"))
    ;;("ü" < "\xfc")
    ;;("ü" < ,(string-to-multibyte "\xfc"))
    )
  "List of (A REL B) where REL is the relation (`<' or `=') between A and B.")

(ert-deftest fns-tests-string-lessp ()
  ;; Exercise both `string-lessp' and its alias `string<', both directly
  ;; and in a function (exercising its bytecode).
  (dolist (fun (list #'string-lessp #'string<
                     (lambda (a b) (string-lessp a b))
                     (lambda (a b) (string< a b))))
    (ert-info ((prin1-to-string fun) :prefix "function: ")
      (should-error (funcall fun 'a 97))
      (should-error (funcall fun 97 "a"))
      (dolist (case fns-tests--string-lessp-cases)
        (ert-info ((prin1-to-string case) :prefix "case: ")
          (pcase-let ((`(,x ,rel ,y) case))
            (cl-assert (memq rel '(< =)))
            (should (equal (funcall fun x y) (eq rel '<)))
            (should (equal (funcall fun y x) nil))))))))

(ert-deftest fns-tests-compare-strings ()
  (should-error (compare-strings))
  (should-error (compare-strings "xyzzy" "xyzzy"))
  (should (= (compare-strings "xyzzy" 0 10 "zyxxy" 0 5) -1))
  (should-error (compare-strings "xyzzy" 0 5 "zyxxy" -1 2))
  (should-error (compare-strings "xyzzy" 'foo nil "zyxxy" 0 1))
  (should-error (compare-strings "xyzzy" 0 'foo "zyxxy" 2 3))
  (should-error (compare-strings "xyzzy" 0 2 "zyxxy" 'foo 3))
  (should-error (compare-strings "xyzzy" nil 3 "zyxxy" 4 'foo))
  (should (eq (compare-strings "" nil nil "" nil nil) t))
  (should (eq (compare-strings "" 0 0 "" 0 0) t))
  (should (eq (compare-strings "test" nil nil "test" nil nil) t))
  (should (eq (compare-strings "test" nil nil "test" nil nil t) t))
  (should (eq (compare-strings "test" nil nil "test" nil nil nil) t))
  (should (eq (compare-strings "Test" nil nil "test" nil nil t) t))
  (should (= (compare-strings "Test" nil nil "test" nil nil) -1))
  (should (= (compare-strings "Test" nil nil "test" nil nil) -1))
  (should (= (compare-strings "test" nil nil "Test" nil nil) 1))
  (should (= (compare-strings "foobaz" nil nil "barbaz" nil nil) 1))
  (should (= (compare-strings "barbaz" nil nil "foobar" nil nil) -1))
  (should (= (compare-strings "foobaz" nil nil "farbaz" nil nil) 2))
  (should (= (compare-strings "farbaz" nil nil "foobar" nil nil) -2))
  (should (eq (compare-strings "abcxyz" 0 2 "abcprq" 0 2) t))
  (should (eq (compare-strings "abcxyz" 0 -3 "abcprq" 0 -3) t))
  (should (= (compare-strings "abcxyz" 0 6 "abcprq" 0 6) 4))
  (should (= (compare-strings "abcprq" 0 6 "abcxyz" 0 6) -4))
  (should (eq (compare-strings "xyzzy" -3 4 "azza" -3 3) t))
  (should (eq (compare-strings "こんにちはｺﾝﾆﾁﾊ" nil nil "こんにちはｺﾝﾆﾁﾊ" nil nil) t))
  (should (= (compare-strings "んにちはｺﾝﾆﾁﾊこ" nil nil "こんにちはｺﾝﾆﾁﾊ" nil nil) 1))
  (should (= (compare-strings "こんにちはｺﾝﾆﾁﾊ" nil nil "んにちはｺﾝﾆﾁﾊこ" nil nil) -1)))

(defun fns-tests--collate-enabled-p ()
  "Check whether collation functions are enabled."
  (and
   ;; When there is no collation library, collation functions fall back
   ;; to their lexicographic counterparts.  We don't need to test then.
   (not (ignore-errors (string-collate-equalp "" "" t)))
   ;; We use a locale, which might not be installed.  Check it.
   (ignore-errors
     (string-collate-equalp
      "" "" (if (eq system-type 'windows-nt) "enu_USA" "en_US.UTF-8")))))

(ert-deftest fns-tests-collate-strings ()
  (skip-unless (fns-tests--collate-enabled-p))

  (should (string-collate-equalp "xyzzy" "xyzzy"))
  (should-not (string-collate-equalp "xyzzy" "XYZZY"))

  ;; In POSIX or C locales, collation order is lexicographic.
  (should (string-collate-lessp "XYZZY" "xyzzy" "POSIX"))
  ;; In a language specific locale on MS-Windows, collation order is different.
  (when (eq system-type 'windows-nt)
    (should (string-collate-lessp "xyzzy" "XYZZY" "enu_USA")))

  ;; Ignore case.
  (should (string-collate-equalp "xyzzy" "XYZZY" nil t))

  ;; Locale must be valid.
  (should-error (string-collate-equalp "xyzzy" "xyzzy" 'not-a-locale)))

;; There must be a check for valid codepoints.  (Check not implemented yet)
;  (should-error
;   (string-collate-equalp (string ?\x00110000) (string ?\x00110000)))
;; Invalid UTF-8 sequences shall be indicated.  How to create such strings?

(ert-deftest fns-tests-sort ()
  (should (equal (sort (list 9 5 2 -1 5 3 8 7 4) (lambda (x y) (< x y)))
		 '(-1 2 3 4 5 5 7 8 9)))
  (should (equal (sort (list 9 5 2 -1 5 3 8 7 4) (lambda (x y) (> x y)))
		 '(9 8 7 5 5 4 3 2 -1)))
  (should (equal (sort (vector 9 5 2 -1 5 3 8 7 4) (lambda (x y) (< x y)))
		 [-1 2 3 4 5 5 7 8 9]))
  (should (equal (sort (vector 9 5 2 -1 5 3 8 7 4) (lambda (x y) (> x y)))
		 [9 8 7 5 5 4 3 2 -1]))
  ;; Sort a reversed list and vector.
  (should (equal
	 (sort (reverse (number-sequence 1 1000)) (lambda (x y) (< x y)))
	 (number-sequence 1 1000)))
  (should (equal
	   (sort (reverse (vconcat (number-sequence 1 1000)))
                 (lambda (x y) (< x y)))
	 (vconcat (number-sequence 1 1000))))
  ;; Sort a constant list and vector.
  (should (equal
           (sort (make-vector 100 1) (lambda (x y) (> x y)))
           (make-vector 100 1)))
  (should (equal
           (sort (append (make-vector 100 1) nil) (lambda (x y) (> x y)))
           (append (make-vector 100 1) nil)))
  ;; Sort a long list and vector with every pair reversed.
  (let ((vec (make-vector 100000 nil))
        (logxor-vec (make-vector 100000 nil)))
    (dotimes (i 100000)
      (aset logxor-vec i  (logxor i 1))
      (aset vec i i))
    (should (equal
             (sort logxor-vec (lambda (x y) (< x y)))
             vec))
    (should (equal
             (sort (append logxor-vec nil) (lambda (x y) (< x y)))
             (append vec nil))))
  ;; Sort a list and vector with seven swaps.
  (let ((vec (make-vector 100 nil))
        (swap-vec (make-vector 100 nil)))
    (dotimes (i 100)
      (aset vec i (- i 50))
      (aset swap-vec i (- i 50)))
    (mapc (lambda (p)
	(let ((tmp (elt swap-vec (car p))))
	  (aset swap-vec (car p) (elt swap-vec (cdr p)))
	  (aset swap-vec (cdr p) tmp)))
          '((48 . 94) (75 . 77) (33 . 41) (92 . 52)
            (10 . 96) (1 . 14) (43 . 81)))
    (should (equal
             (sort (copy-sequence swap-vec) (lambda (x y) (< x y)))
             vec))
    (should (equal
             (sort (append swap-vec nil) (lambda (x y) (< x y)))
             (append vec nil))))
  ;; Check for possible corruption after GC.
  (let* ((size 3000)
         (complex-vec (make-vector size nil))
         (vec (make-vector size nil))
         (counter 0)
         (my-counter (lambda ()
                       (if (< counter 500)
                           (incf counter)
                         (setq counter 0)
                         (garbage-collect))))
         (rand 1)
         (generate-random
	  (lambda () (setq rand
                           (logand (+ (* rand 1103515245) 12345)  2147483647)))))
    ;; Make a complex vector and its sorted version.
    (dotimes (i size)
      (let ((r (funcall generate-random)))
        (aset complex-vec i (cons r "a"))
        (aset vec i (cons r "a"))))
    ;; Sort it.
    (should (equal
             (sort complex-vec
                   (lambda (x y) (funcall my-counter) (< (car x) (car y))))
             (sort vec 'car-less-than-car))))
  ;; Check for sorting stability.
  (should (equal
	   (sort
	    (vector
	     '(8 . "xxx") '(9 . "aaa") '(8 . "bbb") '(9 . "zzz")
	     '(9 . "ppp") '(8 . "ttt") '(8 . "eee") '(9 . "fff"))
	    (lambda (x y) (< (car x) (car y))))
	   [(8 . "xxx") (8 . "bbb") (8 . "ttt") (8 . "eee")
	    (9 . "aaa") (9 . "zzz") (9 . "ppp") (9 . "fff")]))
  ;; Bug#34104
  (should (equal (should-error (sort "cba" #'<) :type 'wrong-type-argument)
                 '(wrong-type-argument list-or-vector-p "cba"))))

(defun fns-tests--shuffle-vector (vect)
  "Shuffle VECT in place."
  (let ((n (length vect)))
    (dotimes (i (1- n))
      (let* ((j (+ i (random (- n i))))
             (vi (aref vect i)))
        (aset vect i (aref vect j))
        (aset vect j vi)))))

(ert-deftest fns-tests-sort-kw ()
  ;; Test the `sort' keyword calling convention by comparing with
  ;; the results from using the old (positional) style tested above.
  (random "my seed")
  (dolist (size '(0 1 2 3 10 100 1000))
    ;; Use a vector with both positive and negative numbers (asymmetric).
    (let ((numbers (vconcat
                    (number-sequence (- (/ size 3)) (- size 1 (/ size 3))))))
      (fns-tests--shuffle-vector numbers)
      ;; Test both list and vector input.
      (dolist (input (list (append numbers nil) numbers))
        (dolist (in-place '(nil t))
          (dolist (reverse '(nil t))
            (dolist (key '(nil abs))
              (dolist (lessp '(nil >))
                (let* ((seq (copy-sequence input))
                       (res (sort seq :key key :lessp lessp
                                  :in-place in-place :reverse reverse))
                       (pred (or lessp #'value<))
                       (exp-in (copy-sequence input))
                       (exp-out
                        (sort (if reverse (reverse exp-in) exp-in)
                              (if key
                                  (lambda (a b)
                                    (funcall pred
                                             (funcall key a) (funcall key b)))
                                pred)))
                       (expected (if reverse (reverse exp-out) exp-out)))
                  (should (equal res expected))
                  (if in-place
                      (should (eq res seq))
                    (should-not (and (> size 0) (eq res seq)))
                    (should (equal seq input))))))))))))

(ert-deftest fns-tests-sort-gc ()
  ;; Make sure our temporary storage is traversed by the GC.
  (let* ((n 1000)
         (a (mapcar #'number-to-string (number-sequence 1 n)))
         (i 0)
         ;; Force frequent GCs in both the :key and :lessp functions.
         (s (sort a
                  :key (lambda (x)
                         (setq i (1+ i))
                         (when (> i 300)
                           (garbage-collect)
                           (setq i 0))
                         (copy-sequence x))
                  :lessp (lambda (a b)
                           (setq i (1+ i))
                           (when (> i 300)
                             (garbage-collect)
                             (setq i 0))
                           (string< a b)))))
    (should (equal (length s) (length a)))))

(defvar w32-collate-ignore-punctuation)

(ert-deftest fns-tests-collate-sort ()
  (skip-unless (fns-tests--collate-enabled-p))

  ;; Punctuation and whitespace characters are relevant for POSIX.
  (should
   (equal
    (sort (list "11" "12" "1 1" "1 2" "1.1" "1.2")
	  (lambda (a b) (string-collate-lessp a b "POSIX")))
    '("1 1" "1 2" "1.1" "1.2" "11" "12")))
  ;; Punctuation and whitespace characters are not taken into account
  ;; for collation in other locales, on MS-Windows systems.
  (when (eq system-type 'windows-nt)
    (should
     (equal
      (sort (list "11" "12" "1 1" "1 2" "1.1" "1.2")
            (lambda (a b)
              (let ((w32-collate-ignore-punctuation t))
                (string-collate-lessp
                 a b "enu_USA"))))
      '("11" "1 1" "1.1" "12" "1 2" "1.2"))))

  ;; Diacritics are different letters for POSIX, they sort lexicographical.
  (should
   (equal
    (sort (list "Ævar" "Agustín" "Adrian" "Eli")
	  (lambda (a b) (string-collate-lessp a b "POSIX")))
    '("Adrian" "Agustín" "Eli" "Ævar")))
  ;; Diacritics are sorted between similar letters for other locales,
  ;; on MS-Windows systems.
  (when (eq system-type 'windows-nt)
    (should
     (equal
      (sort (list "Ævar" "Agustín" "Adrian" "Eli")
            (lambda (a b)
              (let ((w32-collate-ignore-punctuation t))
                (string-collate-lessp
                 a b "enu_USA"))))
      '("Adrian" "Ævar" "Agustín" "Eli")))))

(ert-deftest fns-tests-string-version-lessp ()
  (should (string-version-lessp "foo2.png" "foo12.png"))
  (should (not (string-version-lessp "foo12.png" "foo2.png")))
  (should (string-version-lessp "foo12.png" "foo20000.png"))
  (should (not (string-version-lessp "foo20000.png" "foo12.png")))
  (should (string-version-lessp "foo.png" "foo2.png"))
  (should (not (string-version-lessp "foo2.png" "foo.png")))
  (should (equal (sort (list "foo12.png" "foo2.png" "foo1.png")
                       'string-version-lessp)
                 '("foo1.png" "foo2.png" "foo12.png")))
  (should (string-version-lessp "foo2" "foo1234"))
  (should (not (string-version-lessp "foo1234" "foo2")))
  (should (string-version-lessp "foo.png" "foo2"))
  (should (string-version-lessp "foo1.25.5.png" "foo1.125.5"))
  (should (string-version-lessp "2" "1245"))
  (should (not (string-version-lessp "1245" "2"))))

(ert-deftest fns-tests-func-arity ()
  (should (equal (func-arity 'car) '(1 . 1)))
  (should (equal (func-arity 'caar) '(1 . 1)))
  (should (equal (func-arity 'format) '(1 . many)))
  (require 'info)
  (should (equal (func-arity 'Info-goto-node) '(1 . 3)))
  (should (equal (func-arity (lambda (&rest _x))) '(0 . many)))
  (should (equal (func-arity (eval '(lambda (_x &optional y)) nil)) '(1 . 2)))
  (should (equal (func-arity (eval '(lambda (_x &optional y)) t)) '(1 . 2)))
  (should (equal (func-arity 'let) '(1 . unevalled))))

(defun fns-tests--string-repeat (s o)
  (apply 'concat (make-list o s)))

(defmacro fns-tests--with-region (funcname string &rest args)
  "Apply FUNCNAME in a temp buffer on the region produced by STRING."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,string)
     (,funcname (point-min) (point-max) ,@args)
     (buffer-string)))

(ert-deftest fns-tests-base64-encode-region ()
  ;; standard variant RFC2045
  (should (equal (fns-tests--with-region base64-encode-region "") ""))
  (should (equal (fns-tests--with-region base64-encode-region "f") "Zg=="))
  (should (equal (fns-tests--with-region base64-encode-region "fo") "Zm8="))
  (should (equal (fns-tests--with-region base64-encode-region "foo") "Zm9v"))
  (should (equal (fns-tests--with-region base64-encode-region "foob") "Zm9vYg=="))
  (should (equal (fns-tests--with-region base64-encode-region "fooba") "Zm9vYmE="))
  (should (equal (fns-tests--with-region base64-encode-region "foobar") "Zm9vYmFy"))
  (should (equal (fns-tests--with-region base64-encode-region "\x14\xfb\x9c\x03\xd9\x7e") "FPucA9l+"))
  (should (equal (fns-tests--with-region base64-encode-region "\x14\xfb\x9c\x03\xd9\x7f") "FPucA9l/")))

(ert-deftest fns-tests-base64-encode-string ()
  ;; standard variant RFC2045
  (should (equal (base64-encode-string "") ""))
  (should (equal (base64-encode-string "f") "Zg=="))
  (should (equal (base64-encode-string "fo") "Zm8="))
  (should (equal (base64-encode-string "foo") "Zm9v"))
  (should (equal (base64-encode-string "foob") "Zm9vYg=="))
  (should (equal (base64-encode-string "fooba") "Zm9vYmE="))
  (should (equal (base64-encode-string "foobar") "Zm9vYmFy"))
  (should (equal (base64-encode-string "\x14\xfb\x9c\x03\xd9\x7e") "FPucA9l+"))
  (should (equal (base64-encode-string "\x14\xfb\x9c\x03\xd9\x7f") "FPucA9l/"))

  (should-error (base64-encode-string "ƒ"))
  (should-error (base64-encode-string "ü")))

(ert-deftest fns-test-base64url-encode-region ()
  ;; url variant with padding
  (should (equal (fns-tests--with-region base64url-encode-region "") ""))
  (should (equal (fns-tests--with-region base64url-encode-region "f") "Zg=="))
  (should (equal (fns-tests--with-region base64url-encode-region "fo") "Zm8="))
  (should (equal (fns-tests--with-region base64url-encode-region "foo") "Zm9v"))
  (should (equal (fns-tests--with-region base64url-encode-region "foob") "Zm9vYg=="))
  (should (equal (fns-tests--with-region base64url-encode-region "fooba") "Zm9vYmE="))
  (should (equal (fns-tests--with-region base64url-encode-region "foobar") "Zm9vYmFy"))
  (should (equal (fns-tests--with-region base64url-encode-region "\x14\xfb\x9c\x03\xd9\x7e") "FPucA9l-"))
  (should (equal (fns-tests--with-region base64url-encode-region "\x14\xfb\x9c\x03\xd9\x7f") "FPucA9l_"))

  ;; url variant no padding
  (should (equal (fns-tests--with-region base64url-encode-region "" t) ""))
  (should (equal (fns-tests--with-region base64url-encode-region "f" t) "Zg"))
  (should (equal (fns-tests--with-region base64url-encode-region "fo" t) "Zm8"))
  (should (equal (fns-tests--with-region base64url-encode-region "foo" t) "Zm9v"))
  (should (equal (fns-tests--with-region base64url-encode-region "foob" t) "Zm9vYg"))
  (should (equal (fns-tests--with-region base64url-encode-region "fooba" t) "Zm9vYmE"))
  (should (equal (fns-tests--with-region base64url-encode-region "foobar" t) "Zm9vYmFy"))
  (should (equal (fns-tests--with-region base64url-encode-region "\x14\xfb\x9c\x03\xd9\x7e" t) "FPucA9l-"))
  (should (equal (fns-tests--with-region base64url-encode-region "\x14\xfb\x9c\x03\xd9\x7f" t) "FPucA9l_"))


  ;; url variant no line break no padding
  (should (equal (fns-tests--with-region base64url-encode-region (fns-tests--string-repeat "f" 100) t)
                 (concat (fns-tests--string-repeat "Zm" 66) "Zg")))
  (should (equal (fns-tests--with-region base64url-encode-region (fns-tests--string-repeat "fo" 50) t)
                 (concat (fns-tests--string-repeat "Zm9mb2Zv" 16) "Zm9mbw")))
  (should (equal (fns-tests--with-region base64url-encode-region (fns-tests--string-repeat "foo" 25) t)
                 (fns-tests--string-repeat "Zm9v" 25)))
  (should (equal (fns-tests--with-region base64url-encode-region (fns-tests--string-repeat "foob" 15) t)
                 (fns-tests--string-repeat "Zm9vYmZvb2Jmb29i" 5)))
  (should (equal (fns-tests--with-region base64url-encode-region (fns-tests--string-repeat "fooba" 15) t)
                 (fns-tests--string-repeat "Zm9vYmFmb29iYWZvb2Jh" 5)))
  (should (equal (fns-tests--with-region base64url-encode-region (fns-tests--string-repeat "foobar" 15) t)
                 (concat (fns-tests--string-repeat "Zm9vYmFyZm9vYmFy" 7) "Zm9vYmFy")))
  (should (equal (fns-tests--with-region base64url-encode-region (fns-tests--string-repeat "\x14\xfb\x9c\x03\xd9\x7e" 10) t)
                 (fns-tests--string-repeat "FPucA9l-" 10)))
  (should (equal (fns-tests--with-region base64url-encode-region (fns-tests--string-repeat "\x14\xfb\x9c\x03\xd9\x7f" 10) t)
                 (fns-tests--string-repeat "FPucA9l_" 10)))

  (should-error (fns-tests--with-region base64url-encode-region "ƒ"))
  (should-error (fns-tests--with-region base64url-encode-region "ü")))


(ert-deftest fns-test-base64url-encode-string ()
  ;; url variant with padding
  (should (equal (base64url-encode-string "") ""))
  (should (equal (base64url-encode-string "f") "Zg=="))
  (should (equal (base64url-encode-string "fo") "Zm8="))
  (should (equal (base64url-encode-string "foo") "Zm9v"))
  (should (equal (base64url-encode-string "foob") "Zm9vYg=="))
  (should (equal (base64url-encode-string "fooba") "Zm9vYmE="))
  (should (equal (base64url-encode-string "foobar") "Zm9vYmFy"))
  (should (equal (base64url-encode-string "\x14\xfb\x9c\x03\xd9\x7e") "FPucA9l-"))
  (should (equal (base64url-encode-string "\x14\xfb\x9c\x03\xd9\x7f") "FPucA9l_"))

  ;; url variant no padding
  (should (equal (base64url-encode-string "" t) ""))
  (should (equal (base64url-encode-string "f" t) "Zg"))
  (should (equal (base64url-encode-string "fo" t) "Zm8"))
  (should (equal (base64url-encode-string "foo" t) "Zm9v"))
  (should (equal (base64url-encode-string "foob" t) "Zm9vYg"))
  (should (equal (base64url-encode-string "fooba" t) "Zm9vYmE"))
  (should (equal (base64url-encode-string "foobar" t) "Zm9vYmFy"))
  (should (equal (base64url-encode-string "\x14\xfb\x9c\x03\xd9\x7e" t) "FPucA9l-"))
  (should (equal (base64url-encode-string "\x14\xfb\x9c\x03\xd9\x7f" t) "FPucA9l_"))


  ;; url variant no line break no padding
  (should (equal (base64url-encode-string (fns-tests--string-repeat "f" 100) t) (concat (fns-tests--string-repeat "Zm" 66) "Zg")))
  (should (equal (base64url-encode-string (fns-tests--string-repeat "fo" 50) t) (concat (fns-tests--string-repeat "Zm9mb2Zv" 16) "Zm9mbw")))
  (should (equal (base64url-encode-string (fns-tests--string-repeat "foo" 25) t) (fns-tests--string-repeat "Zm9v" 25)))
  (should (equal (base64url-encode-string (fns-tests--string-repeat "foob" 15) t) (fns-tests--string-repeat "Zm9vYmZvb2Jmb29i" 5)))
  (should (equal (base64url-encode-string (fns-tests--string-repeat "fooba" 15) t) (fns-tests--string-repeat "Zm9vYmFmb29iYWZvb2Jh" 5)))
  (should (equal (base64url-encode-string (fns-tests--string-repeat "foobar" 15) t) (concat (fns-tests--string-repeat "Zm9vYmFyZm9vYmFy" 7) "Zm9vYmFy")))
  (should (equal (base64url-encode-string (fns-tests--string-repeat "\x14\xfb\x9c\x03\xd9\x7e" 10) t) (fns-tests--string-repeat "FPucA9l-" 10)))
  (should (equal (base64url-encode-string (fns-tests--string-repeat "\x14\xfb\x9c\x03\xd9\x7f" 10) t) (fns-tests--string-repeat "FPucA9l_" 10)))

  (should-error (base64url-encode-string "ƒ"))
  (should-error (base64url-encode-string "ü")))

(ert-deftest fns-tests-base64-decode-string ()
  ;; standard variant RFC2045
  (should (equal (base64-decode-string "") ""))
  (should (equal (base64-decode-string "Zg==") "f"))
  (should (equal (base64-decode-string "Zm8=") "fo"))
  (should (equal (base64-decode-string "Zm9v") "foo"))
  (should (equal (base64-decode-string "Zm9vYg==") "foob"))
  (should (equal (base64-decode-string "Zm9vYmE=") "fooba"))
  (should (equal (base64-decode-string "Zm9vYmFy") "foobar"))
  (should (equal (base64-decode-string "FPucA9l+") "\x14\xfb\x9c\x03\xd9\x7e"))
  (should (equal (base64-decode-string "FPucA9l/") "\x14\xfb\x9c\x03\xd9\x7f"))

  ;; no padding
  (should (equal (base64-decode-string "" t) ""))
  (should (equal (base64-decode-string "Zg" t) "f"))
  (should (equal (base64-decode-string "Zm8" t) "fo"))
  (should (equal (base64-decode-string "Zm9v" t) "foo"))
  (should (equal (base64-decode-string "Zm9vYg" t) "foob"))
  (should (equal (base64-decode-string "Zm9vYmE" t) "fooba"))
  (should (equal (base64-decode-string "Zm9vYmFy" t) "foobar"))

  ;; url variant with padding
  (should (equal (base64-decode-string "") ""))
  (should (equal (base64-decode-string "Zg==" t) "f") )
  (should (equal (base64-decode-string "Zm8=" t) "fo"))
  (should (equal (base64-decode-string "Zm9v" t) "foo"))
  (should (equal (base64-decode-string "Zm9vYg==" t) "foob"))
  (should (equal (base64-decode-string "Zm9vYmE=" t) "fooba"))
  (should (equal (base64-decode-string "Zm9vYmFy" t) "foobar"))
  (should (equal (base64-decode-string "FPucA9l-" t) "\x14\xfb\x9c\x03\xd9\x7e"))
  (should (equal (base64-decode-string "FPucA9l_" t) "\x14\xfb\x9c\x03\xd9\x7f"))

  ;; url variant no padding
  (should (equal (base64-decode-string "") ""))
  (should (equal (base64-decode-string "Zg" t) "f"))
  (should (equal (base64-decode-string "Zm8" t) "fo"))
  (should (equal (base64-decode-string "Zm9v" t) "foo"))
  (should (equal (base64-decode-string "Zm9vYg" t) "foob"))
  (should (equal (base64-decode-string "Zm9vYmE" t) "fooba"))
  (should (equal (base64-decode-string "Zm9vYmFy" t) "foobar"))
  (should (equal (base64-decode-string "FPucA9l-" t) "\x14\xfb\x9c\x03\xd9\x7e"))
  (should (equal (base64-decode-string "FPucA9l_" t) "\x14\xfb\x9c\x03\xd9\x7f"))


  ;; url variant no line break no padding
  (should (equal (base64-decode-string (concat (fns-tests--string-repeat "Zm" 66) "Zg") t)
                 (fns-tests--string-repeat "f" 100)))
  (should (equal (base64-decode-string (concat (fns-tests--string-repeat "Zm9mb2Zv" 16) "Zm9mbw") t)
                 (fns-tests--string-repeat "fo" 50)))
  (should (equal (base64-decode-string (fns-tests--string-repeat "Zm9v" 25) t)
                 (fns-tests--string-repeat "foo" 25)))
  (should (equal (base64-decode-string (fns-tests--string-repeat "Zm9vYmZvb2Jmb29i" 5) t)
                 (fns-tests--string-repeat "foob" 15)))
  (should (equal (base64-decode-string (fns-tests--string-repeat "Zm9vYmFmb29iYWZvb2Jh" 5) t)
                 (fns-tests--string-repeat "fooba" 15)))
  (should (equal (base64-decode-string (concat (fns-tests--string-repeat "Zm9vYmFyZm9vYmFy" 7) "Zm9vYmFy") t)
                 (fns-tests--string-repeat "foobar" 15)))
  (should (equal (base64-decode-string (fns-tests--string-repeat "FPucA9l-" 10) t)
                 (fns-tests--string-repeat "\x14\xfb\x9c\x03\xd9\x7e" 10)))
  (should (equal (base64-decode-string (fns-tests--string-repeat "FPucA9l_" 10) t)
                 (fns-tests--string-repeat "\x14\xfb\x9c\x03\xd9\x7f" 10)))

  ;; errors check
  (should (eq :got-error (condition-case () (base64-decode-string "Zg=") (error :got-error))))
  (should (eq :got-error (condition-case () (base64-decode-string "Zm9vYmE") (error :got-error))))
  (should (eq :got-error (condition-case () (base64-decode-string "Zm9vYmFy=") (error :got-error))))
  (should (eq :got-error (condition-case () (base64-decode-string "Zg=Zg=") (error :got-error)))))

(ert-deftest fns-tests-hash-buffer ()
  (should (equal (sha1 "foo") "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"))
  (should (equal (with-temp-buffer
                   (insert "foo")
                   (buffer-hash))
                 (sha1 "foo")))
  ;; This tests whether the presence of a gap in the middle of the
  ;; buffer is handled correctly.
  (should (equal (with-temp-buffer
                   (insert "foo")
                   (goto-char 2)
                   (insert " ")
                   (delete-char -1)
                   (buffer-hash))
                 (sha1 "foo"))))

(ert-deftest fns-tests-mapconcat ()
  (should (string= (mapconcat #'identity '()) ""))
  (should (string= (mapconcat #'identity '("a" "b")) "ab"))
  (should (string= (mapconcat #'identity '() "_") ""))
  (should (string= (mapconcat #'identity '("A") "_") "A"))
  (should (string= (mapconcat #'identity '("A" "B") "_") "A_B"))
  (should (string= (mapconcat #'identity '("A" "B" "C") "_") "A_B_C"))
  ;; non-ASCII strings
  (should (string= (mapconcat #'identity '("Ä" "ø" "☭" "தமிழ்") "_漢字_")
                   "Ä_漢字_ø_漢字_☭_漢字_தமிழ்"))
  ;; vector
  (should (string= (mapconcat #'identity ["a" "b"]) "ab"))
  ;; bool-vector
  (should (string= (mapconcat #'identity [nil nil]) ""))
  (should-error (mapconcat #'identity [nil nil t])
                :type 'wrong-type-argument))

(ert-deftest fns-tests-mapcan ()
  (should-error (mapcan))
  (should-error (mapcan #'identity))
  (should-error (mapcan #'identity (make-char-table 'foo)))
  (should (equal (mapcan #'list (list 1 2 3)) '(1 2 3)))
  ;; `mapcan' is destructive
  (let ((data (list (list 'foo) (list 'bar))))
    (should (equal (mapcan #'identity data) '(foo bar)))
    (should (equal data                     '((foo bar) (bar))))))

;; Test handling of cyclic and dotted lists.

(defun cyc1 (a)
  (let ((ls (make-list 10 a)))
    (nconc ls ls)
    ls))

(defun cyc2 (a b)
  (let ((ls1 (make-list 10 a))
        (ls2 (make-list 1000 b)))
    (nconc ls2 ls2)
    (nconc ls1 ls2)
    ls1))

(defun dot1 (a)
  (let ((ls (make-list 10 a)))
    (nconc ls 'tail)
    ls))

(defun dot2 (a b)
  (let ((ls1 (make-list 10 a))
        (ls2 (make-list 10 b)))
    (nconc ls1 ls2)
    (nconc ls2 'tail)
    ls1))

(ert-deftest test-cycle-length ()
  (should-error (length (cyc1 1)) :type 'circular-list)
  (should-error (length (cyc2 1 2)) :type 'circular-list)
  (should-error (length (dot1 1)) :type 'wrong-type-argument)
  (should-error (length (dot2 1 2)) :type 'wrong-type-argument))

(ert-deftest test-cycle-safe-length ()
  (should (<= 10 (safe-length (cyc1 1))))
  (should (<= 1010 (safe-length (cyc2 1 2))))
  (should (= 10 (safe-length (dot1 1))))
  (should (= 20 (safe-length (dot2 1 2)))))

(ert-deftest test-cycle-member ()
  (let ((c1 (cyc1 1))
        (c2 (cyc2 1 2))
        (d1 (dot1 1))
        (d2 (dot2 1 2)))
    (should (member 1 c1))
    (should (member 1 c2))
    (should (member 1 d1))
    (should (member 1 d2))
    (should-error (member 2 c1) :type 'circular-list)
    (should (member 2 c2))
    (should-error (member 2 d1) :type 'wrong-type-argument)
    (should (member 2 d2))
    (should-error (member 3 c1) :type 'circular-list)
    (should-error (member 3 c2) :type 'circular-list)
    (should-error (member 3 d1) :type 'wrong-type-argument)
    (should-error (member 3 d2) :type 'wrong-type-argument)))

(ert-deftest test-cycle-memq ()
  (let ((c1 (cyc1 1))
        (c2 (cyc2 1 2))
        (d1 (dot1 1))
        (d2 (dot2 1 2)))
    (should (memq 1 c1))
    (should (memq 1 c2))
    (should (memq 1 d1))
    (should (memq 1 d2))
    (should-error (memq 2 c1) :type 'circular-list)
    (should (memq 2 c2))
    (should-error (memq 2 d1) :type 'wrong-type-argument)
    (should (memq 2 d2))
    (should-error (memq 3 c1) :type 'circular-list)
    (should-error (memq 3 c2) :type 'circular-list)
    (should-error (memq 3 d1) :type 'wrong-type-argument)
    (should-error (memq 3 d2) :type 'wrong-type-argument)))

(ert-deftest test-cycle-memql ()
  (let ((c1 (cyc1 1))
        (c2 (cyc2 1 2))
        (d1 (dot1 1))
        (d2 (dot2 1 2)))
    (should (memql 1 c1))
    (should (memql 1 c2))
    (should (memql 1 d1))
    (should (memql 1 d2))
    (should-error (memql 2 c1) :type 'circular-list)
    (should (memql 2 c2))
    (should-error (memql 2 d1) :type 'wrong-type-argument)
    (should (memql 2 d2))
    (should-error (memql 3 c1) :type 'circular-list)
    (should-error (memql 3 c2) :type 'circular-list)
    (should-error (memql 3 d1) :type 'wrong-type-argument)
    (should-error (memql 3 d2) :type 'wrong-type-argument)))

(ert-deftest test-cycle-assq ()
  (let ((c1 (cyc1 '(1)))
        (c2 (cyc2 '(1) '(2)))
        (d1 (dot1 '(1)))
        (d2 (dot2 '(1) '(2))))
    (should (assq 1 c1))
    (should (assq 1 c2))
    (should (assq 1 d1))
    (should (assq 1 d2))
    (should-error (assq 2 c1) :type 'circular-list)
    (should (assq 2 c2))
    (should-error (assq 2 d1) :type 'wrong-type-argument)
    (should (assq 2 d2))
    (should-error (assq 3 c1) :type 'circular-list)
    (should-error (assq 3 c2) :type 'circular-list)
    (should-error (assq 3 d1) :type 'wrong-type-argument)
    (should-error (assq 3 d2) :type 'wrong-type-argument)))

(ert-deftest test-cycle-assoc ()
  (let ((c1 (cyc1 '(1)))
        (c2 (cyc2 '(1) '(2)))
        (d1 (dot1 '(1)))
        (d2 (dot2 '(1) '(2))))
    (should (assoc 1 c1))
    (should (assoc 1 c2))
    (should (assoc 1 d1))
    (should (assoc 1 d2))
    (should-error (assoc 2 c1) :type 'circular-list)
    (should (assoc 2 c2))
    (should-error (assoc 2 d1) :type 'wrong-type-argument)
    (should (assoc 2 d2))
    (should-error (assoc 3 c1) :type 'circular-list)
    (should-error (assoc 3 c2) :type 'circular-list)
    (should-error (assoc 3 d1) :type 'wrong-type-argument)
    (should-error (assoc 3 d2) :type 'wrong-type-argument)))

(ert-deftest test-assoc-testfn ()
  (let ((alist '(("a" . 1) ("b" . 2))))
    (should-not (assoc "a" alist #'ignore))
    (should (eq (assoc "b" alist #'string-equal) (cadr alist)))
    (should-not (assoc "b" alist #'eq))))

(ert-deftest test-cycle-rassq ()
  (let ((c1 (cyc1 '(0 . 1)))
        (c2 (cyc2 '(0 . 1) '(0 . 2)))
        (d1 (dot1 '(0 . 1)))
        (d2 (dot2 '(0 . 1) '(0 . 2))))
    (should (rassq 1 c1))
    (should (rassq 1 c2))
    (should (rassq 1 d1))
    (should (rassq 1 d2))
    (should-error (rassq 2 c1) :type 'circular-list)
    (should (rassq 2 c2))
    (should-error (rassq 2 d1) :type 'wrong-type-argument)
    (should (rassq 2 d2))
    (should-error (rassq 3 c1) :type 'circular-list)
    (should-error (rassq 3 c2) :type 'circular-list)
    (should-error (rassq 3 d1) :type 'wrong-type-argument)
    (should-error (rassq 3 d2) :type 'wrong-type-argument)))

(ert-deftest test-cycle-rassoc ()
  (let ((c1 (cyc1 '(0 . 1)))
        (c2 (cyc2 '(0 . 1) '(0 . 2)))
        (d1 (dot1 '(0 . 1)))
        (d2 (dot2 '(0 . 1) '(0 . 2))))
    (should (rassoc 1 c1))
    (should (rassoc 1 c2))
    (should (rassoc 1 d1))
    (should (rassoc 1 d2))
    (should-error (rassoc 2 c1) :type 'circular-list)
    (should (rassoc 2 c2))
    (should-error (rassoc 2 d1) :type 'wrong-type-argument)
    (should (rassoc 2 d2))
    (should-error (rassoc 3 c1) :type 'circular-list)
    (should-error (rassoc 3 c2) :type 'circular-list)
    (should-error (rassoc 3 d1) :type 'wrong-type-argument)
    (should-error (rassoc 3 d2) :type 'wrong-type-argument)))

(ert-deftest test-cycle-delq ()
  (should-error (delq 1 (cyc1 1)) :type 'circular-list)
  (should-error (delq 1 (cyc2 1 2)) :type 'circular-list)
  (should-error (delq 1 (dot1 1)) :type 'wrong-type-argument)
  (should-error (delq 1 (dot2 1 2)) :type 'wrong-type-argument)
  (should-error (delq 2 (cyc1 1)) :type 'circular-list)
  (should-error (delq 2 (cyc2 1 2)) :type 'circular-list)
  (should-error (delq 2 (dot1 1)) :type 'wrong-type-argument)
  (should-error (delq 2 (dot2 1 2)) :type 'wrong-type-argument)
  (should-error (delq 3 (cyc1 1)) :type 'circular-list)
  (should-error (delq 3 (cyc2 1 2)) :type 'circular-list)
  (should-error (delq 3 (dot1 1)) :type 'wrong-type-argument)
  (should-error (delq 3 (dot2 1 2)) :type 'wrong-type-argument))

(ert-deftest test-cycle-delete ()
  (should-error (delete 1 (cyc1 1)) :type 'circular-list)
  (should-error (delete 1 (cyc2 1 2)) :type 'circular-list)
  (should-error (delete 1 (dot1 1)) :type 'wrong-type-argument)
  (should-error (delete 1 (dot2 1 2)) :type 'wrong-type-argument)
  (should-error (delete 2 (cyc1 1)) :type 'circular-list)
  (should-error (delete 2 (cyc2 1 2)) :type 'circular-list)
  (should-error (delete 2 (dot1 1)) :type 'wrong-type-argument)
  (should-error (delete 2 (dot2 1 2)) :type 'wrong-type-argument)
  (should-error (delete 3 (cyc1 1)) :type 'circular-list)
  (should-error (delete 3 (cyc2 1 2)) :type 'circular-list)
  (should-error (delete 3 (dot1 1)) :type 'wrong-type-argument)
  (should-error (delete 3 (dot2 1 2)) :type 'wrong-type-argument))

(ert-deftest test-cycle-reverse ()
  (should-error (reverse (cyc1 1)) :type 'circular-list)
  (should-error (reverse (cyc2 1 2)) :type 'circular-list)
  (should-error (reverse (dot1 1)) :type 'wrong-type-argument)
  (should-error (reverse (dot2 1 2)) :type 'wrong-type-argument))

(ert-deftest test-cycle-equal ()
  (should-error (equal (cyc1 1) (cyc1 1)))
  (should-error (equal (cyc2 1 2) (cyc2 1 2))))

(ert-deftest test-cycle-nconc ()
  (should-error (nconc (cyc1 1) 'tail) :type 'circular-list)
  (should-error (nconc (cyc2 1 2) 'tail) :type 'circular-list))

(ert-deftest test-cycle-plist-get ()
  (let ((c1 (cyc1 1))
        (c2 (cyc2 1 2))
        (d1 (dot1 1))
        (d2 (dot2 1 2)))
    (should (plist-get c1 1))
    (should (plist-get c2 1))
    (should (plist-get d1 1))
    (should (plist-get d2 1))
    (should-not (plist-get c1 2))
    (should (plist-get c2 2))
    (should-not (plist-get d1 2))
    (should (plist-get d2 2))
    (should-not (plist-get c1 3))
    (should-not (plist-get c2 3))
    (should-not (plist-get d1 3))
    (should-not (plist-get d2 3))))

(ert-deftest test-cycle-plist-member ()
  (let ((c1 (cyc1 1))
        (c2 (cyc2 1 2))
        (d1 (dot1 1))
        (d2 (dot2 1 2)))
    (should (plist-member c1 1))
    (should (plist-member c2 1))
    (should (plist-member d1 1))
    (should (plist-member d2 1))
    (should-error (plist-member c1 2) :type 'circular-list)
    (should (plist-member c2 2))
    (should-error (plist-member d1 2) :type 'wrong-type-argument)
    (should (plist-member d2 2))
    (should-error (plist-member c1 3) :type 'circular-list)
    (should-error (plist-member c2 3) :type 'circular-list)
    (should-error (plist-member d1 3) :type 'wrong-type-argument)
    (should-error (plist-member d2 3) :type 'wrong-type-argument)))

(ert-deftest test-cycle-plist-put ()
  (let ((c1 (cyc1 1))
        (c2 (cyc2 1 2))
        (d1 (dot1 1))
        (d2 (dot2 1 2)))
    (should (plist-put c1 1 1))
    (should (plist-put c2 1 1))
    (should (plist-put d1 1 1))
    (should (plist-put d2 1 1))
    (should-error (plist-put c1 2 2) :type 'circular-list)
    (should (plist-put c2 2 2))
    (should-error (plist-put d1 2 2) :type 'wrong-type-argument)
    (should (plist-put d2 2 2))
    (should-error (plist-put c1 3 3) :type 'circular-list)
    (should-error (plist-put c2 3 3) :type 'circular-list)
    (should-error (plist-put d1 3 3) :type 'wrong-type-argument)
    (should-error (plist-put d2 3 3) :type 'wrong-type-argument)))

(ert-deftest plist-get/odd-number-of-elements ()
  "Test that `plist-get' doesn't signal an error on degenerate plists."
  (should-not (plist-get '(:foo 1 :bar) :bar)))

(ert-deftest plist-put/odd-number-of-elements ()
  "Check for bug#27726."
  (should (equal (should-error (plist-put (list :foo 1 :bar) :zot 2))
                 '(wrong-type-argument plistp (:foo 1 :bar)))))

(ert-deftest plist-member/improper-list ()
  "Check for bug#27726."
  (should (equal (should-error (plist-member '(:foo 1 . :bar) :qux))
                 '(wrong-type-argument plistp (:foo 1 . :bar)))))

(ert-deftest test-plist ()
  (let ((plist (list :a "b")))
    (setq plist (plist-put plist :b "c"))
    (should (equal (plist-get plist :b) "c"))
    (should (equal (plist-member plist :b) '(:b "c"))))

  (let ((plist (list "1" "2" "a" "b")))
    (setq plist (plist-put plist (string ?a) "c"))
    (should (equal plist '("1" "2" "a" "b" "a" "c")))
    (should-not (plist-get plist (string ?a)))
    (should-not (plist-member plist (string ?a))))

  (let ((plist (list "1" "2" "a" "b")))
    (setq plist (plist-put plist (string ?a) "c" #'equal))
    (should (equal plist '("1" "2" "a" "c")))
    (should (equal (plist-get plist (string ?a) #'equal) "c"))
    (should (equal (plist-member plist (string ?a) #'equal) '("a" "c"))))

  (let ((plist (list :a 1 :b 2 :c 3)))
    (setq plist (plist-put plist ":a" 4 #'string>))
    (should (equal plist '(:a 1 :b 4 :c 3)))
    (should (equal (plist-get plist ":b" #'string>) 3))
    (should (equal (plist-member plist ":c" #'string<) plist))
    (dolist (fn '(plist-get plist-member))
      (should-not (funcall fn plist ":a" #'string<))
      (should-not (funcall fn plist ":c" #'string>)))))

(ert-deftest test-string-distance ()
  "Test `string-distance' behavior."
  ;; ASCII characters are always fine
  (should (equal 1 (string-distance "heelo" "hello")))
  (should (equal 2 (string-distance "aeelo" "hello")))
  (should (equal 0 (string-distance "ab" "ab" t)))
  (should (equal 1 (string-distance "ab" "abc" t)))

  ;; string containing hanzi character, compare by byte
  (should (equal 6 (string-distance "ab" "ab我她" t)))
  (should (equal 3 (string-distance "ab" "a我b" t)))
  (should (equal 3 (string-distance "我" "她" t)))

  ;; string containing hanzi character, compare by character
  (should (equal 2 (string-distance "ab" "ab我她")))
  (should (equal 1 (string-distance "ab" "a我b")))
  (should (equal 1 (string-distance "我" "她")))

  ;; correct behavior with empty strings
  (should (equal 0 (string-distance "" "")))
  (should (equal 0 (string-distance "" "" t)))
  (should (equal 1 (string-distance "x" "")))
  (should (equal 1 (string-distance "x" "" t)))
  (should (equal 1 (string-distance "" "x")))
  (should (equal 1 (string-distance "" "x" t))))

(ert-deftest test-bignum-eql ()
  "Test that `eql' works for bignums."
  (let ((x (+ most-positive-fixnum 1))
        (y (+ most-positive-fixnum 1)))
    (should (eq x x))
    (should (eql x y))
    (should (equal x y))
    (should-not (eql x 0.0e+NaN))
    (should (memql x (list y)))))

(ert-deftest test-bignum-hash ()
  "Test that hash tables work for bignums."
  ;; Make two bignums that are eql but not eq.
  (let ((b1 (1+ most-positive-fixnum))
        (b2 (1+ most-positive-fixnum)))
    (dolist (test '(eq eql equal))
      (let ((hash (make-hash-table :test test)))
        (puthash b1 t hash)
        (should (eq (gethash b2 hash)
                    (funcall test b1 b2)))))))

(ert-deftest test-nthcdr-simple ()
  (should (eq (nthcdr 0 'x) 'x))
  (should (eq (nthcdr 1 '(x . y)) 'y))
  (should (eq (nthcdr 2 '(x y . z)) 'z)))

(ert-deftest test-nthcdr-circular ()
  (dolist (len '(1 2 5 37 120 997 1024))
    (let ((cycle (make-list len nil)))
      (setcdr (last cycle) cycle)
      (dolist (n (list (1- most-negative-fixnum) most-negative-fixnum
                       -1 0 1
                       (1- len) len (1+ len)
                       most-positive-fixnum (1+ most-positive-fixnum)
                       (* 2 most-positive-fixnum)
                       (* most-positive-fixnum most-positive-fixnum)
                       (ash 1 12345)))
        (let ((a (nthcdr n cycle))
              (b (if (<= n 0) cycle (nthcdr (mod n len) cycle))))
          (should (equal (list (eq a b) n len)
                         (list t n len))))))))

(ert-deftest test-proper-list-p ()
  "Test `proper-list-p' behavior."
  (dotimes (length 4)
    ;; Proper and dotted lists.
    (let ((list (make-list length 0)))
      (should (= (proper-list-p list) length))
      (should (not (proper-list-p (nconc list 0)))))
    ;; Circular lists.
    (dotimes (n (1+ length))
      (let ((circle (make-list (1+ length) 0)))
        (should (not (proper-list-p (nconc circle (nthcdr n circle))))))))
  ;; Atoms.
  (should (not (proper-list-p 0)))
  (should (not (proper-list-p "")))
  (should (not (proper-list-p [])))
  (should (not (proper-list-p (make-bool-vector 0 nil))))
  (should (not (proper-list-p (make-symbol "a")))))

(ert-deftest test-hash-table ()
  (let ((h (make-hash-table))
        (val "anything"))
    (puthash 123 val h)
    (should (eq (gethash 123 h) val)))
  (let ((h (make-hash-table :test 'equal))
        (val "anything"))
    (puthash '("hello" 123) val h)
    (should (eq (gethash '("hello" 123) h) val))))

(ert-deftest test-hash-table-wrong-keywords ()
  (should (make-hash-table :purecopy t))      ; obsolete and ignored
  (should (make-hash-table :rehash-size 123)) ; obsolete and ignored
  (should (make-hash-table :rehash-threshold 123)) ; obsolete and ignored
  (should-error (make-hash-table :some-random-keyword 123)))

(ert-deftest test-remhash ()
  (let ((h (make-hash-table))
        (val "anything"))
    (puthash 'foo val h)
    (remhash 'foo h)
    (should-not (gethash 'foo h))))

(ert-deftest test-clrhash ()
  (let ((h (make-hash-table)))
    (puthash 'foo1 'bar1 h)
    (puthash 'foo2 'bar2 h)
    (puthash 'foo3 'bar3 h)
    (puthash 'foo4 'bar4 h)
    (clrhash h)
    (should-not (gethash 'foo h))))

(ert-deftest test-hash-table-p ()
  (let ((h (make-hash-table)))
    (should (hash-table-p h)))
  (should-not (hash-table-p 123))
  (should-not (hash-table-p "foo"))
  (should-not (hash-table-p [foo]))
  (should-not (hash-table-p (list 'foo))))

(ert-deftest test-hash-table-count ()
  (let ((h (make-hash-table)))
    (puthash 'foo1 'bar1 h)
    (should (= (hash-table-count h) 1))
    (puthash 'foo2 'bar2 h)
    (should (= (hash-table-count h) 2))
    (puthash 'foo3 'bar3 h)
    (should (= (hash-table-count h) 3))
    (puthash 'foo4 'bar4 h)
    (should (= (hash-table-count h) 4))
    (clrhash h)
    (should (= (hash-table-count h) 0))))

(ert-deftest test-maphash ()
  (let ((h (make-hash-table))
        (sum 0))
    (puthash 'foo1 1 h)
    (puthash 'foo2 22 h)
    (puthash 'foo3 333 h)
    (puthash 'foo4 4444 h)
    (maphash (lambda (_key value) (incf sum value)) h)
    (should (= sum 4800))))

(ert-deftest test-copy-hash-table ()
  (let* ((h1 (make-hash-table))
         h2)
    (puthash 'foo '(bar baz) h1)
    (setq h2 (copy-hash-table h1))
    (should-not (eq h1 h2))
    (should (equal (gethash 'foo h2) '(bar baz)))))

(ert-deftest ft-hash-table-weakness ()
  (dolist (w '(nil key value key-or-value key-and-value t))
    (let* ((h (make-hash-table :weakness w))
           (w2 (hash-table-weakness h)))
      (cond ((eq w t)
             (should (eq w2 'key-and-value)))
            (t
             (should (eq w2 w)))))))


;;; Weak hashtable tests

(defun ft--init-rng () (random "weak-hashtable-tests"))
(defun ft--nentries () 50)

(defun ft--format-component (num key? dead?)
  (format "%02d-%s-%s" num (if key? "key" "val") (if dead? "dead" "alive")))

(defun ft--parse-component (string)
  (or (string-match "^\\([0-9]+\\)-\\(key\\|val\\)-\\(dead\\|alive\\)$"
                    string)
      (error "Invalid argument: %S" string))
  (vector (string-to-number (match-string 1 string))
          (equal (match-string 2 string) "key")
          (equal (match-string 3 string) "dead")))

(defun ft--component-num (string) (aref (ft--parse-component string) 0))

(defun ft--dead-component (string key?)
  (ft--format-component (ft--component-num string) key? t))

;; Create NENTRIES pairs of strings and put them into TABLE.  Randomly
;; select a subset of the strings as "dead".  Return the list of pairs
;; where the "dead" strings are replaced the with nil.
(defun ft--populate-hashtable (table nentries)
  (let ((pairs '()))
    (dotimes (i nentries)
      (let* ((r (random 4))
             (key (cl-ecase r
                    ((0 2) (ft--format-component i t nil))
                    ((1 3) (ft--format-component i t t))))
             (val (cl-ecase r
                    ((0 1) (ft--format-component i nil nil))
                    ((2 3) (ft--format-component i nil t)))))
        (puthash key val table)
        (cl-ecase r
          (0 (push (cons key val) pairs))
          (1 (push (cons nil val) pairs))
          (2 (push (cons key nil) pairs))
          (3 ))))
    (nreverse pairs)))

(defun ft--hash-table-entries (table)
  (let ((entries '()))
    (maphash (lambda (k v) (push (cons k v) entries))
             table)
    entries))

(defun ft--check-entry (weakness key1 val1 key2 val2)
  (cl-ecase weakness
    (key
     (should (eq key1 key2))
     (cond (val1 (should (eq val1 val2)))
           (t (should (equal (ft--dead-component key1 nil)
                             val2)))))
    (value
     (should (eq val1 val2))
     (cond (key1 (should (eq key1 key2)))
           (t (should (equal (ft--dead-component val1 t)
                             key2)))))
    (key-and-value
     (should (eq key1 key2))
     (should (eq val1 val2)))
    (key-or-value
     (cond (key1 (should (eq key1 key2)))
           (t (should (equal (ft--dead-component val1 t)
                             key2))))
     (cond (val1 (should (eq val1 val2)))
           (t (should (equal (ft--dead-component key1 nil)
                             val2)))))))

(defun ft--check-entries (table pairs)
  (let* ((w (hash-table-weakness table))
         (expected (cl-ecase w
                     (key (cl-remove nil pairs :key #'car))
                     (value (cl-remove nil pairs :key #'cdr))
                     (key-and-value
                      (cl-remove-if (lambda (e)
                                      (not (and (car e) (cdr e))))
                                    pairs))
                     (key-or-value
                      (cl-remove-if (lambda (e)
                                      (not (or (car e) (cdr e))))
                                    pairs))))
         (actual (sort (ft--hash-table-entries table)
                       :key #'car :lessp #'string<)))
    (cl-loop for (k1 . v1) in expected
             for (k2 . v2) in actual
             do (ft--check-entry w k1 v1 k2 v2))
    (should (= (length expected) (length actual)))))

(defun ft--gc () (garbage-collect))

;; Test that weakly held objects are no longer in a hash table after a
;; GC cycle.
(defun ft--test-weak-removal (weakness)
  ;; Use a separate thread to avoid stray references on the stack.
  (unless (featurep 'threads)
    (ert-skip '(not (featurep 'threads))))
  (let* ((_ (ft--init-rng))
         (table (make-hash-table :weakness weakness))
         (f (lambda () (ft--populate-hashtable table (ft--nentries))))
         (pairs (thread-join (make-thread f))))
    (ft--gc)
    (ft--check-entries table pairs)))

(ert-deftest ft-weak-key-removal () (ft--test-weak-removal 'key))
(ert-deftest ft-weak-value-removal () (ft--test-weak-removal 'value))
(ert-deftest ft-weak-and-removal () (ft--test-weak-removal 'key-and-value))
(ert-deftest ft-weak-or-removal () (ft--test-weak-removal 'key-or-value))

(defun ft--test-puthash (weakness)
  (let ((h (make-hash-table :weakness weakness))
        (a (string ?a))
        (b (string ?b))
        (c (string ?c)))
    (puthash a a h)
    (should (eq (gethash a h) a))
    (puthash a b h)
    (should (eq (gethash a h) b))
    (puthash a c h)
    (should (eq (gethash a h) c))))

(ert-deftest ft-puthash-weak ()
  (dolist (w '(nil key value key-and-value key-or-value))
    (ft--test-puthash w)))



(ert-deftest test-hash-function-that-mutates-hash-table ()
  (define-hash-table-test 'badeq 'eq 'bad-hash)
  (let ((h (make-hash-table :test 'badeq :size 1 :rehash-size 1)))
    (defun bad-hash (k)
      (if (eq k 100)
	  (clrhash h))
      (sxhash-eq k))
    (should-error
     (dotimes (k 200)
       (puthash k k h)))
    (should (= 100 (hash-table-count h)))))

(ert-deftest test-sxhash-equal ()
  (should (= (sxhash-equal (* most-positive-fixnum most-negative-fixnum))
	     (sxhash-equal (* most-positive-fixnum most-negative-fixnum))))
  (should (= (sxhash-equal (make-string 1000 ?a))
	     (sxhash-equal (make-string 1000 ?a))))
  (should (= (sxhash-equal (point-marker))
	     (sxhash-equal (point-marker))))
  (should (= (sxhash-equal (make-vector 1000 (make-string 10 ?a)))
	     (sxhash-equal (make-vector 1000 (make-string 10 ?a)))))
  (should (= (sxhash-equal (make-bool-vector 1000 t))
	     (sxhash-equal (make-bool-vector 1000 t))))
  (should (= (sxhash-equal (make-char-table nil (make-string 10 ?a)))
	     (sxhash-equal (make-char-table nil (make-string 10 ?a)))))
  (should (= (sxhash-equal (record 'a (make-string 10 ?a)))
	     (sxhash-equal (record 'a (make-string 10 ?a))))))

(ert-deftest fns--define-hash-table-test ()
  ;; Check that we can have two differently-named tests using the
  ;; same functions (bug#68668).
  (define-hash-table-test 'fns-tests--1 'my-cmp 'my-hash)
  (define-hash-table-test 'fns-tests--2 'my-cmp 'my-hash)
  (let ((h1 (make-hash-table :test 'fns-tests--1))
        (h2 (make-hash-table :test 'fns-tests--2)))
    (should (eq (hash-table-test h1) 'fns-tests--1))
    (should (eq (hash-table-test h2) 'fns-tests--2))))

(ert-deftest test-secure-hash ()
  (should (equal (secure-hash 'md5    "foobar")
                 "3858f62230ac3c915f300c664312c63f"))
  (should (equal (secure-hash 'sha1   "foobar")
                 "8843d7f92416211de9ebb963ff4ce28125932878"))
  (should (equal (secure-hash 'sha224 "foobar")
                 "de76c3e567fca9d246f5f8d3b2e704a38c3c5e258988ab525f941db8"))
  (should (equal (secure-hash 'sha256 "foobar")
                 (concat "c3ab8ff13720e8ad9047dd39466b3c89"
                         "74e592c2fa383d4a3960714caef0c4f2")))
  (should (equal (secure-hash 'sha384 "foobar")
                 (concat "3c9c30d9f665e74d515c842960d4a451c83a0125fd3de739"
                         "2d7b37231af10c72ea58aedfcdf89a5765bf902af93ecf06")))
  (should (equal (secure-hash 'sha512 "foobar")
                 (concat "0a50261ebd1a390fed2bf326f2673c145582a6342d5"
                         "23204973d0219337f81616a8069b012587cf5635f69"
                         "25f1b56c360230c19b273500ee013e030601bf2425")))
  ;; Test that a call to getrandom returns the right format.
  ;; This does not test randomness; it's merely a format check.
  (should (string-match "\\`[0-9a-f]\\{128\\}\\'"
                        (secure-hash 'sha512 'iv-auto 100))))

(ert-deftest test-vector-delete ()
  (let ((v1 (make-vector 1000 1)))
    (should (equal (delete t (vector nil t)) [nil]))
    (should (equal (delete 1 v1) (vector)))
    (should (equal (delete 2 v1) v1))))

(ert-deftest string-search ()
  (should (equal (string-search "zot" "foobarzot") 6))
  (should (equal (string-search "foo" "foobarzot") 0))
  (should (not (string-search "fooz" "foobarzot")))
  (should (not (string-search "zot" "foobarzo")))
  (should (equal (string-search "ab" "ab") 0))
  (should (equal (string-search "ab\0" "ab") nil))
  (should (equal (string-search "ab" "abababab" 3) 4))
  (should (equal (string-search "ab" "ababac" 3) nil))
  (should (equal (string-search "aaa" "aa") nil))
  (let ((case-fold-search t))
    (should (equal (string-search "ab" "AB") nil)))

  (should (equal
           (string-search (make-string 2 130)
	                  (concat "helló" (make-string 5 130 t) "bár"))
           5))
  (should (equal
           (string-search (make-string 2 127)
	                  (concat "helló" (make-string 5 127 t) "bár"))
           5))

  (should (equal (string-search "\377" "a\377ø") 1))
  (should (equal (string-search "\377" "a\377a") 1))

  (should (not (string-search (make-string 1 255) "a\377ø")))
  (should (not (string-search (make-string 1 255) "a\377a")))

  (should (equal (string-search "fóo" "zotfóo") 3))

  (should (equal (string-search (string-to-multibyte "\377") "ab\377c") 2))
  (should (equal (string-search "\303" "aøb") nil))
  (should (equal (string-search "\270" "aøb") nil))
  (should (equal (string-search "ø" "\303\270") nil))
  (should (equal (string-search "ø" (make-string 32 ?a)) nil))
  (should (equal (string-search "ø" (string-to-multibyte (make-string 32 ?a)))
                 nil))
  (should (equal (string-search "o" (string-to-multibyte
                                     (apply #'string
                                            (number-sequence ?a ?z))))
                 14))

  (should (equal (string-search "a\U00010f98z" "a\U00010f98a\U00010f98z") 2))

  (should-error (string-search "a" "abc" -1))
  (should-error (string-search "a" "abc" 4))
  (should-error (string-search "a" "abc" 100000000000))

  (should (equal (string-search "a" "aaa" 3) nil))
  (should (equal (string-search "aa" "aa" 1) nil))
  (should (equal (string-search "\0" "") nil))

  (should (equal (string-search "" "") 0))
  (should-error (string-search "" "" 1))
  (should (equal (string-search "" "abc") 0))
  (should (equal (string-search "" "abc" 2) 2))
  (should (equal (string-search "" "abc" 3) 3))
  (should-error (string-search "" "abc" 4))
  (should-error (string-search "" "abc" -1))

  (should-not (string-search "ø" "foo\303\270"))
  (should-not (string-search "\303\270" "ø"))
  (should-not (string-search "\370" "ø"))
  (should-not (string-search (string-to-multibyte "\370") "ø"))
  (should-not (string-search "ø" "\370"))
  (should-not (string-search "ø" (string-to-multibyte "\370")))
  (should-not (string-search "\303\270" "\370"))
  (should-not (string-search (string-to-multibyte "\303\270") "\370"))
  (should-not (string-search "\303\270" (string-to-multibyte "\370")))
  (should-not (string-search (string-to-multibyte "\303\270")
                             (string-to-multibyte "\370")))
  (should-not (string-search "\370" "\303\270"))
  (should-not (string-search (string-to-multibyte "\370") "\303\270"))
  (should-not (string-search "\370" (string-to-multibyte "\303\270")))
  (should-not (string-search (string-to-multibyte "\370")
                             (string-to-multibyte "\303\270")))
  (should (equal (string-search (string-to-multibyte "o\303\270") "foo\303\270")
                 2))
  (should (equal (string-search "\303\270" "foo\303\270") 3)))

(ert-deftest object-intervals ()
  (should (equal (object-intervals (propertize "foo" 'bar 'zot))
                 '((0 3 (bar zot)))))
  (should (equal (object-intervals (concat (propertize "foo" 'bar 'zot)
                                           (propertize "foo" 'gazonk "gazonk")))
                 '((0 3 (bar zot)) (3 6 (gazonk "gazonk")))))
  (should (equal
           (with-temp-buffer
             (insert "foobar")
             (put-text-property 1 3 'foo 1)
             (put-text-property 3 6 'bar 2)
             (put-text-property 2 5 'zot 3)
             (object-intervals (current-buffer)))
           '((0 1 (foo 1)) (1 2 (zot 3 foo 1)) (2 4 (zot 3 bar 2))
             (4 5 (bar 2)) (5 6 nil)))))

(ert-deftest length-equals-tests ()
  (should-not (length< (list 1 2 3) 2))
  (should-not (length< (list 1 2 3) 3))
  (should (length< (list 1 2 3) 4))

  (should-not (length< "abc" 2))
  (should-not (length< "abc" 3))
  (should (length< "abc" 4))

  (should (length> (list 1 2 3) 2))
  (should-not (length> (list 1 2 3) 3))
  (should-not (length> (list 1 2 3) 4))

  (should (length> "abc" 2))
  (should-not (length> "abc" 3))
  (should-not (length> "abc" 4))

  (should-not (length= (list 1 2 3) 2))
  (should (length= (list 1 2 3) 3))
  (should-not (length= (list 1 2 3) 4))

  (should-not (length= "abc" 2))
  (should (length= "abc" 3))
  (should-not (length= "abc" 4))

  (should-not (length< (list 1 2 3) -1))
  (should-not (length< (list 1 2 3) 0))
  (should-not (length< (list 1 2 3) -10))

  (should (length> (list 1 2 3) -1))
  (should (length> (list 1 2 3) 0))

  (should-not (length= (list 1 2 3) -1))
  (should-not (length= (list 1 2 3) 0))
  (should-not (length= (list 1 2 3) 1))

  (should-error
   (let ((list (list 1)))
     (setcdr list list)
     (length< list #x1fffe))))

(defun approx-equal (list1 list2)
  (and (equal (length list1) (length list2))
       (cl-loop for v1 in list1
                for v2 in list2
                when (not (or (= v1 v2)
                              (< (abs (- v1 v2)) 0.1)))
                return nil
                finally return t)))

(ert-deftest test-buffer-line-stats-nogap ()
  (with-temp-buffer
    (insert "")
    (should (approx-equal (buffer-line-statistics) '(0 0 0))))
  (with-temp-buffer
    (insert "123\n")
    (should (approx-equal (buffer-line-statistics) '(1 3 3))))
  (with-temp-buffer
    (insert "123\n12345\n123\n")
    (should (approx-equal (buffer-line-statistics) '(3 5 3.66))))
  (with-temp-buffer
    (insert "123\n12345\n123")
    (should (approx-equal (buffer-line-statistics) '(3 5 3.66))))
  (with-temp-buffer
    (insert "123\n12345")
    (should (approx-equal (buffer-line-statistics) '(2 5 4))))

  (with-temp-buffer
    (insert "123\n12é45\n123\n")
    (should (approx-equal (buffer-line-statistics) '(3 6 4))))

  (with-temp-buffer
    (insert "\n\n\n")
    (should (approx-equal (buffer-line-statistics) '(3 0 0)))))

(ert-deftest test-buffer-line-stats-gap ()
  (with-temp-buffer
    (dotimes (_ 1000)
      (insert "12345678901234567890123456789012345678901234567890\n"))
    (goto-char (point-min))
    ;; This should make a gap appear.
    (insert "123\n")
    (delete-region (point-min) (point))
    (should (approx-equal (buffer-line-statistics) '(1000 50 50.0))))
  (with-temp-buffer
    (dotimes (_ 1000)
      (insert "12345678901234567890123456789012345678901234567890\n"))
    (goto-char (point-min))
    (insert "123\n")
    (should (approx-equal (buffer-line-statistics) '(1001 50 49.9))))
  (with-temp-buffer
    (dotimes (_ 1000)
      (insert "12345678901234567890123456789012345678901234567890\n"))
    (goto-char (point-min))
    (insert "123\n")
    (goto-char (point-max))
    (insert "fóo")
    (should (approx-equal (buffer-line-statistics) '(1002 50 49.9)))))

(ert-deftest test-line-number-at-position ()
  (with-temp-buffer
    (insert (make-string 10 ?\n))
    (should (= (line-number-at-pos (point)) 11))
    (should (= (line-number-at-pos nil) 11))
    (should-error (line-number-at-pos -1))
    (should-error (line-number-at-pos 100))))

(defun fns-tests-concat (&rest args)
  ;; Dodge the byte-compiler's partial evaluation of `concat' with
  ;; constant arguments.
  (apply #'concat args))

(ert-deftest fns-concat ()
  (should (equal (fns-tests-concat) ""))
  (should (equal (fns-tests-concat "") ""))
  (should (equal (fns-tests-concat nil) ""))
  (should (equal (fns-tests-concat []) ""))
  (should (equal (fns-tests-concat [97 98]) "ab"))
  (should (equal (fns-tests-concat '(97 98)) "ab"))
  (should (equal (fns-tests-concat "ab" '(99 100) nil [101 102] "gh")
                 "abcdefgh"))
  (should (equal (fns-tests-concat "Ab" "\200" "cd") "Ab\200cd"))
  (should (equal (fns-tests-concat "aB" "\200" "çd") "aB\200çd"))
  (should (equal (fns-tests-concat "AB" (string-to-multibyte "\200") "cd")
                 (string-to-multibyte "AB\200cd")))
  (should (equal (fns-tests-concat "ab" '(#xe5) [255] "cd") "abåÿcd"))
  (should (equal (fns-tests-concat '(#x3fffff) [#x3fff80] "xy") "\377\200xy"))
  (should (equal (fns-tests-concat '(#x3fffff) [#x3fff80] "xy§") "\377\200xy§"))
  (should (equal-including-properties
           (fns-tests-concat #("abc" 0 3 (a 1)) #("de" 0 2 (a 1)))
           #("abcde" 0 5 (a 1))))
  (should (equal-including-properties
           (fns-tests-concat #("abc" 0 3 (a 1)) "§ü" #("çå" 0 2 (b 2)))
           #("abc§üçå" 0 3 (a 1) 5 7 (b 2))))
  (should-error (fns-tests-concat "a" '(98 . 99))
                :type 'wrong-type-argument)
  (let ((loop (list 66 67)))
    (setcdr (cdr loop) loop)
    (should-error (fns-tests-concat "A" loop)
                  :type 'circular-list)))

(ert-deftest fns-vconcat ()
  (should (equal (vconcat) []))
  (should (equal (vconcat nil) []))
  (should (equal (vconcat "") []))
  (should (equal (vconcat [1 2 3]) [1 2 3]))
  (should (equal (vconcat '(1 2 3)) [1 2 3]))
  (should (equal (vconcat "ABC") [65 66 67]))
  (should (equal (vconcat "ü§") [252 167]))
  (should (equal (vconcat [1 2 3] nil '(4 5) "AB" "å"
                          "\377" (string-to-multibyte "\377")
                          (bool-vector t nil nil t nil))
                 [1 2 3 4 5 65 66 #xe5 255 #x3fffff t nil nil t nil]))
  (should-error (vconcat [1] '(2 . 3))
                :type 'wrong-type-argument)
  (let ((loop (list 1 2)))
    (setcdr (cdr loop) loop)
    (should-error (vconcat [1] loop)
                  :type 'circular-list)))

(ert-deftest fns-append ()
  (should (equal (append) nil))
  (should (equal (append 'tail) 'tail))
  (should (equal (append [1 2 3] nil '(4 5) "AB" "å"
                         "\377" (string-to-multibyte "\377")
                          (bool-vector t nil nil t nil)
                         '(9 10))
                 '(1 2 3 4 5 65 66 #xe5 255 #x3fffff t nil nil t nil 9 10)))
  (should (equal (append '(1 2) '(3 4) 'tail)
                 '(1 2 3 4 . tail)))
  (should-error (append '(1 . 2) '(3))
                :type 'wrong-type-argument)
  (let ((loop (list 1 2)))
    (setcdr (cdr loop) loop)
    (should-error (append loop '(end))
                  :type 'circular-list)))

(ert-deftest fns--string-to-unibyte-multibyte ()
  (dolist (str (list "" "a" "abc" "a\x00\x7fz" "a\xaa\xbbz" "\x80\xdd\xff"
                     (apply #'unibyte-string (number-sequence 0 255))))
    (ert-info ((prin1-to-string str) :prefix "str: ")
      (should-not (multibyte-string-p str))
      (let* ((u (string-to-unibyte str))   ; should be identity
             (m (string-to-multibyte u))   ; lossless conversion
             (mm (string-to-multibyte m))  ; should be identity
             (uu (string-to-unibyte m))    ; also lossless
             (ml (mapcar (lambda (c) (if (<= c #x7f) c (+ c #x3fff00))) u)))
        (should-not (multibyte-string-p u))
        (should (multibyte-string-p m))
        (should (multibyte-string-p mm))
        (should-not (multibyte-string-p uu))
        (should (equal str u))
        (should (equal m mm))
        (should (equal str uu))
        (should (equal (append m nil) ml)))))
  (should-error (string-to-unibyte "å"))
  (should-error (string-to-unibyte "ABC∀BC")))

(defun fns-tests--take-ref (n list)
  "Reference implementation of `take'."
  (named-let loop ((m n) (tail list) (ac nil))
    (if (and (> m 0) tail)
        (loop (1- m) (cdr tail) (cons (car tail) ac))
      (nreverse ac))))

(ert-deftest fns--take-ntake ()
  "Test `take' and `ntake'."
  ;; Check errors and edge cases.
  (should-error (take 'x '(a)))
  (should-error (ntake 'x '(a)))
  (should-error (take 1 'a))
  (should-error (ntake 1 'a))
  (should-error (take 2 '(a . b)))
  (should-error (ntake 2 '(a . b)))
  ;; Tolerate non-lists for a count of zero.
  (should (equal (take 0 'a) nil))
  (should (equal (ntake 0 'a) nil))
  ;; But not non-numbers for empty lists.
  (should-error (take 'x nil))
  (should-error (ntake 'x nil))

  (dolist (list '(nil (a) (a b) (a b c) (a b c d) (a . b) (a b . c)))
    (ert-info ((prin1-to-string list) :prefix "list: ")
      (let ((max (if (proper-list-p list)
                     (+ 2 (length list))
                   (safe-length list))))
        (dolist (n (number-sequence -1 max))
          (ert-info ((prin1-to-string n) :prefix "n: ")
            (let* ((l (copy-tree list))
                   (ref (fns-tests--take-ref n l)))
              (should (equal (take n l) ref))
              (should (equal l list))
              (should (equal (ntake n l) ref))))))))

  ;; Circular list.
  (let ((list (list 'a 'b 'c)))
    (setcdr (nthcdr 2 list) (cdr list)) ; list now (a b c b c b c ...)
    (should (equal (take 0 list) nil))
    (should (equal (take 1 list) '(a)))
    (should (equal (take 2 list) '(a b)))
    (should (equal (take 3 list) '(a b c)))
    (should (equal (take 4 list) '(a b c b)))
    (should (equal (take 5 list) '(a b c b c)))
    (should (equal (take 10 list) '(a b c b c b c b c b)))

    (should (equal (ntake 10 list) '(a b))))

  ;; Bignum N argument.
  (let ((list (list 'a 'b 'c)))
    (should (equal (take (+ most-positive-fixnum 1) list) '(a b c)))
    (should (equal (take (- most-negative-fixnum 1) list) nil))
    (should (equal (ntake (+ most-positive-fixnum 1) list) '(a b c)))
    (should (equal (ntake (- most-negative-fixnum 1) list) nil))
    (should (equal list '(a b c)))))

(ert-deftest fns--copy-alist ()
  (dolist (orig '(nil
                  ((a . 1) (b . 2) (a . 3))
                  (a (b . 3) ((c) (d)))))
    (ert-info ((prin1-to-string orig) :prefix "orig: ")
      (let ((copy (copy-alist orig)))
        (should (equal orig copy))
        (while orig
          (should-not (eq orig copy))
          ;; Check that cons pairs are copied but nothing else.
          (let ((orig-elt (car orig))
                (copy-elt (car copy)))
            (if (atom orig-elt)
                (should (eq orig-elt copy-elt))
              (should-not (eq orig-elt copy-elt))
              (should (eq (car orig-elt) (car copy-elt)))
              (should (eq (cdr orig-elt) (cdr copy-elt)))))
          (setq orig (cdr orig))
          (setq copy (cdr copy))))))

  (should-error (copy-alist 'a)
                :type 'wrong-type-argument)
  (should-error (copy-alist [(a . 1) (b . 2) (a . 3)])
                :type 'wrong-type-argument)
  (should-error (copy-alist "abc")
                :type 'wrong-type-argument))

(ert-deftest fns-value<-ordered ()
  ;; values (X . Y) where X<Y
  (let* ((big (* 10 most-positive-fixnum))
         (buf1 (get-buffer-create " *one*"))
         (buf2 (get-buffer-create " *two*"))
         (buf3 (get-buffer-create " *three*"))
         (_ (progn (with-current-buffer buf1 (insert (make-string 20 ?a)))
                   (with-current-buffer buf2 (insert (make-string 20 ?b)))))
         (mark1 (set-marker (make-marker) 12 buf1))
         (mark2 (set-marker (make-marker) 13 buf1))
         (mark3 (set-marker (make-marker) 12 buf2))
         (mark4 (set-marker (make-marker) 13 buf2))
         (proc1 (make-pipe-process :name " *proc one*"))
         (proc2 (make-pipe-process :name " *proc two*")))
    (kill-buffer buf3)
    (unwind-protect
        (dolist (c
                 `(
                   ;; fixnums
                   (1 . 2)  (-2 . -1) (-2 . 1) (-1 . 2)
                   ;; bignums
                   (,big . ,(1+ big)) (,(- big) . ,big)
                   (,(- -1 big) . ,(- big))
                   ;; fixnums/bignums
                   (1 . ,big) (-1 . ,big) (,(- big) . -1) (,(- big) . 1)
                   ;; floats
                   (1.5 . 1.6) (-1.3 . -1.2) (-13.0 . 12.0)
                   ;; floats/fixnums
                   (1 . 1.1) (1.9 . 2) (-2.0 . 1) (-2 . 1.0)
                   ;; fixnums that can't be represented as floats
                   (72057594037927935 . 72057594037927936.0)
                   (72057594037927936.0 . 72057594037927937)
                   (-72057594037927936.0 . -72057594037927935)
                   (-72057594037927937 . -72057594037927936.0)
                   (2305843009213693951 . 2305843009213693952.0)

                   ;; floats/bignums
                   (,big . ,(float (* 2 big))) (,(float big) . ,(* 2 big))
                   ;; symbols
                   (a . b) (nil . nix) (b . ba) (## . a) (A . a)
                   (#:a . #:b) (a . #:b) (#:a . b)
                   ;; strings
                   ("" . "a") ("a" . "b") ("A" . "a") ("abc" . "abd")
                   ("b" . "ba")
                   ;; strings again, but in a context where 3-way comparison
                   ;; matters
                   (("" . 2) . ("a" . 1))
                   (("å" . 2) . ("åü" . 1))
                   (("a" . 2) . ("aå" . 1))
                   (("\x80" . 2) . ("\x80å" . 1))

                   ;; lists
                   ((1 2 3) . (2 3 4)) ((2) . (2 1)) (() . (0))
                   ((1 2 3) . (1 3)) ((1 2 3) . (1 3 2))
                   (((b a) (c d) e) . ((b a) (c d) f))
                   (((b a) (c D) e) . ((b a) (c d) e))
                   (((b a) (c d () x) e) . ((b a) (c d (1) x) e))
                   ((1 . 2) . (1 . 3)) ((1 2 . 3) . (1 2 . 4))

                   ;; vectors
                   ([1 2 3] . [2 3 4]) ([2] . [2 1]) ([] . [0])
                   ([1 2 3] . [1 3]) ([1 2 3] . [1 3 2])
                   ([[b a] [c d] e] . [[b a] [c d] f])
                   ([[b a] [c D] e] . [[b a] [c d] e])
                   ([[b a] [c d [] x] e] . [[b a] [c d [1] x] e])

                   ;; bool-vectors
                   (,(bool-vector) . ,(bool-vector nil))
                   (,(bool-vector nil) . ,(bool-vector t))
                   (,(bool-vector t nil t nil) . ,(bool-vector t nil t t))
                   (,(bool-vector t nil t) . ,(bool-vector t nil t nil))

                   ;; records
                   (#s(a 2 3) . #s(b 3 4)) (#s(b) . #s(b a))
                   (#s(a 2 3) . #s(a 3)) (#s(a 2 3) . #s(a 3 2))
                   (#s(#s(b a) #s(c d) e) . #s(#s(b a) #s(c d) f))
                   (#s(#s(b a) #s(c D) e) . #s(#s(b a) #s(c d) e))
                   (#s(#s(b a) #s(c d #s(u) x) e)
                    . #s(#s(b a) #s(c d #s(v) x) e))

                   ;; markers
                   (,mark1 . ,mark2) (,mark1 . ,mark3) (,mark1 . ,mark4)
                   (,mark2 . ,mark3) (,mark2 . ,mark4) (,mark3 . ,mark4)

                   ;; buffers
                   (,buf1 . ,buf2) (,buf3 . ,buf1) (,buf3 . ,buf2)

                   ;; processes
                   (,proc1 . ,proc2)
                   ))
          (let ((x (car c))
                (y (cdr c)))
            (should (value< x y))
            (should-not (value< y x))
            (should-not (value< x x))
            (should-not (value< y y))
            (should (value< (vector x 2) (vector y 1)))
            (should-not (value< (vector y 1) (vector x 2)))
            (should (value< (vector x 1) (vector x 2)))
            (should (value< (vector y 1) (vector y 2)))))

      (delete-process proc2)
      (delete-process proc1)
      (kill-buffer buf2)
      (kill-buffer buf1))))

(ert-deftest fns-value<-unordered ()
  ;; values (X . Y) where neither X<Y nor Y<X

  (let ((buf1 (get-buffer-create " *one*"))
        (buf2 (get-buffer-create " *two*")))
    (kill-buffer buf2)
    (kill-buffer buf1)
    (dolist (c `(
                 ;; numbers
                 (0 . 0.0) (0 . -0.0) (0.0 . -0.0)

                 (72057594037927936 . 72057594037927936.0)
                 (1 . 0.0e+NaN)

                 ;; symbols
                 (a . #:a)

                 ;; (dead) buffers
                 (,buf1 . ,buf2)

                 ;; unordered types
                 (,(make-hash-table) . ,(make-hash-table))
                 (,(obarray-make) . ,(obarray-make))
                 ;; FIXME: more?
                 ))
      (let ((x (car c))
            (y (cdr c)))
        (should-not (value< x y))
        (should-not (value< y x))
        (should (value< (cons x 1) (cons y 2)))
        (should-not (value< (cons x 2) (cons y 1)))))))

(ert-deftest fns-value<-type-mismatch ()
  ;; values of disjoint (incomparable) types
  (let ((incomparable
         `( 1 a "a" (a b) [a b] ,(bool-vector nil t) #s(a b)
            ,(make-char-table 'test)
            ,(make-hash-table)
            ,(obarray-make)
            ;; FIXME: more?
            )))
    (let ((tail incomparable))
      (while tail
        (let ((x (car tail)))
          (dolist (y (cdr tail))
            (should-error (value< x y) :type 'type-mismatch)
            (should-error (value< y x) :type 'type-mismatch)))
        (setq tail (cdr tail))))))

(ert-deftest fns-value<-symbol-with-pos ()
  ;; values (X . Y) where X<Y
  (let* ((a-sp-1 (position-symbol 'a 1))
         (a-sp-2 (position-symbol 'a 2))
         (b-sp-1 (position-symbol 'b 1))
         (b-sp-2 (position-symbol 'b 2)))

    (dolist (swp '(nil t))
      (let ((symbols-with-pos-enabled swp))
        ;; Enabled or not, they compare by name.
        (dolist (c `((,a-sp-1 . ,b-sp-1) (,a-sp-1 . ,b-sp-2)
                     (,a-sp-2 . ,b-sp-1) (,a-sp-2 . ,b-sp-2)))
          (let ((x (car c))
                (y (cdr c)))
            (should (value< x y))
            (should-not (value< y x))
            (should-not (value< x x))
            (should-not (value< y y))))
        (should-not (value< a-sp-1 a-sp-2))
        (should-not (value< a-sp-2 a-sp-1))))

    ;; When disabled, symbol-with-pos and symbols do not compare.
    (should-error (value< a-sp-1 'a) :type 'type-mismatch)
    (should-error (value< 'a a-sp-1) :type 'type-mismatch)

    (let ((symbols-with-pos-enabled t))
      ;; When enabled, a symbol-with-pos compares as a plain symbol.
      (dolist (c `((,a-sp-1 . b) (a . ,b-sp-1)))
        (let ((x (car c))
              (y (cdr c)))
          (should (value< x y))
          (should-not (value< y x))
          (should-not (value< x x))
          (should-not (value< y y))))
      (should-not (value< a-sp-1 'a))
      (should-not (value< 'a a-sp-1)))))

(ert-deftest fns-value<-circle ()
  ;; Check that we at least don't hang when comparing two circular lists.
  (let ((a (number-sequence 1 5))
        (b (number-sequence 1 5)))
    (setcdr (last a) (nthcdr 2 a))
    (setcdr (last b) (nthcdr 2 b))
    (should-error (value< a b :type 'circular))
    (should-error (value< b a :type 'circular))))

(ert-deftest fns-value<-bool-vector ()
  ;; More thorough test of `value<' for bool-vectors.
  (random "my seed")
  (dolist (na '(0 1 5 8 9 32 63 64 65 200 1001 1024))
    (let ((a (make-bool-vector na nil)))
      (dotimes (i na)
        (aset a i (zerop (random 2))))
      (dolist (nb '(0 1 5 8 9 32 63 64 65 200 1001 1024))
        (when (<= nb na)
          (let ((b (make-bool-vector nb nil)))
            (dotimes (i nb)
              (aset b i (aref a i)))
            ;; `b' is now a prefix of `a'.
            (should-not (value< a b))
            (cond ((= nb na)
                   (should (equal a b))
                   (should-not (value< b a)))
                  (t
                   (should-not (equal a b))
                   (should (value< b a))))
            (unless (zerop nb)
              ;; Flip random bits in `b' and check how it affects the order.
              (dotimes (_ 3)
                (let ((i (random nb)))
                  (let ((val (aref b i)))
                    (aset b i (not val))
                    (should-not (equal a b))
                    (cond
                     (val
                      ;; t -> nil: `b' is now always a proper prefix of `a'.
                      (should-not (value< a b))
                      (should (value< b a)))
                     (t
                      ;; nil -> t: `a' is now less than `b'.
                      (should (value< a b))
                      (should-not (value< b a))))
                    ;; Undo the flip.
                    (aset b i val)))))))))))

;; Local Variables:
;; read-symbol-shorthands: (("ft-" . "fns-tests-"))
;; End:

;;; fns-tests.el ends here
