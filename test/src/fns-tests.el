;;; fns-tests.el --- tests for src/fns.c

;; Copyright (C) 2014-2018 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `https://www.gnu.org/licenses/'.

;;; Commentary:

;;; Code:

(require 'cl-lib)

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
  (should (equal (nreverse "xyzzy") "yzzyx"))
  (let ((A []))
    (nreverse A)
    (should (equal A [])))
  (let ((A [0]))
    (nreverse A)
    (should (equal A [0])))
  (let ((A [1 2 3 4]))
    (nreverse A)
    (should (equal A [4 3 2 1])))
  (let ((A [1 2 3 4]))
    (nreverse A)
    (nreverse A)
    (should (equal A [1 2 3 4])))
  (let* ((A [1 2 3 4])
	 (B (nreverse (nreverse A))))
    (should (equal A B))))

(ert-deftest fns-tests-reverse-bool-vector ()
  (let ((A (make-bool-vector 10 nil)))
    (dotimes (i 5) (aset A i t))
    (should (equal [nil nil nil nil nil t t t t t] (vconcat (reverse A))))
    (should (equal A (reverse (reverse A))))))

(ert-deftest fns-tests-nreverse-bool-vector ()
  (let ((A (make-bool-vector 10 nil)))
    (dotimes (i 5) (aset A i t))
    (nreverse A)
    (should (equal [nil nil nil nil nil t t t t t] (vconcat A)))
    (should (equal [t t t t t nil nil nil nil nil] (vconcat (nreverse A))))))

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
  ;; In a language specific locale, collation order is different.
  (should (string-collate-lessp
	   "xyzzy" "XYZZY"
	   (if (eq system-type 'windows-nt) "enu_USA" "en_US.UTF-8")))

  ;; Ignore case.
  (should (string-collate-equalp "xyzzy" "XYZZY" nil t))

  ;; Locale must be valid.
  (should-error (string-collate-equalp "xyzzy" "xyzzy" "en_DE.UTF-8")))

;; There must be a check for valid codepoints.  (Check not implemented yet)
;  (should-error
;   (string-collate-equalp (string ?\x00110000) (string ?\x00110000)))
;; Invalid UTF-8 sequences shall be indicated.  How to create such strings?

(ert-deftest fns-tests-sort ()
  (should (equal (sort '(9 5 2 -1 5 3 8 7 4) (lambda (x y) (< x y)))
		 '(-1 2 3 4 5 5 7 8 9)))
  (should (equal (sort '(9 5 2 -1 5 3 8 7 4) (lambda (x y) (> x y)))
		 '(9 8 7 5 5 4 3 2 -1)))
  (should (equal (sort '[9 5 2 -1 5 3 8 7 4] (lambda (x y) (< x y)))
		 [-1 2 3 4 5 5 7 8 9]))
  (should (equal (sort '[9 5 2 -1 5 3 8 7 4] (lambda (x y) (> x y)))
		 [9 8 7 5 5 4 3 2 -1]))
  (should (equal
	   (sort
	    (vector
	     '(8 . "xxx") '(9 . "aaa") '(8 . "bbb") '(9 . "zzz")
	     '(9 . "ppp") '(8 . "ttt") '(8 . "eee") '(9 . "fff"))
	    (lambda (x y) (< (car x) (car y))))
	   [(8 . "xxx") (8 . "bbb") (8 . "ttt") (8 . "eee")
	    (9 . "aaa") (9 . "zzz") (9 . "ppp") (9 . "fff")])))

(ert-deftest fns-tests-collate-sort ()
  ;; See https://lists.gnu.org/r/emacs-devel/2015-10/msg02505.html.
  :expected-result (if (eq system-type 'cygwin) :failed :passed)
  (skip-unless (fns-tests--collate-enabled-p))

  ;; Punctuation and whitespace characters are relevant for POSIX.
  (should
   (equal
    (sort '("11" "12" "1 1" "1 2" "1.1" "1.2")
	  (lambda (a b) (string-collate-lessp a b "POSIX")))
    '("1 1" "1 2" "1.1" "1.2" "11" "12")))
  ;; Punctuation and whitespace characters are not taken into account
  ;; for collation in other locales.
  (should
   (equal
    (sort '("11" "12" "1 1" "1 2" "1.1" "1.2")
	  (lambda (a b)
	    (let ((w32-collate-ignore-punctuation t))
	      (string-collate-lessp
	       a b (if (eq system-type 'windows-nt) "enu_USA" "en_US.UTF-8")))))
    '("11" "1 1" "1.1" "12" "1 2" "1.2")))

  ;; Diacritics are different letters for POSIX, they sort lexicographical.
  (should
   (equal
    (sort '("Ævar" "Agustín" "Adrian" "Eli")
	  (lambda (a b) (string-collate-lessp a b "POSIX")))
    '("Adrian" "Agustín" "Eli" "Ævar")))
  ;; Diacritics are sorted between similar letters for other locales.
  (should
   (equal
    (sort '("Ævar" "Agustín" "Adrian" "Eli")
	  (lambda (a b)
	    (let ((w32-collate-ignore-punctuation t))
	      (string-collate-lessp
	       a b (if (eq system-type 'windows-nt) "enu_USA" "en_US.UTF-8")))))
    '("Adrian" "Ævar" "Agustín" "Eli"))))

(ert-deftest fns-tests-string-version-lessp ()
  (should (string-version-lessp "foo2.png" "foo12.png"))
  (should (not (string-version-lessp "foo12.png" "foo2.png")))
  (should (string-version-lessp "foo12.png" "foo20000.png"))
  (should (not (string-version-lessp "foo20000.png" "foo12.png")))
  (should (string-version-lessp "foo.png" "foo2.png"))
  (should (not (string-version-lessp "foo2.png" "foo.png")))
  (should (equal (sort '("foo12.png" "foo2.png" "foo1.png")
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
  (should (equal (func-arity (lambda (&rest x))) '(0 . many)))
  (should (equal (func-arity (eval (lambda (x &optional y)) nil)) '(1 . 2)))
  (should (equal (func-arity (eval (lambda (x &optional y)) t)) '(1 . 2)))
  (should (equal (func-arity 'let) '(1 . unevalled))))

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
                   (backward-delete-char 1)
                   (buffer-hash))
                 (sha1 "foo"))))

(ert-deftest fns-tests-mapcan ()
  (should-error (mapcan))
  (should-error (mapcan #'identity))
  (should-error (mapcan #'identity (make-char-table 'foo)))
  (should (equal (mapcan #'list '(1 2 3)) '(1 2 3)))
  ;; `mapcan' is destructive
  (let ((data '((foo) (bar))))
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

(ert-deftest test-cycle-lax-plist-get ()
  (let ((c1 (cyc1 1))
        (c2 (cyc2 1 2))
        (d1 (dot1 1))
        (d2 (dot2 1 2)))
    (should (lax-plist-get c1 1))
    (should (lax-plist-get c2 1))
    (should (lax-plist-get d1 1))
    (should (lax-plist-get d2 1))
    (should-error (lax-plist-get c1 2) :type 'circular-list)
    (should (lax-plist-get c2 2))
    (should-error (lax-plist-get d1 2) :type 'wrong-type-argument)
    (should (lax-plist-get d2 2))
    (should-error (lax-plist-get c1 3) :type 'circular-list)
    (should-error (lax-plist-get c2 3) :type 'circular-list)
    (should-error (lax-plist-get d1 3) :type 'wrong-type-argument)
    (should-error (lax-plist-get d2 3) :type 'wrong-type-argument)))

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

(ert-deftest test-cycle-lax-plist-put ()
  (let ((c1 (cyc1 1))
        (c2 (cyc2 1 2))
        (d1 (dot1 1))
        (d2 (dot2 1 2)))
    (should (lax-plist-put c1 1 1))
    (should (lax-plist-put c2 1 1))
    (should (lax-plist-put d1 1 1))
    (should (lax-plist-put d2 1 1))
    (should-error (lax-plist-put c1 2 2) :type 'circular-list)
    (should (lax-plist-put c2 2 2))
    (should-error (lax-plist-put d1 2 2) :type 'wrong-type-argument)
    (should (lax-plist-put d2 2 2))
    (should-error (lax-plist-put c1 3 3) :type 'circular-list)
    (should-error (lax-plist-put c2 3 3) :type 'circular-list)
    (should-error (lax-plist-put d1 3 3) :type 'wrong-type-argument)
    (should-error (lax-plist-put d2 3 3) :type 'wrong-type-argument)))

(ert-deftest test-cycle-equal ()
  (should-error (equal (cyc1 1) (cyc1 1)))
  (should-error (equal (cyc2 1 2) (cyc2 1 2))))

(ert-deftest test-cycle-nconc ()
  (should-error (nconc (cyc1 1) 'tail) :type 'circular-list)
  (should-error (nconc (cyc2 1 2) 'tail) :type 'circular-list))

(ert-deftest plist-get/odd-number-of-elements ()
  "Test that `plist-get' doesn't signal an error on degenerate plists."
  (should-not (plist-get '(:foo 1 :bar) :bar)))

(ert-deftest lax-plist-get/odd-number-of-elements ()
  "Check for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27726."
  (should (equal (should-error (lax-plist-get '(:foo 1 :bar) :bar)
                               :type 'wrong-type-argument)
                 '(wrong-type-argument plistp (:foo 1 :bar)))))

(ert-deftest plist-put/odd-number-of-elements ()
  "Check for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27726."
  (should (equal (should-error (plist-put '(:foo 1 :bar) :zot 2)
                               :type 'wrong-type-argument)
                 '(wrong-type-argument plistp (:foo 1 :bar)))))

(ert-deftest lax-plist-put/odd-number-of-elements ()
  "Check for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27726."
  (should (equal (should-error (lax-plist-put '(:foo 1 :bar) :zot 2)
                               :type 'wrong-type-argument)
                 '(wrong-type-argument plistp (:foo 1 :bar)))))

(ert-deftest plist-member/improper-list ()
  "Check for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27726."
  (should (equal (should-error (plist-member '(:foo 1 . :bar) :qux)
                               :type 'wrong-type-argument)
                 '(wrong-type-argument plistp (:foo 1 . :bar)))))

(provide 'fns-tests)
