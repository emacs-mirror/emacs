;;; casefiddle-tests.el --- tests for casefiddle.c functions -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016 Free Software Foundation, Inc.

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

(require 'case-table)
(require 'ert)

(ert-deftest casefiddle-tests-char-properties ()
  "Sanity check of character Unicode properties."
  (let ((props '(uppercase lowercase titlecase
                 special-uppercase special-lowercase special-titlecase))
        (tests '((?A nil ?a  nil  nil  nil  nil)
                 (?a ?A  nil ?A   nil  nil  nil)
                 (?Ł nil ?ł  nil  nil  nil  nil)
                 (?ł ?Ł  nil ?Ł   nil  nil  nil)

                 (?Ǆ nil ?ǆ  ?ǅ   nil  nil  nil)
                 (?ǅ ?Ǆ  ?ǆ  ?ǅ   nil  nil  nil)
                 (?ǆ ?Ǆ  nil ?ǅ   nil  nil  nil)

                 (?Σ nil ?σ  nil  nil  nil  nil)
                 (?σ ?Σ  nil ?Σ   nil  nil  nil)
                 (?ς ?Σ  nil ?Σ   nil  nil  nil)

                 (?ⅷ ?Ⅷ  nil ?Ⅷ   nil  nil  nil)
                 (?Ⅷ nil ?ⅷ  nil  nil  nil  nil)

                 (?ﬁ nil nil nil  "FI" nil "Fi")
                 (?ß nil nil nil  "SS" nil "Ss")
                 (?İ nil ?i  nil  nil "i\u0307" nil)))
        errors)
    (dolist (test tests)
      (let ((ch (car test))
            (expected (cdr test)))
        (dolist (prop props)
          (let ((got (get-char-code-property ch prop)))
            (unless (equal (car expected) got)
              (push (format "\n%c %s; expected: %s but got: %s"
                            ch prop (car expected) got)
                    errors)))
          (setq expected (cdr expected)))))
    (when errors
      (ert-fail (mapconcat (lambda (line) line) (nreverse errors) "")))))


(defconst casefiddle-tests--characters
  ;; character  uppercase  lowercase  titlecase
  '((?A ?A ?a ?A)
    (?a ?A ?a ?A)
    (?Ł ?Ł ?ł ?Ł)
    (?ł ?Ł ?ł ?Ł)

    (?Ǆ ?Ǆ ?ǆ ?ǅ)
    (?ǅ ?Ǆ ?ǆ ?ǅ)
    (?ǆ ?Ǆ ?ǆ ?ǅ)

    (?Σ ?Σ ?σ ?Σ)
    (?σ ?Σ ?σ ?Σ)
    (?ς ?Σ ?ς ?Σ)

    (?Ⅷ ?Ⅷ ?ⅷ ?Ⅷ)
    (?ⅷ ?Ⅷ ?ⅷ ?Ⅷ)))


(ert-deftest casefiddle-tests-case-table ()
  "Sanity check of down and up case tables."
  (should-not
   (let (errors
         (up (case-table-get-table (current-case-table) 'up))
         (down (case-table-get-table (current-case-table) 'down)))
     (dolist (test casefiddle-tests--characters)
       (let ((ch (car test))
             (expected (cdr test))
             (props '(uppercase lowercase))
             (tabs (list up down)))
         (while props
           (let ((got (aref (car tabs) ch)))
             (unless (equal (car expected) got)
               (push (format "\n%c %s; expected: %s but got: %s"
                             ch (car props) (car expected) got)
                     errors)))
           (setq props (cdr props) tabs (cdr tabs) expected (cdr expected)))))
     (when errors
       (mapconcat (lambda (line) line) (nreverse errors) "")))))


(ert-deftest casefiddle-tests-casing-character ()
  (should-not
   (let (errors)
     (dolist (test casefiddle-tests--characters)
       (let ((ch (car test))
             (expected (cdr test))
             (funcs '(upcase downcase capitalize)))
         (while funcs
           (let ((got (funcall (car funcs) ch)))
             (unless (equal (car expected) got)
               (push (format "\n%c %s; expected: %s but got: %s"
                             ch (car funcs) (car expected) got)
                     errors)))
           (setq funcs (cdr funcs) expected (cdr expected)))))
     (when errors
       (mapconcat (lambda (line) line) (nreverse errors) "")))))


(ert-deftest casefiddle-tests-casing-word ()
  (with-temp-buffer
    (dolist (test '((upcase-word     . "FOO Bar")
                    (downcase-word   . "foo Bar")
                    (capitalize-word . "Foo Bar")))
      (dolist (back '(nil t))
        (delete-region (point-min) (point-max))
        (insert "foO Bar")
        (goto-char (+ (if back 4 0) (point-min)))
        (funcall (car test) (if back -1 1))
        (should (string-equal (cdr test) (buffer-string)))
        (should (equal (+ (if back 4 3) (point-min)) (point)))))))


(defun casefiddle-tests--test-casing (tests)
  (nreverse
   (cl-reduce
    (lambda (errors test)
      (let* ((input (car test))
             (expected (cdr test))
             (func-pairs '((upcase upcase-region)
                           (downcase downcase-region)
                           (capitalize capitalize-region)
                           (upcase-initials upcase-initials-region)))
             (get-string (lambda (func) (funcall func input)))
             (get-region (lambda (func)
                           (delete-region (point-min) (point-max))
                           (unwind-protect
                               (progn
                                 (unless (multibyte-string-p input)
                                   (toggle-enable-multibyte-characters))
                                 (insert input)
                                 (funcall func (point-min) (point-max))
                                 (buffer-string))
                             (unless (multibyte-string-p input)
                               (toggle-enable-multibyte-characters)))))
             (fmt-str (lambda (str)
                        (format "%s  (%sbyte; %d chars; %d bytes)"
                                str
                                (if (multibyte-string-p str) "multi" "uni")
                                (length str) (string-bytes str))))
             funcs getters)
        (while (and func-pairs expected)
          (setq funcs (car func-pairs)
                getters (list get-string get-region))
          (while (and funcs getters)
            (let ((got (funcall (car getters) (car funcs))))
              (unless (string-equal got (car expected))
                (let ((fmt (length (symbol-name (car funcs)))))
                  (setq fmt (format "\n%%%ds: %%s" (max fmt 8)))
                  (push (format (concat fmt fmt fmt)
                                (car funcs) (funcall fmt-str input)
                                "expected" (funcall fmt-str (car expected))
                                "but got" (funcall fmt-str got))
                        errors))))
            (setq funcs (cdr funcs) getters (cdr getters)))
          (setq func-pairs (cdr func-pairs) expected (cdr expected))))
      errors)
    (cons () tests))))

(ert-deftest casefiddle-tests-casing ()
  (should-not
   (with-temp-buffer
     (casefiddle-tests--test-casing
      ;; input     upper     lower    capitalize up-initials
      '(("Foo baR" "FOO BAR" "foo bar" "Foo Bar" "Foo BaR")
        ("Ⅷ ⅷ" "Ⅷ Ⅷ" "ⅷ ⅷ" "Ⅷ Ⅷ" "Ⅷ Ⅷ")
        ;; "ǅUNGLA" is an unfortunate result but it’s really best we can
        ;; do while still being consistent.  Hopefully, users only ever
        ;; use upcase-initials on camelCase identifiers not real words.
        ("ǄUNGLA" "ǄUNGLA" "ǆungla" "ǅungla" "ǅUNGLA")
        ("ǅungla" "ǄUNGLA" "ǆungla" "ǅungla" "ǅungla")
        ("ǆungla" "ǄUNGLA" "ǆungla" "ǅungla" "ǅungla")
        ("deﬁne" "DEFINE" "deﬁne" "Deﬁne" "Deﬁne")
        ("ﬁsh" "FISH" "ﬁsh" "Fish" "Fish")
        ("Straße" "STRASSE" "straße" "Straße" "Straße")

        ;; The word repeated twice to test behaviour at the end of a word
        ;; inside of an input string as well as at the end of the string.
        ("ΌΣΟΣ ΌΣΟΣ" "ΌΣΟΣ ΌΣΟΣ" "όσος όσος" "Όσος Όσος" "ΌΣΟΣ ΌΣΟΣ")
        ;; What should be done with sole sigma?  It is ‘final’ but on the
        ;; other hand it does not form a word.  We’re using regular sigma.
        ("Σ Σ" "Σ Σ" "σ σ" "Σ Σ" "Σ Σ")
        ("όσος" "ΌΣΟΣ" "όσος" "Όσος" "Όσος")
        ;; If sigma is already lower case, we don’t want to change it.
        ("όσοσ" "ΌΣΟΣ" "όσοσ" "Όσοσ" "Όσοσ"))))))

(ert-deftest casefiddle-tests-casing-byte8 ()
  (should-not
   (with-temp-buffer
     (casefiddle-tests--test-casing
      '(("\xff Foo baR \xff"
         "\xff FOO BAR \xff"
         "\xff foo bar \xff"
         "\xff Foo Bar \xff"
         "\xff Foo BaR \xff")
        ("\xff Zażółć gĘŚlą \xff"
         "\xff ZAŻÓŁĆ GĘŚLĄ \xff"
         "\xff zażółć gęślą \xff"
         "\xff Zażółć Gęślą \xff"
         "\xff Zażółć GĘŚlą \xff"))))))

(ert-deftest casefiddle-tests-casing-byte8-with-changes ()
  (let ((tab (copy-case-table (standard-case-table)))
        (test '("\xff\xff\xef Foo baR \xcf\xcf"
                "\xef\xef\xef FOO BAR \xcf\xcf"
                "\xff\xff\xff foo bar \xcf\xcf"
                "\xef\xff\xff Foo Bar \xcf\xcf"
                "\xef\xff\xef Foo BaR \xcf\xcf"))
        (byte8 #x3FFF00))
    (should-not
     (with-temp-buffer
       (set-case-table tab)
       (set-case-syntax-pair (+ byte8 #xef) (+ byte8 #xff) tab)
       (casefiddle-tests--test-casing
        (list test
              (mapcar (lambda (str) (decode-coding-string str 'binary)) test)
              '("\xff\xff\xef Zażółć gĘŚlą \xcf\xcf"
                "\xef\xef\xef ZAŻÓŁĆ GĘŚLĄ \xcf\xcf"
                "\xff\xff\xff zażółć gęślą \xcf\xcf"
                "\xef\xff\xff Zażółć Gęślą \xcf\xcf"
                "\xef\xff\xef Zażółć GĘŚlą \xcf\xcf")))))))


(ert-deftest casefiddle-tests-char-casing ()
  ;;             input upcase downcase [titlecase]
  (dolist (test '((?a ?A ?a) (?A ?A ?a)
                  (?ł ?Ł ?ł) (?Ł ?Ł ?ł)
                  (?ß ?ß ?ß) (?ẞ ?ẞ ?ß)
                  (?ⅷ ?Ⅷ ?ⅷ) (?Ⅷ ?Ⅷ ?ⅷ)
                  (?Ǆ ?Ǆ ?ǆ ?ǅ) (?ǅ ?Ǆ ?ǆ ?ǅ) (?ǆ ?Ǆ ?ǆ ?ǅ)))
    (let ((ch (car test))
          (up (nth 1 test))
          (lo (nth 2 test))
          (tc (or (nth 3 test) (nth 1 test))))
      (should (eq up (upcase ch)))
      (should (eq lo (downcase ch)))
      (should (eq tc (capitalize ch)))
      (should (eq tc (upcase-initials ch))))))


;;; casefiddle-tests.el ends here
