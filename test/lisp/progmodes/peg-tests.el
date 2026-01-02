;;; peg-tests.el --- Tests of PEG parsers            -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2026 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests and examples, that used to live in peg.el wrapped inside an `eval'.

;;; Code:

(require 'peg)
(require 'ert)

;;; Tests:

(defmacro peg-parse-string (pex string &optional noerror)
  "Parse STRING according to PEX.
If NOERROR is non-nil, push nil resp. t if the parse failed
resp. succeeded instead of signaling an error."
  (declare (indent 1))
  (let ((oldstyle (consp (car-safe pex)))) ;PEX is really a list of rules.
    `(with-temp-buffer
       (insert ,string)
       (goto-char (point-min))
       ,(if oldstyle
            `(with-peg-rules ,pex
               (peg-run (peg ,(caar pex))
                        ,(unless noerror '#'peg-signal-failure)))
          `(peg-run (peg ,pex)
                    ,(unless noerror '#'peg-signal-failure))))))

(define-peg-rule peg-test-natural ()
  [0-9] (* [0-9]))

(ert-deftest peg-test ()
  (should (peg-parse-string peg-test-natural "99 bottles" t))
  (should (peg-parse-string ((s "a")) "a" t))
  (should (not (peg-parse-string ((s "a")) "b" t)))
  (should (peg-parse-string ((s (not "a"))) "b" t))
  (should (not (peg-parse-string ((s (not "a"))) "a" t)))
  (should (peg-parse-string ((s (if "a"))) "a" t))
  (should (not (peg-parse-string ((s (if "a"))) "b" t)))
  (should (peg-parse-string ((s "ab")) "ab" t))
  (should (not (peg-parse-string ((s "ab")) "ba" t)))
  (should (not (peg-parse-string ((s "ab")) "a" t)))
  (should (peg-parse-string ((s (range ?0 ?9))) "0" t))
  (should (not (peg-parse-string ((s (range ?0 ?9))) "a" t)))
  (should (peg-parse-string ((s [0-9])) "0" t))
  (should (not (peg-parse-string ((s [0-9])) "a" t)))
  (should (not (peg-parse-string ((s [0-9])) "" t)))
  (should (peg-parse-string ((s (any))) "0" t))
  (should (not (peg-parse-string ((s (any))) "" t)))
  (should (peg-parse-string ((s (eob))) "" t))
  (should (peg-parse-string ((s (not (eob)))) "a" t))
  (should (peg-parse-string ((s (or "a" "b"))) "a" t))
  (should (peg-parse-string ((s (or "a" "b"))) "b" t))
  (should (not (peg-parse-string ((s (or "a" "b"))) "c" t)))
  (should (peg-parse-string (and "a" "b") "ab" t))
  (should (peg-parse-string ((s (and "a" "b"))) "abc" t))
  (should (not (peg-parse-string (and "a" "b") "ba" t)))
  (should (peg-parse-string ((s (and "a" "b" "c"))) "abc" t))
  (should (peg-parse-string ((s (* "a") "b" (eob))) "b" t))
  (should (peg-parse-string ((s (* "a") "b" (eob))) "ab" t))
  (should (peg-parse-string ((s (* "a") "b" (eob))) "aaab" t))
  (should (not (peg-parse-string ((s (* "a") "b" (eob))) "abc" t)))
  (should (peg-parse-string ((s "")) "abc" t))
  (should (peg-parse-string ((s "" (eob))) "" t))
  (should (peg-parse-string ((s (opt "a") "b")) "abc" t))
  (should (peg-parse-string ((s (opt "a") "b")) "bc" t))
  (should (not (peg-parse-string ((s (or))) "ab" t)))
  (should (peg-parse-string ((s (and))) "ab" t))
  (should (peg-parse-string ((s (and))) "" t))
  (should (peg-parse-string ((s ["^"])) "^" t))
  (should (peg-parse-string ((s ["^a"])) "a" t))
  (should (peg-parse-string ["-"] "-" t))
  (should (peg-parse-string ((s ["]-"])) "]" t))
  (should (peg-parse-string ((s ["^]"])) "^" t))
  (should (peg-parse-string ((s [alpha])) "z" t))
  (should (not (peg-parse-string ((s [alpha])) "0" t)))
  (should (not (peg-parse-string ((s [alpha])) "" t)))
  (should (not (peg-parse-string ((s ["][:alpha:]"])) "z" t)))
  (should (peg-parse-string ((s (bob))) "" t))
  (should (peg-parse-string ((s (bos))) "x" t))
  (should (not (peg-parse-string ((s (bos))) " x" t)))
  (should (peg-parse-string ((s "x" (eos))) "x" t))
  (should (peg-parse-string ((s (syntax-class whitespace))) " " t))
  (should (peg-parse-string ((s (= "foo"))) "foo" t))
  (should (let ((f "foo")) (peg-parse-string ((s (= f))) "foo" t)))
  (should (not (peg-parse-string ((s (= "foo"))) "xfoo" t)))
  (should (equal (peg-parse-string ((s `(-- 1 2))) "") '(2 1)))
  (should (equal (peg-parse-string ((s `(-- 1 2) `(a b -- a b))) "") '(2 1)))
  (should (equal (peg-parse-string ((s (or (and (any) s)
					   (substring [0-9]))))
				   "ab0cd1ef2gh")
		 '("2")))
  ;; The PEG rule `doesntexist' doesn't exist, which will cause a byte-compiler
  ;; warning, but not an error at run time because the rule is not actually
  ;; used in this particular case.
  (let* ((testfun '(lambda ()
                     (peg-parse-string ((s (substring (or "a" doesntexist)))
                                        ;; Unused left-recursive rule, should
                                        ;; cause a byte-compiler warning.
                                        (r (* "a") r))
                       "af")))
         (compiledfun
          (progn
            (with-current-buffer (get-buffer-create "*Compile-Log*")
             (let ((inhibit-read-only t)) (erase-buffer)))
            (let ((lexical-binding t)) (byte-compile testfun)))))
    (with-current-buffer (get-buffer-create "*Compile-Log*")
      (goto-char (point-min))
      (should
       ;; FIXME: The byte-compiler emits "not known to be defined"
       ;; warnings when compiling a file but not from `byte-compile'.
       ;; Instead, we have to dig it out of the mess it leaves behind.  🙂
       (or (assq 'peg-rule\ doesntexist byte-compile-unresolved-functions)
           (should (re-search-forward
                    "peg-rule.? doesntexist.*not known to be defined" nil t))))
      (goto-char (point-min))
      (should (re-search-forward "left recursion.*r -> r" nil t)))

    (should (equal (funcall compiledfun) '("a"))))
  (should (equal (peg-parse-string ((s (list x y))
				    (x `(-- 1))
				    (y `(-- 2)))
				   "")
		 '((1 2))))
  (should (equal (peg-parse-string ((s (list (* x)))
                                    (x "" `(-- 'x)))
                                   "xxx")
                 ;; The empty loop body should be matched once!
                 '((x))))
  (should (equal (peg-parse-string ((s (list (* x)))
                                    (x "x" `(-- 'x)))
                                   "xxx")
                 '((x x x))))
  (should (equal (peg-parse-string ((s (region (* x)))
				    (x "x" `(-- 'x)))
				   "xxx")
                 ;; FIXME: Since string positions start at 0, this should
                 ;; really be '(3 x x x 0) !!
		 '(4 x x x 1)))
  (should (equal (peg-parse-string ((s (region (list (* x))))
				    (x "x" `(-- 'x 'y)))
				   "xxx")
		 '(4 (x y x y x y) 1)))
  (should (equal (with-temp-buffer
		   (save-excursion (insert "abcdef"))
		   (list
		    (peg-run (peg "a"
				  (replace "bc" "x")
				  (replace "de" "y")
				  "f"))
		    (buffer-string)))
		 '(t "axyf")))
  (with-temp-buffer
    (insert "toro")
    (goto-char (point-min))
    (should (peg-run (peg "to")))
    (should-not (peg-run (peg "to")))
    (should (peg-run (peg "ro")))
    (should (eobp)))
  (with-temp-buffer
    (insert "   ")
    (goto-char (point-min))
    (peg-run (peg (+ (syntax-class whitespace))))
    (should (eobp)))
  )

(define-peg-ruleset peg-test-myrules
  (sign  () (or "+" "-" ""))
  (digit () [0-9])
  (nat   () digit (* digit))
  (int   () sign digit (* digit))
  (float () int "." nat))

(ert-deftest peg-test-ruleset ()
  (with-peg-rules
      (peg-test-myrules
       (complex float "+i" float))
    (should (peg-parse-string nat "123" t))
    (should (not (peg-parse-string nat "home" t)))))

;;; Examples:

;; peg-ex-recognize-int recognizes integers.  An integer begins with a
;; optional sign, then follows one or more digits.  Digits are all
;; characters from 0 to 9.
;;
;; Notes:
;; 1) "" matches the empty sequence, i.e. matches without consuming
;;    input.
;; 2) [0-9] is the character range from 0 to 9.  This can also be
;;    written as (range ?0 ?9).  Note that 0-9 is a symbol.
(defun peg-ex-recognize-int ()
  (with-peg-rules ((number   sign digit (* digit))
	           (sign     (or "+" "-" ""))
	           (digit    [0-9]))
    (peg-run (peg number))))

;; peg-ex-parse-int recognizes integers and computes the corresponding
;; value.  The grammar is the same as for `peg-ex-recognize-int'
;; augmented with parsing actions.  Unfortunately, the actions add
;; quite a bit of clutter.
;;
;; The actions for the sign rule push -1 on the stack for a minus sign
;; and 1 for plus or no sign.
;;
;; The action for the digit rule pushes the value for a single digit.
;;
;; The action `(a b -- (+ (* a 10) b)), takes two items from the stack
;; and pushes the first digit times 10 added to the second digit.
;;
;; The action `(sign val -- (* sign val)), multiplies val with the
;; sign (1 or -1).
(defun peg-ex-parse-int ()
  (with-peg-rules ((number sign digit (* digit
				         `(a b -- (+ (* a 10) b)))
		           `(sign val -- (* sign val)))
                   (sign (or (and "+" `(-- 1))
	                     (and "-" `(-- -1))
	                     (and ""  `(-- 1))))
	           (digit [0-9] `(-- (- (char-before) ?0))))
    (peg-run (peg number))))

;; Put point after the ) and press C-x C-e
;; (peg-ex-parse-int)-234234

;; Parse arithmetic expressions and compute the result as side effect.
(defun peg-ex-arith ()
  (peg-parse
   (expr _ sum eol)
   (sum product (* (or (and "+" _ product `(a b -- (+ a b)))
		       (and "-" _ product `(a b -- (- a b))))))
   (product value (* (or (and "*" _ value `(a b -- (* a b)))
			 (and "/" _ value `(a b -- (/ a b))))))
   (value (or (and (substring number) `(string -- (string-to-number string)))
	      (and "(" _ sum ")" _)))
   (number (+ [0-9]) _)
   (_ (* [" \t"]))
   (eol (or "\n" "\r\n" "\r"))))

;; (peg-ex-arith)   1 + 2 * 3 * (4 + 5)
;; (peg-ex-arith)   1 + 2 ^ 3 * (4 + 5)  ; fails to parse

;; Parse URI according to RFC 2396.
(defun peg-ex-uri ()
  (peg-parse
   (URI-reference (or absoluteURI relativeURI)
		  (or (and "#" (substring fragment))
		      `(-- nil))
		  `(scheme user host port path query fragment --
			   (list :scheme scheme :user user
				 :host host :port port
				 :path path :query query
				 :fragment fragment)))
   (absoluteURI (substring scheme) ":" (or hier-part opaque-part))
   (hier-part ;(-- user host port path query)
    (or net-path
	(and `(-- nil nil nil)
	     abs-path))
    (or (and "?" (substring query))
	`(-- nil)))
   (net-path "//" authority (or abs-path `(-- nil)))
   (abs-path "/" path-segments)
   (path-segments segment (list (* "/" segment)) `(s l -- (cons s l)))
   (segment (substring (* pchar) (* ";" param)))
   (param (* pchar))
   (pchar (or unreserved escaped [":@&=+$,"]))
   (query (* uric))
   (fragment (* uric))
   (relativeURI (or net-path abs-path rel-path) (opt "?" query))
   (rel-path rel-segment (opt abs-path))
   (rel-segment (+ unreserved escaped [";@&=+$,"]))
   (authority (or server reg-name))
   (server (or (and (or (and (substring userinfo) "@")
			`(-- nil))
		    hostport)
	       `(-- nil nil nil)))
   (userinfo (* (or unreserved escaped [";:&=+$,"])))
   (hostport (substring host) (or (and ":" (substring port))
				  `(-- nil)))
   (host (or hostname ipv4address))
   (hostname (* domainlabel ".") toplabel (opt "."))
   (domainlabel alphanum
		(opt (* (or alphanum "-") (if alphanum))
		     alphanum))
   (toplabel alpha
	     (* (or alphanum "-") (if alphanum))
	     alphanum)
   (ipv4address (+ digit) "." (+ digit) "." (+ digit) "." (+ digit))
   (port (* digit))
   (scheme alpha (* (or alpha digit ["+-."])))
   (reg-name (or unreserved escaped ["$,;:@&=+"]))
   (opaque-part uric-no-slash (* uric))
   (uric (or reserved unreserved escaped))
   (uric-no-slash (or unreserved escaped [";?:@&=+$,"]))
   (reserved (set ";/?:@&=+$,"))
   (unreserved (or alphanum mark))
   (escaped "%" hex hex)
   (hex (or digit [A-F] [a-f]))
   (mark (set "-_.!~*'()"))
   (alphanum (or alpha digit))
   (alpha (or lowalpha upalpha))
   (lowalpha [a-z])
   (upalpha [A-Z])
   (digit [0-9])))

;; (peg-ex-uri)http://luser@www.foo.com:8080/bar/baz.html?x=1#foo
;; (peg-ex-uri)file:/bar/baz.html?foo=df#x

;; Split STRING where SEPARATOR occurs.
(defun peg-ex-split (string separator)
  (peg-parse-string ((s (list (* (* sep) elt)))
		     (elt (substring (+ (not sep) (any))))
		     (sep (= separator)))
		    string))

;; (peg-ex-split "-abc-cd-" "-")

;; Parse a lisp style Sexp.
;; [To keep the example short, ' and . are handled as ordinary symbol.]
(defun peg-ex-lisp ()
  (peg-parse
   (sexp _ (or string list number symbol))
   (_ (* (or [" \n\t"] comment)))
   (comment ";" (* (not (or "\n" (eob))) (any)))
   (string "\"" (substring  (* (not "\"") (any))) "\"")
   (number (substring (opt (set "+-")) (+ digit))
	   (if terminating)
	   `(string -- (string-to-number string)))
   (symbol (substring (and symchar (* (not terminating) symchar)))
	   `(s -- (intern s)))
   (symchar [a-z A-Z 0-9 "-;!#%&'*+,./:;<=>?@[]^_`{|}~"])
   (list "("		`(-- (cons nil nil)) `(hd -- hd hd)
	 (* sexp	`(tl e -- (setcdr tl (list e))))
	 _ ")"	`(hd _tl -- (cdr hd)))
   (digit [0-9])
   (terminating (or (set " \n\t();\"'") (eob)))))

;; (peg-ex-lisp)

;; We try to detect left recursion and report it as error.
(defun peg-ex-left-recursion ()
  (eval '(peg-parse (exp (or term
			     (and exp "+" exp)))
		    (term (or digit
			      (and term "*" term)))
		    (digit [0-9]))
        t))

(defun peg-ex-infinite-loop ()
  (eval '(peg-parse (exp (* (or "x"
				"y"
				(action (foo))))))
        t))

;; Some efficiency problems:

;; Find the last digit in a string.
;; Recursive definition with excessive stack usage.
(defun peg-ex-last-digit (string)
  (peg-parse-string ((s (or (and (any) s)
			    (substring [0-9]))))
		    string))

;; (peg-ex-last-digit "ab0cd1ef2gh")
;; (peg-ex-last-digit (make-string 50 ?-))
;; (peg-ex-last-digit (make-string 1000 ?-))

;; Find the last digit without recursion.  Doesn't run out of stack,
;; but probably still too inefficient for large inputs.
(defun peg-ex-last-digit2 (string)
  (peg-parse-string ((s `(-- nil)
			(+ (* (not digit) (any))
			   (substring digit)
			   `(_d1 d2 -- d2)))
		     (digit [0-9]))
		    string))

;; (peg-ex-last-digit2 "ab0cd1ef2gh")
;; (peg-ex-last-digit2 (concat (make-string 500000 ?-) "8a9b"))
;; (peg-ex-last-digit2 (make-string 500000 ?-))
;; (peg-ex-last-digit2 (make-string 500000 ?5))

(ert-deftest peg-tests--peg-parse ()
  (with-temp-buffer
    (insert "abc")
    (goto-char (point-min))
    (peg-parse (bob) "ab")
    (should (looking-at "c"))))

(provide 'peg-tests)
;;; peg-tests.el ends here
