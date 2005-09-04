;;; calc-lang.el --- calc language functions

;; Copyright (C) 1990, 1991, 1992, 1993, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: David Gillespie <daveg@synaptics.com>
;; Maintainer: Jay Belanger <belanger@truman.edu>

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; Commentary:

;;; Code:

;; This file is autoloaded from calc-ext.el.

(require 'calc-ext)
(require 'calc-macs)

;;; Alternate entry/display languages.

(defun calc-set-language (lang &optional option no-refresh)
  (setq math-expr-opers (or (get lang 'math-oper-table) math-standard-opers)
	math-expr-function-mapping (get lang 'math-function-table)
	math-expr-special-function-mapping (get lang 'math-special-function-table)
	math-expr-variable-mapping (get lang 'math-variable-table)
	calc-language-input-filter (get lang 'math-input-filter)
	calc-language-output-filter (get lang 'math-output-filter)
	calc-vector-brackets (or (get lang 'math-vector-brackets) "[]")
	calc-complex-format (get lang 'math-complex-format)
	calc-radix-formatter (get lang 'math-radix-formatter)
	calc-function-open (or (get lang 'math-function-open) "(")
	calc-function-close (or (get lang 'math-function-close) ")"))
  (if no-refresh
      (setq calc-language lang
	    calc-language-option option)
    (calc-change-mode '(calc-language calc-language-option)
		      (list lang option) t)))

(defun calc-normal-language ()
  (interactive)
  (calc-wrapper
   (calc-set-language nil)
   (message "Normal language mode")))

(defun calc-flat-language ()
  (interactive)
  (calc-wrapper
   (calc-set-language 'flat)
   (message "Flat language mode (all stack entries shown on one line)")))

(defun calc-big-language ()
  (interactive)
  (calc-wrapper
   (calc-set-language 'big)
   (message "\"Big\" language mode")))

(defun calc-unformatted-language ()
  (interactive)
  (calc-wrapper
   (calc-set-language 'unform)
   (message "Unformatted language mode")))


(defun calc-c-language ()
  (interactive)
  (calc-wrapper
   (calc-set-language 'c)
   (message "`C' language mode")))

(put 'c 'math-oper-table
  '( ( "u+"    ident	     -1 1000 )
     ( "u-"    neg	     -1 1000 )
     ( "u!"    calcFunc-lnot -1 1000 )
     ( "~"     calcFunc-not  -1 1000 )
     ( "*"     *	     190 191 )
     ( "/"     /	     190 191 )
     ( "%"     %	     190 191 )
     ( "+"     +	     180 181 )
     ( "-"     -	     180 181 )
     ( "<<"    calcFunc-lsh  170 171 )
     ( ">>"    calcFunc-rsh  170 171 )
     ( "<"     calcFunc-lt   160 161 )
     ( ">"     calcFunc-gt   160 161 )
     ( "<="    calcFunc-leq  160 161 )
     ( ">="    calcFunc-geq  160 161 )
     ( "=="    calcFunc-eq   150 151 )
     ( "!="    calcFunc-neq  150 151 )
     ( "&"     calcFunc-and  140 141 )
     ( "^"     calcFunc-xor  131 130 )
     ( "|"     calcFunc-or   120 121 )
     ( "&&"    calcFunc-land 110 111 )
     ( "||"    calcFunc-lor  100 101 )
     ( "?"     (math-read-if)  91  90 )
     ( "!!!"   calcFunc-pnot  -1  88 )
     ( "&&&"   calcFunc-pand  85  86 )
     ( "|||"   calcFunc-por   75  76 )
     ( "="     calcFunc-assign 51 50 )
     ( ":="    calcFunc-assign 51 50 )
     ( "::"    calcFunc-condition 45 46 ))) ; should support full assignments

(put 'c 'math-function-table
  '( ( acos	   . calcFunc-arccos )
     ( acosh	   . calcFunc-arccosh )
     ( asin	   . calcFunc-arcsin )
     ( asinh	   . calcFunc-arcsinh )
     ( atan	   . calcFunc-arctan )
     ( atan2	   . calcFunc-arctan2 )
     ( atanh	   . calcFunc-arctanh )))

(put 'c 'math-variable-table
  '( ( M_PI	   . var-pi )
     ( M_E	   . var-e )))

(put 'c 'math-vector-brackets "{}")

(put 'c 'math-radix-formatter
     (function (lambda (r s)
		 (if (= r 16) (format "0x%s" s)
		   (if (= r 8) (format "0%s" s)
		     (format "%d#%s" r s))))))


(defun calc-pascal-language (n)
  (interactive "P")
  (calc-wrapper
   (and n (setq n (prefix-numeric-value n)))
   (calc-set-language 'pascal n)
   (message (if (and n (/= n 0))
		(if (> n 0)
		    "Pascal language mode (all uppercase)"
		  "Pascal language mode (all lowercase)")
	      "Pascal language mode"))))

(put 'pascal 'math-oper-table
  '( ( "not"   calcFunc-lnot -1 1000 )
     ( "*"     *	     190 191 )
     ( "/"     /	     190 191 )
     ( "and"   calcFunc-and  190 191 )
     ( "div"   calcFunc-idiv 190 191 )
     ( "mod"   %	     190 191 )
     ( "u+"    ident	     -1  185 )
     ( "u-"    neg	     -1  185 )
     ( "+"     +	     180 181 )
     ( "-"     -	     180 181 )
     ( "or"    calcFunc-or   180 181 )
     ( "xor"   calcFunc-xor  180 181 )
     ( "shl"   calcFunc-lsh  180 181 )
     ( "shr"   calcFunc-rsh  180 181 )
     ( "in"    calcFunc-in   160 161 )
     ( "<"     calcFunc-lt   160 161 )
     ( ">"     calcFunc-gt   160 161 )
     ( "<="    calcFunc-leq  160 161 )
     ( ">="    calcFunc-geq  160 161 )
     ( "="     calcFunc-eq   160 161 )
     ( "<>"    calcFunc-neq  160 161 )
     ( "!!!"   calcFunc-pnot  -1  85 )
     ( "&&&"   calcFunc-pand  80  81 )
     ( "|||"   calcFunc-por   75  76 )
     ( ":="    calcFunc-assign 51 50 )
     ( "::"    calcFunc-condition 45 46 )))

(put 'pascal 'math-input-filter 'calc-input-case-filter)
(put 'pascal 'math-output-filter 'calc-output-case-filter)

(put 'pascal 'math-radix-formatter
     (function (lambda (r s)
		 (if (= r 16) (format "$%s" s)
		   (format "%d#%s" r s)))))

(defun calc-input-case-filter (str)
  (cond ((or (null calc-language-option) (= calc-language-option 0))
	 str)
	(t
	 (downcase str))))

(defun calc-output-case-filter (str)
  (cond ((or (null calc-language-option) (= calc-language-option 0))
	 str)
	((> calc-language-option 0)
	 (upcase str))
	(t
	 (downcase str))))


(defun calc-fortran-language (n)
  (interactive "P")
  (calc-wrapper
   (and n (setq n (prefix-numeric-value n)))
   (calc-set-language 'fortran n)
   (message (if (and n (/= n 0))
		(if (> n 0)
		    "FORTRAN language mode (all uppercase)"
		  "FORTRAN language mode (all lowercase)")
	      "FORTRAN language mode"))))

(put 'fortran 'math-oper-table
  '( ( "u/"    (math-parse-fortran-vector) -1 1 )
     ( "/"     (math-parse-fortran-vector-end) 1 -1 )
     ( "**"    ^             201 200 )
     ( "u+"    ident	     -1  191 )
     ( "u-"    neg	     -1  191 )
     ( "*"     *	     190 191 )
     ( "/"     /	     190 191 )
     ( "+"     +	     180 181 )
     ( "-"     -	     180 181 )
     ( ".LT."  calcFunc-lt   160 161 )
     ( ".GT."  calcFunc-gt   160 161 )
     ( ".LE."  calcFunc-leq  160 161 )
     ( ".GE."  calcFunc-geq  160 161 )
     ( ".EQ."  calcFunc-eq   160 161 )
     ( ".NE."  calcFunc-neq  160 161 )
     ( ".NOT." calcFunc-lnot -1  121 )
     ( ".AND." calcFunc-land 110 111 )
     ( ".OR."  calcFunc-lor  100 101 )
     ( "!!!"   calcFunc-pnot  -1  85 )
     ( "&&&"   calcFunc-pand  80  81 )
     ( "|||"   calcFunc-por   75  76 )
     ( "="     calcFunc-assign 51 50 )
     ( ":="    calcFunc-assign 51 50 )
     ( "::"    calcFunc-condition 45 46 )))

(put 'fortran 'math-vector-brackets "//")

(put 'fortran 'math-function-table
  '( ( acos	   . calcFunc-arccos )
     ( acosh	   . calcFunc-arccosh )
     ( aimag	   . calcFunc-im )
     ( aint	   . calcFunc-ftrunc )
     ( asin	   . calcFunc-arcsin )
     ( asinh	   . calcFunc-arcsinh )
     ( atan	   . calcFunc-arctan )
     ( atan2	   . calcFunc-arctan2 )
     ( atanh	   . calcFunc-arctanh )
     ( conjg	   . calcFunc-conj )
     ( log	   . calcFunc-ln )
     ( nint	   . calcFunc-round )
     ( real	   . calcFunc-re )))

(put 'fortran 'math-input-filter 'calc-input-case-filter)
(put 'fortran 'math-output-filter 'calc-output-case-filter)

;; The next few variables are local to math-read-exprs in calc-aent.el 
;; and math-read-expr in calc-ext.el, but are set in functions they call.

(defvar math-exp-token)
(defvar math-expr-data)
(defvar math-exp-old-pos)

(defvar math-parsing-fortran-vector nil)
(defun math-parse-fortran-vector (op)
  (let ((math-parsing-fortran-vector '(end . "\000")))
    (prog1
	(math-read-brackets t "]")
      (setq math-exp-token (car math-parsing-fortran-vector)
	    math-expr-data (cdr math-parsing-fortran-vector)))))

(defun math-parse-fortran-vector-end (x op)
  (if math-parsing-fortran-vector
      (progn
	(setq math-parsing-fortran-vector (cons math-exp-token math-expr-data)
	      math-exp-token 'end
	      math-expr-data "\000")
	x)
    (throw 'syntax "Unmatched closing `/'")))

(defun math-parse-fortran-subscr (sym args)
  (setq sym (math-build-var-name sym))
  (while args
    (setq sym (list 'calcFunc-subscr sym (car args))
	  args (cdr args)))
  sym)


(defun calc-tex-language (n)
  (interactive "P")
  (calc-wrapper
   (and n (setq n (prefix-numeric-value n)))
   (calc-set-language 'tex n)
   (cond ((not n)
          (message "TeX language mode"))
         ((= n 0)
          (message "TeX language mode with multiline matrices"))
         ((= n 1)
          (message "TeX language mode with \\hbox{func}(\\hbox{var})"))
         ((> n 1)
          (message 
           "TeX language mode with \\hbox{func}(\\hbox{var}) and multiline matrices"))
         ((= n -1)
          (message "TeX language mode with \\func(\\hbox{var})"))
         ((< n -1)
          (message 
           "TeX language mode with \\func(\\hbox{var}) and multiline matrices")))))

(defun calc-latex-language (n)
  (interactive "P")
  (calc-wrapper
   (and n (setq n (prefix-numeric-value n)))
   (calc-set-language 'latex n)
   (cond ((not n)
          (message "LaTeX language mode"))
         ((= n 0)
          (message "LaTeX language mode with multiline matrices"))
         ((= n 1)
          (message "LaTeX language mode with \\text{func}(\\text{var})"))
         ((> n 1)
          (message 
           "LaTeX language mode with \\text{func}(\\text{var}) and multiline matrices"))
         ((= n -1)
          (message "LaTeX language mode with \\func(\\text{var})"))
         ((< n -1)
          (message 
           "LaTeX language mode with \\func(\\text{var}) and multiline matrices")))))

(put 'tex 'math-oper-table
  '( ( "u+"       ident		   -1 1000 )
     ( "u-"       neg		   -1 1000 )
     ( "\\hat"    calcFunc-hat     -1  950 )
     ( "\\check"  calcFunc-check   -1  950 )
     ( "\\tilde"  calcFunc-tilde   -1  950 )
     ( "\\acute"  calcFunc-acute   -1  950 )
     ( "\\grave"  calcFunc-grave   -1  950 )
     ( "\\dot"    calcFunc-dot     -1  950 )
     ( "\\ddot"   calcFunc-dotdot  -1  950 )
     ( "\\breve"  calcFunc-breve   -1  950 )
     ( "\\bar"    calcFunc-bar     -1  950 )
     ( "\\vec"    calcFunc-Vec     -1  950 )
     ( "\\underline" calcFunc-under -1  950 )
     ( "u|"       calcFunc-abs	   -1    0 )
     ( "|"        closing	    0   -1 )
     ( "\\lfloor" calcFunc-floor   -1    0 )
     ( "\\rfloor" closing           0   -1 )
     ( "\\lceil"  calcFunc-ceil    -1    0 )
     ( "\\rceil"  closing           0   -1 )
     ( "\\pm"	  sdev		   300 300 )
     ( "!"        calcFunc-fact	   210  -1 )
     ( "^"	  ^		   201 200 )
     ( "_"	  calcFunc-subscr  201 200 )
     ( "\\times"  *		   191 190 )
     ( "*"        *		   191 190 )
     ( "2x"	  *		   191 190 )
     ( "+"	  +		   180 181 )
     ( "-"	  -		   180 181 )
     ( "\\over"	  /		   170 171 )
     ( "/"	  /		   170 171 )
     ( "\\choose" calcFunc-choose  170 171 )
     ( "\\mod"	  %		   170 171 )
     ( "<"	  calcFunc-lt	   160 161 )
     ( ">"	  calcFunc-gt	   160 161 )
     ( "\\leq"	  calcFunc-leq	   160 161 )
     ( "\\geq"	  calcFunc-geq	   160 161 )
     ( "="	  calcFunc-eq	   160 161 )
     ( "\\neq"	  calcFunc-neq	   160 161 )
     ( "\\ne"	  calcFunc-neq	   160 161 )
     ( "\\lnot"   calcFunc-lnot     -1 121 )
     ( "\\land"	  calcFunc-land    110 111 )
     ( "\\lor"	  calcFunc-lor     100 101 )
     ( "?"	  (math-read-if)    91  90 )
     ( "!!!"	  calcFunc-pnot	    -1  85 )
     ( "&&&"	  calcFunc-pand	    80  81 )
     ( "|||"	  calcFunc-por	    75  76 )
     ( "\\gets"	  calcFunc-assign   51  50 )
     ( ":="	  calcFunc-assign   51  50 )
     ( "::"       calcFunc-condition 45 46 )
     ( "\\to"	  calcFunc-evalto   40  41 )
     ( "\\to"	  calcFunc-evalto   40  -1 )
     ( "=>" 	  calcFunc-evalto   40  41 )
     ( "=>" 	  calcFunc-evalto   40  -1 )))

(put 'tex 'math-function-table
  '( ( \\arccos	   . calcFunc-arccos )
     ( \\arcsin	   . calcFunc-arcsin )
     ( \\arctan	   . calcFunc-arctan )
     ( \\arg	   . calcFunc-arg )
     ( \\cos	   . calcFunc-cos )
     ( \\cosh	   . calcFunc-cosh )
     ( \\cot	   . calcFunc-cot )
     ( \\coth	   . calcFunc-coth )
     ( \\csc	   . calcFunc-csc )
     ( \\det	   . calcFunc-det )
     ( \\exp	   . calcFunc-exp )
     ( \\gcd	   . calcFunc-gcd )
     ( \\ln	   . calcFunc-ln )
     ( \\log	   . calcFunc-log10 )
     ( \\max	   . calcFunc-max )
     ( \\min	   . calcFunc-min )
     ( \\sec	   . calcFunc-sec )
     ( \\sin	   . calcFunc-sin )
     ( \\sinh	   . calcFunc-sinh )
     ( \\sqrt	   . calcFunc-sqrt )
     ( \\tan	   . calcFunc-tan )
     ( \\tanh	   . calcFunc-tanh )
     ( \\phi	   . calcFunc-totient )
     ( \\mu	   . calcFunc-moebius )))

(put 'tex 'math-variable-table
  '( ( \\pi	   . var-pi )
     ( \\infty	   . var-inf )
     ( \\infty	   . var-uinf )
     ( \\phi       . var-phi )
     ( \\gamma     . var-gamma )
     ( \\sum       . (math-parse-tex-sum calcFunc-sum) )
     ( \\prod      . (math-parse-tex-sum calcFunc-prod) )))

(put 'tex 'math-complex-format 'i)

(defun math-parse-tex-sum (f val)
  (let (low high save)
    (or (equal math-expr-data "_") (throw 'syntax "Expected `_'"))
    (math-read-token)
    (setq save math-exp-old-pos)
    (setq low (math-read-factor))
    (or (eq (car-safe low) 'calcFunc-eq)
	(progn
	  (setq math-exp-old-pos (1+ save))
	  (throw 'syntax "Expected equation")))
    (or (equal math-expr-data "^") (throw 'syntax "Expected `^'"))
    (math-read-token)
    (setq high (math-read-factor))
    (list (nth 2 f) (math-read-factor) (nth 1 low) (nth 2 low) high)))

(defun math-tex-input-filter (str)   ; allow parsing of 123\,456\,789.
  (while (string-match "[0-9]\\\\,[0-9]" str)
    (setq str (concat (substring str 0 (1+ (match-beginning 0)))
		      (substring str (1- (match-end 0))))))
  str)
(put 'tex 'math-input-filter 'math-tex-input-filter)

(put 'latex 'math-oper-table
     (append (get 'tex 'math-oper-table)
             '(( "\\Hat"    calcFunc-Hat     -1  950 )
               ( "\\Check"  calcFunc-Check   -1  950 )
               ( "\\Tilde"  calcFunc-Tilde   -1  950 )
               ( "\\Acute"  calcFunc-Acute   -1  950 )
               ( "\\Grave"  calcFunc-Grave   -1  950 )
               ( "\\Dot"    calcFunc-Dot     -1  950 )
               ( "\\Ddot"   calcFunc-Dotdot  -1  950 )
               ( "\\Breve"  calcFunc-Breve   -1  950 )
               ( "\\Bar"    calcFunc-Bar     -1  950 )
               ( "\\Vec"    calcFunc-VEC     -1  950 )
               ( "\\dddot"  calcFunc-dddot   -1  950 )
               ( "\\ddddot" calcFunc-ddddot  -1  950 )
               ( "\div"     /                170 171 )
               ( "\\le"     calcFunc-leq     160 161 )
               ( "\\leqq"   calcFunc-leq     160 161 )
               ( "\\leqsland" calcFunc-leq   160 161 )
               ( "\\ge"	    calcFunc-geq     160 161 )
               ( "\\geqq"   calcFunc-geq     160 161 )
               ( "\\geqslant" calcFunc-geq   160 161 )
               ( "="	    calcFunc-eq	     160 161 )
               ( "\\neq"    calcFunc-neq     160 161 )
               ( "\\ne"	    calcFunc-neq     160 161 )
               ( "\\lnot"   calcFunc-lnot     -1 121 )
               ( "\\land"   calcFunc-land    110 111 )
               ( "\\lor"    calcFunc-lor     100 101 )
               ( "?"	    (math-read-if)    91  90 )
               ( "!!!"	    calcFunc-pnot     -1  85 )
               ( "&&&"	    calcFunc-pand     80  81 )
               ( "|||"	    calcFunc-por      75  76 )
               ( "\\gets"   calcFunc-assign   51  50 )
               ( ":="	    calcFunc-assign   51  50 )
               ( "::"       calcFunc-condition 45 46 )
               ( "\\to"	    calcFunc-evalto   40  41 )
               ( "\\to"	    calcFunc-evalto   40  -1 )
               ( "=>" 	    calcFunc-evalto   40  41 )
               ( "=>" 	    calcFunc-evalto   40  -1 ))))

(put 'latex 'math-function-table
     (append
      (get 'tex 'math-function-table)
      '(( \\frac      . (math-latex-parse-frac))
        ( \\tfrac     . (math-latex-parse-frac))
        ( \\dfrac     . (math-latex-parse-frac))
        ( \\binom     . (math-latex-parse-two-args calcFunc-choose))
        ( \\tbinom    . (math-latex-parse-two-args calcFunc-choose))
        ( \\dbinom    . (math-latex-parse-two-args calcFunc-choose))
        ( \\phi	      . calcFunc-totient )
        ( \\mu	      . calcFunc-moebius ))))

(put 'latex 'math-special-function-table
     '((/               . (math-latex-print-frac "\\frac"))
       (calcFunc-choose . (math-latex-print-frac "\\binom"))))

(put 'latex 'math-variable-table
     (get 'tex 'math-variable-table))

(put 'latex 'math-complex-format 'i)


(defun math-latex-parse-frac (f val)
  (let (numer denom)
    (setq numer (car (math-read-expr-list)))
    (math-read-token)
    (setq denom (math-read-factor))
    (if (and (Math-num-integerp numer)
             (Math-num-integerp denom))
        (list 'frac numer denom)
      (list '/ numer denom))))

(defun math-latex-parse-two-args (f val)
  (let (first second)
    (setq first (car (math-read-expr-list)))
    (math-read-token)
    (setq second (math-read-factor))
    (list (nth 2 f) first second)))

(defun math-latex-print-frac (a fn)
  (list 'horiz (nth 1 fn) "{" (math-compose-expr (nth 1 a) -1)
               "}{"
               (math-compose-expr (nth 2 a) -1)
               "}"))

(put 'latex 'math-input-filter 'math-tex-input-filter)

(defun calc-eqn-language (n)
  (interactive "P")
  (calc-wrapper
   (calc-set-language 'eqn)
   (message "Eqn language mode")))

(put 'eqn 'math-oper-table
  '( ( "u+"       ident		   -1 1000 )
     ( "u-"       neg		   -1 1000 )
     ( "prime"    (math-parse-eqn-prime) 950  -1 )
     ( "prime"    calcFunc-Prime   950  -1 )
     ( "dot"      calcFunc-dot     950  -1 )
     ( "dotdot"   calcFunc-dotdot  950  -1 )
     ( "hat"      calcFunc-hat     950  -1 )
     ( "tilde"    calcFunc-tilde   950  -1 )
     ( "vec"      calcFunc-Vec     950  -1 )
     ( "dyad"     calcFunc-dyad    950  -1 )
     ( "bar"      calcFunc-bar     950  -1 )
     ( "under"    calcFunc-under   950  -1 )
     ( "sub"	  calcFunc-subscr  931 930 )
     ( "sup"	  ^		   921 920 )
     ( "sqrt"	  calcFunc-sqrt    -1  910 )
     ( "over"	  /		   900 901 )
     ( "u|"       calcFunc-abs	   -1    0 )
     ( "|"        closing	    0   -1 )
     ( "left floor"  calcFunc-floor -1   0 )
     ( "right floor" closing        0   -1 )
     ( "left ceil"   calcFunc-ceil  -1   0 )
     ( "right ceil"  closing        0   -1 )
     ( "+-"	  sdev		   300 300 )
     ( "!"        calcFunc-fact	   210  -1 )
     ( "times"    *		   191 190 )
     ( "*"        *		   191 190 )
     ( "2x"	  *		   191 190 )
     ( "/"	  /		   180 181 )
     ( "%"	  %		   180 181 )
     ( "+"	  +		   170 171 )
     ( "-"	  -		   170 171 )
     ( "<"	  calcFunc-lt	   160 161 )
     ( ">"	  calcFunc-gt	   160 161 )
     ( "<="	  calcFunc-leq	   160 161 )
     ( ">="	  calcFunc-geq	   160 161 )
     ( "="	  calcFunc-eq	   160 161 )
     ( "=="	  calcFunc-eq	   160 161 )
     ( "!="	  calcFunc-neq	   160 161 )
     ( "u!"       calcFunc-lnot     -1 121 )
     ( "&&"	  calcFunc-land    110 111 )
     ( "||"	  calcFunc-lor     100 101 )
     ( "?"	  (math-read-if)    91  90 )
     ( "!!!"	  calcFunc-pnot	    -1  85 )
     ( "&&&"	  calcFunc-pand	    80  81 )
     ( "|||"	  calcFunc-por	    75  76 )
     ( "<-"	  calcFunc-assign   51  50 )
     ( ":="	  calcFunc-assign   51  50 )
     ( "::"	  calcFunc-condition 45 46 )
     ( "->"	  calcFunc-evalto   40  41 )
     ( "->"	  calcFunc-evalto   40  -1 )
     ( "=>" 	  calcFunc-evalto   40  41 )
     ( "=>" 	  calcFunc-evalto   40  -1 )))

(put 'eqn 'math-function-table
  '( ( arc\ cos	   . calcFunc-arccos )
     ( arc\ cosh   . calcFunc-arccosh )
     ( arc\ sin	   . calcFunc-arcsin )
     ( arc\ sinh   . calcFunc-arcsinh )
     ( arc\ tan	   . calcFunc-arctan )
     ( arc\ tanh   . calcFunc-arctanh )
     ( GAMMA	   . calcFunc-gamma )
     ( phi	   . calcFunc-totient )
     ( mu	   . calcFunc-moebius )
     ( matrix	   . (math-parse-eqn-matrix) )))

(put 'eqn 'math-variable-table
  '( ( inf	   . var-uinf )))

(put 'eqn 'math-complex-format 'i)

(defun math-parse-eqn-matrix (f sym)
  (let ((vec nil))
    (while (assoc math-expr-data '(("ccol") ("lcol") ("rcol")))
      (math-read-token)
      (or (equal math-expr-data calc-function-open)
	  (throw 'syntax "Expected `{'"))
      (math-read-token)
      (setq vec (cons (cons 'vec (math-read-expr-list)) vec))
      (or (equal math-expr-data calc-function-close)
	  (throw 'syntax "Expected `}'"))
      (math-read-token))
    (or (equal math-expr-data calc-function-close)
	(throw 'syntax "Expected `}'"))
    (math-read-token)
    (math-transpose (cons 'vec (nreverse vec)))))

(defun math-parse-eqn-prime (x sym)
  (if (eq (car-safe x) 'var)
      (if (equal math-expr-data calc-function-open)
	  (progn
	    (math-read-token)
	    (let ((args (if (or (equal math-expr-data calc-function-close)
				(eq math-exp-token 'end))
			    nil
			  (math-read-expr-list))))
	      (if (not (or (equal math-expr-data calc-function-close)
			   (eq math-exp-token 'end)))
		  (throw 'syntax "Expected `)'"))
	      (math-read-token)
	      (cons (intern (format "calcFunc-%s'" (nth 1 x))) args)))
	(list 'var
	      (intern (concat (symbol-name (nth 1 x)) "'"))
	      (intern (concat (symbol-name (nth 2 x)) "'"))))
    (list 'calcFunc-Prime x)))


(defun calc-mathematica-language ()
  (interactive)
  (calc-wrapper
   (calc-set-language 'math)
   (message "Mathematica language mode")))

(put 'math 'math-oper-table
  '( ( "[["    (math-read-math-subscr) 250 -1 )
     ( "!"     calcFunc-fact  210 -1 )
     ( "!!"    calcFunc-dfact 210 -1 )
     ( "^"     ^	     201 200 )
     ( "u+"    ident	     -1  197 )
     ( "u-"    neg	     -1  197 )
     ( "/"     /	     195 196 )
     ( "*"     *	     190 191 )
     ( "2x"    *	     190 191 )
     ( "+"     +	     180 181 )
     ( "-"     -	     180 181 )
     ( "<"     calcFunc-lt   160 161 )
     ( ">"     calcFunc-gt   160 161 )
     ( "<="    calcFunc-leq  160 161 )
     ( ">="    calcFunc-geq  160 161 )
     ( "=="    calcFunc-eq   150 151 )
     ( "!="    calcFunc-neq  150 151 )
     ( "u!"    calcFunc-lnot -1  121 )
     ( "&&"    calcFunc-land 110 111 )
     ( "||"    calcFunc-lor  100 101 )
     ( "!!!"   calcFunc-pnot  -1  85 )
     ( "&&&"   calcFunc-pand  80  81 )
     ( "|||"   calcFunc-por   75  76 )
     ( ":="    calcFunc-assign 51 50 )
     ( "="     calcFunc-assign 51 50 )
     ( "->"    calcFunc-assign 51 50 )
     ( ":>"    calcFunc-assign 51 50 )
     ( "::"    calcFunc-condition 45 46 )
))

(put 'math 'math-function-table
  '( ( Abs	   . calcFunc-abs )
     ( ArcCos	   . calcFunc-arccos )
     ( ArcCosh	   . calcFunc-arccosh )
     ( ArcSin	   . calcFunc-arcsin )
     ( ArcSinh	   . calcFunc-arcsinh )
     ( ArcTan	   . calcFunc-arctan )
     ( ArcTanh	   . calcFunc-arctanh )
     ( Arg	   . calcFunc-arg )
     ( Binomial	   . calcFunc-choose )
     ( Ceiling	   . calcFunc-ceil )
     ( Conjugate   . calcFunc-conj )
     ( Cos	   . calcFunc-cos )
     ( Cosh	   . calcFunc-cosh )
     ( Cot	   . calcFunc-cot )
     ( Coth	   . calcFunc-coth )
     ( Csc	   . calcFunc-csc )
     ( Csch	   . calcFunc-csch )
     ( D	   . calcFunc-deriv )
     ( Dt	   . calcFunc-tderiv )
     ( Det	   . calcFunc-det )
     ( Exp	   . calcFunc-exp )
     ( EulerPhi	   . calcFunc-totient )
     ( Floor	   . calcFunc-floor )
     ( Gamma	   . calcFunc-gamma )
     ( GCD	   . calcFunc-gcd )
     ( If	   . calcFunc-if )
     ( Im	   . calcFunc-im )
     ( Inverse	   . calcFunc-inv )
     ( Integrate   . calcFunc-integ )
     ( Join	   . calcFunc-vconcat )
     ( LCM	   . calcFunc-lcm )
     ( Log	   . calcFunc-ln )
     ( Max	   . calcFunc-max )
     ( Min	   . calcFunc-min )
     ( Mod	   . calcFunc-mod )
     ( MoebiusMu   . calcFunc-moebius )
     ( Random	   . calcFunc-random )
     ( Round	   . calcFunc-round )
     ( Re	   . calcFunc-re )
     ( Sec	   . calcFunc-sec )
     ( Sech	   . calcFunc-sech )
     ( Sign	   . calcFunc-sign )
     ( Sin	   . calcFunc-sin )
     ( Sinh	   . calcFunc-sinh )
     ( Sqrt	   . calcFunc-sqrt )
     ( Tan	   . calcFunc-tan )
     ( Tanh	   . calcFunc-tanh )
     ( Transpose   . calcFunc-trn )
     ( Length	   . calcFunc-vlen )
))

(put 'math 'math-variable-table
  '( ( I	   . var-i )
     ( Pi	   . var-pi )
     ( E	   . var-e )
     ( GoldenRatio . var-phi )
     ( EulerGamma  . var-gamma )
     ( Infinity	   . var-inf )
     ( ComplexInfinity . var-uinf )
     ( Indeterminate . var-nan )
))

(put 'math 'math-vector-brackets "{}")
(put 'math 'math-complex-format 'I)
(put 'math 'math-function-open "[")
(put 'math 'math-function-close "]")

(put 'math 'math-radix-formatter
     (function (lambda (r s) (format "%d^^%s" r s))))

(defun math-read-math-subscr (x op)
  (let ((idx (math-read-expr-level 0)))
    (or (and (equal math-expr-data "]")
	     (progn
	       (math-read-token)
	       (equal math-expr-data "]")))
	(throw 'syntax "Expected ']]'"))
    (math-read-token)
    (list 'calcFunc-subscr x idx)))


(defun calc-maple-language ()
  (interactive)
  (calc-wrapper
   (calc-set-language 'maple)
   (message "Maple language mode")))

(put 'maple 'math-oper-table
  '( ( "matrix" ident	     -1  300 )
     ( "MATRIX" ident	     -1  300 )
     ( "!"     calcFunc-fact  210 -1 )
     ( "^"     ^	     201 200 )
     ( "**"    ^	     201 200 )
     ( "u+"    ident	     -1  197 )
     ( "u-"    neg	     -1  197 )
     ( "/"     /	     191 192 )
     ( "*"     *	     191 192 )
     ( "intersect" calcFunc-vint 191 192 )
     ( "+"     +	     180 181 )
     ( "-"     -	     180 181 )
     ( "union" calcFunc-vunion 180 181 )
     ( "minus" calcFunc-vdiff 180 181 )
     ( "mod"   %	     170 170 )
     ( ".."    (math-read-maple-dots) 165 165 )
     ( "\\dots" (math-read-maple-dots) 165 165 )
     ( "<"     calcFunc-lt   160 160 )
     ( ">"     calcFunc-gt   160 160 )
     ( "<="    calcFunc-leq  160 160 )
     ( ">="    calcFunc-geq  160 160 )
     ( "="     calcFunc-eq   160 160 )
     ( "<>"    calcFunc-neq  160 160 )
     ( "not"   calcFunc-lnot -1  121 )
     ( "and"   calcFunc-land 110 111 )
     ( "or"    calcFunc-lor  100 101 )
     ( "!!!"   calcFunc-pnot  -1  85 )
     ( "&&&"   calcFunc-pand  80  81 )
     ( "|||"   calcFunc-por   75  76 )
     ( ":="    calcFunc-assign 51 50 )
     ( "::"    calcFunc-condition 45 46 )
))

(put 'maple 'math-function-table
  '( ( bernoulli   . calcFunc-bern )
     ( binomial	   . calcFunc-choose )
     ( diff	   . calcFunc-deriv )
     ( GAMMA	   . calcFunc-gamma )
     ( ifactor	   . calcFunc-prfac )
     ( igcd 	   . calcFunc-gcd )
     ( ilcm	   . calcFunc-lcm )
     ( int  	   . calcFunc-integ )
     ( modp	   . % )
     ( irem	   . % )
     ( iquo	   . calcFunc-idiv )
     ( isprime	   . calcFunc-prime )
     ( length	   . calcFunc-vlen )
     ( member	   . calcFunc-in )
     ( crossprod   . calcFunc-cross )
     ( inverse	   . calcFunc-inv )
     ( trace	   . calcFunc-tr )
     ( transpose   . calcFunc-trn )
     ( vectdim	   . calcFunc-vlen )
))

(put 'maple 'math-variable-table
  '( ( I	   . var-i )
     ( Pi	   . var-pi )
     ( E	   . var-e )
     ( infinity	   . var-inf )
     ( infinity    . var-uinf )
     ( infinity    . var-nan )
))

(put 'maple 'math-complex-format 'I)

(defun math-read-maple-dots (x op)
  (list 'intv 3 x (math-read-expr-level (nth 3 op))))


;; The variable math-read-big-lines is local to math-read-big-expr in
;; calc-ext.el, but is used by math-read-big-rec, math-read-big-char,
;; math-read-big-emptyp, math-read-big-error and math-read-big-balance,
;; which are called (directly and indirectly) by math-read-big-expr.
;; It is also local to math-read-big-bigp in calc-ext.el, which calls
;; math-read-big-balance.
(defvar math-read-big-lines)

;; The variables math-read-big-baseline and math-read-big-h2 are
;; local to math-read-big-expr in calc-ext.el, but used by
;; math-read-big-rec.
(defvar math-read-big-baseline)
(defvar math-read-big-h2)

;; The variables math-rb-h1, math-rb-h2, math-rb-v1 and math-rb-v2 
;; are local to math-read-big-rec, but are used by math-read-big-char, 
;; math-read-big-emptyp and math-read-big-balance which are called by 
;; math-read-big-rec.
;; math-rb-h2 is also local to math-read-big-bigp in calc-ext.el,
;; which calls math-read-big-balance.
(defvar math-rb-h1)
(defvar math-rb-h2)
(defvar math-rb-v1)
(defvar math-rb-v2)

(defun math-read-big-rec (math-rb-h1 math-rb-v1 math-rb-h2 math-rb-v2 
                                     &optional baseline prec short)
  (or prec (setq prec 0))

  ;; Clip whitespace above or below.
  (while (and (< math-rb-v1 math-rb-v2) 
              (math-read-big-emptyp math-rb-h1 math-rb-v1 math-rb-h2 (1+ math-rb-v1)))
    (setq math-rb-v1 (1+ math-rb-v1)))
  (while (and (< math-rb-v1 math-rb-v2) 
              (math-read-big-emptyp math-rb-h1 (1- math-rb-v2) math-rb-h2 math-rb-v2))
    (setq math-rb-v2 (1- math-rb-v2)))

  ;; If formula is a single line high, normal parser can handle it.
  (if (<= math-rb-v2 (1+ math-rb-v1))
      (if (or (<= math-rb-v2 math-rb-v1)
	      (> math-rb-h1 (length (setq math-rb-v2 
                                          (nth math-rb-v1 math-read-big-lines)))))
	  (math-read-big-error math-rb-h1 math-rb-v1)
	(setq math-read-big-baseline math-rb-v1
	      math-read-big-h2 math-rb-h2
	      math-rb-v2 (nth math-rb-v1 math-read-big-lines)
	      math-rb-h2 (math-read-expr 
                          (substring math-rb-v2 math-rb-h1 
                                     (min math-rb-h2 (length math-rb-v2)))))
	(if (eq (car-safe math-rb-h2) 'error)
	    (math-read-big-error (+ math-rb-h1 (nth 1 math-rb-h2)) 
                                 math-rb-v1 (nth 2 math-rb-h2))
	  math-rb-h2))

    ;; Clip whitespace at left or right.
    (while (and (< math-rb-h1 math-rb-h2) 
                (math-read-big-emptyp math-rb-h1 math-rb-v1 (1+ math-rb-h1) math-rb-v2))
      (setq math-rb-h1 (1+ math-rb-h1)))
    (while (and (< math-rb-h1 math-rb-h2) 
                (math-read-big-emptyp (1- math-rb-h2) math-rb-v1 math-rb-h2 math-rb-v2))
      (setq math-rb-h2 (1- math-rb-h2)))

    ;; Scan to find widest left-justified "----" in the region.
    (let* ((widest nil)
	   (widest-h2 0)
	   (lines-v1 (nthcdr math-rb-v1 math-read-big-lines))
	   (p lines-v1)
	   (v math-rb-v1)
	   (other-v nil)
	   other-char line len h)
      (while (< v math-rb-v2)
	(setq line (car p)
	      len (min math-rb-h2 (length line)))
	(and (< math-rb-h1 len)
	     (/= (aref line math-rb-h1) ?\ )
	     (if (and (= (aref line math-rb-h1) ?\-)
		      ;; Make sure it's not a minus sign.
		      (or (and (< (1+ math-rb-h1) len) 
                               (= (aref line (1+ math-rb-h1)) ?\-))
			  (/= (math-read-big-char math-rb-h1 (1- v)) ?\ )
			  (/= (math-read-big-char math-rb-h1 (1+ v)) ?\ )))
		 (progn
		   (setq h math-rb-h1)
		   (while (and (< (setq h (1+ h)) len)
			       (= (aref line h) ?\-)))
		   (if (> h widest-h2)
		       (setq widest v
			     widest-h2 h)))
	       (or other-v (setq other-v v other-char (aref line math-rb-h1)))))
	(setq v (1+ v)
	      p (cdr p)))

      (cond ((not (setq v other-v))
	     (math-read-big-error math-rb-h1 math-rb-v1))   ; Should never happen!

	    ;; Quotient.
	    (widest
	     (setq h widest-h2
		   v widest)
	     (let ((num (math-read-big-rec math-rb-h1 math-rb-v1 h v))
		   (den (math-read-big-rec math-rb-h1 (1+ v) h math-rb-v2)))
	       (setq p (if (and (math-integerp num) (math-integerp den))
			   (math-make-frac num den)
			 (list '/ num den)))))

	    ;; Big radical sign.
	    ((= other-char ?\\)
	     (or (= (math-read-big-char (1+ math-rb-h1) v) ?\|)
		 (math-read-big-error (1+ math-rb-h1) v "Malformed root sign"))
	     (math-read-big-emptyp math-rb-h1 math-rb-v1 (1+ math-rb-h1) v nil t)
	     (while (= (math-read-big-char (1+ math-rb-h1) (setq v (1- v))) ?\|))
	     (or (= (math-read-big-char (setq h (+ math-rb-h1 2)) v) ?\_)
		 (math-read-big-error h v "Malformed root sign"))
	     (while (= (math-read-big-char (setq h (1+ h)) v) ?\_))
	     (math-read-big-emptyp math-rb-h1 math-rb-v1 (1+ math-rb-h1) v nil t)
	     (math-read-big-emptyp math-rb-h1 (1+ other-v) h math-rb-v2 nil t)
	     (setq p (list 'calcFunc-sqrt (math-read-big-rec
					   (+ math-rb-h1 2) (1+ v)
					   h (1+ other-v) baseline))
		   v math-read-big-baseline))

	    ;; Small radical sign.
	    ((and (= other-char ?V)
		  (= (math-read-big-char (1+ math-rb-h1) (1- v)) ?\_))
	     (setq h (1+ math-rb-h1))
	     (math-read-big-emptyp math-rb-h1 math-rb-v1 h (1- v) nil t)
	     (math-read-big-emptyp math-rb-h1 (1+ v) h math-rb-v2 nil t)
	     (math-read-big-emptyp math-rb-h1 math-rb-v1 (1+ math-rb-h1) v nil t)
	     (while (= (math-read-big-char (setq h (1+ h)) (1- v)) ?\_))
	     (setq p (list 'calcFunc-sqrt (math-read-big-rec
					   (1+ math-rb-h1) v h (1+ v) t))
		   v math-read-big-baseline))

	    ;; Binomial coefficient.
	    ((and (= other-char ?\()
		  (= (math-read-big-char (1+ math-rb-h1) v) ?\ )
		  (= (string-match "( *)" (nth v math-read-big-lines) 
                                   math-rb-h1) math-rb-h1))
	     (setq h (match-end 0))
	     (math-read-big-emptyp math-rb-h1 math-rb-v1 (1+ math-rb-h1) v nil t)
	     (math-read-big-emptyp math-rb-h1 (1+ v) (1+ math-rb-h1) math-rb-v2 nil t)
	     (math-read-big-emptyp (1- h) math-rb-v1 h v nil t)
	     (math-read-big-emptyp (1- h) (1+ v) h math-rb-v2 nil t)
	     (setq p (list 'calcFunc-choose
			   (math-read-big-rec (1+ math-rb-h1) math-rb-v1 (1- h) v)
			   (math-read-big-rec (1+ math-rb-h1) (1+ v)
					      (1- h) math-rb-v2))))

	    ;; Minus sign.
	    ((= other-char ?\-)
	     (setq p (list 'neg (math-read-big-rec (1+ math-rb-h1) math-rb-v1 
                                                   math-rb-h2 math-rb-v2 v 250 t))
		   v math-read-big-baseline
		   h math-read-big-h2))

	    ;; Parentheses.
	    ((= other-char ?\()
	     (math-read-big-emptyp math-rb-h1 math-rb-v1 (1+ math-rb-h1) v nil t)
	     (math-read-big-emptyp math-rb-h1 (1+ v) (1+ math-rb-h1) math-rb-v2 nil t)
	     (setq h (math-read-big-balance (1+ math-rb-h1) v "(" t))
	     (math-read-big-emptyp (1- h) math-rb-v1 h v nil t)
	     (math-read-big-emptyp (1- h) (1+ v) h math-rb-v2 nil t)
	     (let ((sep (math-read-big-char (1- h) v))
		   hmid)
	       (if (= sep ?\.)
		   (setq h (1+ h)))
	       (if (= sep ?\])
		   (math-read-big-error (1- h) v "Expected `)'"))
	       (if (= sep ?\))
		   (setq p (math-read-big-rec 
                            (1+ math-rb-h1) math-rb-v1 (1- h) math-rb-v2 v))
		 (setq hmid (math-read-big-balance h v "(")
		       p (list p 
                               (math-read-big-rec h math-rb-v1 (1- hmid) math-rb-v2 v))
		       h hmid)
		 (cond ((= sep ?\.)
			(setq p (cons 'intv (cons (if (= (math-read-big-char
							  (1- h) v)
							 ?\))
						      0 1)
						  p))))
		       ((= (math-read-big-char (1- h) v) ?\])
			(math-read-big-error (1- h) v "Expected `)'"))
		       ((= sep ?\,)
			(or (and (math-realp (car p)) (math-realp (nth 1 p)))
			    (math-read-big-error
			     math-rb-h1 v "Complex components must be real"))
			(setq p (cons 'cplx p)))
		       ((= sep ?\;)
			(or (and (math-realp (car p)) (math-anglep (nth 1 p)))
			    (math-read-big-error
			     math-rb-h1 v "Complex components must be real"))
			(setq p (cons 'polar p)))))))

	    ;; Matrix.
	    ((and (= other-char ?\[)
		  (or (= (math-read-big-char (setq h math-rb-h1) (1+ v)) ?\[)
		      (= (math-read-big-char (setq h (1+ h)) v) ?\[)
		      (and (= (math-read-big-char h v) ?\ )
			   (= (math-read-big-char (setq h (1+ h)) v) ?\[)))
		  (= (math-read-big-char h (1+ v)) ?\[))
	     (math-read-big-emptyp math-rb-h1 math-rb-v1 h v nil t)
	     (let ((vtop v)
		   (hleft h)
		   (hright nil))
	       (setq p nil)
	       (while (progn
			(setq h (math-read-big-balance (1+ hleft) v "["))
			(if hright
			    (or (= h hright)
				(math-read-big-error hright v "Expected `]'"))
			  (setq hright h))
			(setq p (cons (math-read-big-rec
				       hleft v h (1+ v)) p))
			(and (memq (math-read-big-char h v) '(?\  ?\,))
			     (= (math-read-big-char hleft (1+ v)) ?\[)))
		 (setq v (1+ v)))
	       (or (= hleft math-rb-h1)
		   (progn
		     (if (= (math-read-big-char h v) ?\ )
			 (setq h (1+ h)))
		     (and (= (math-read-big-char h v) ?\])
			  (setq h (1+ h))))
		   (math-read-big-error (1- h) v "Expected `]'"))
	       (if (= (math-read-big-char h vtop) ?\,)
		   (setq h (1+ h)))
	       (math-read-big-emptyp math-rb-h1 (1+ v) (1- h) math-rb-v2 nil t)
	       (setq v (+ vtop (/ (- v vtop) 2))
		     p (cons 'vec (nreverse p)))))

	    ;; Square brackets.
	    ((= other-char ?\[)
	     (math-read-big-emptyp math-rb-h1 math-rb-v1 (1+ math-rb-h1) v nil t)
	     (math-read-big-emptyp math-rb-h1 (1+ v) (1+ math-rb-h1) math-rb-v2 nil t)
	     (setq p nil
		   h (1+ math-rb-h1))
	     (while (progn
		      (setq widest (math-read-big-balance h v "[" t))
		      (math-read-big-emptyp (1- h) math-rb-v1 h v nil t)
		      (math-read-big-emptyp (1- h) (1+ v) h math-rb-v2 nil t)
		      (setq p (cons (math-read-big-rec
				     h math-rb-v1 (1- widest) math-rb-v2 v) p)
			    h widest)
		      (= (math-read-big-char (1- h) v) ?\,)))
	     (setq widest (math-read-big-char (1- h) v))
	     (if (or (memq widest '(?\; ?\)))
		     (and (eq widest ?\.) (cdr p)))
		 (math-read-big-error (1- h) v "Expected `]'"))
	     (if (= widest ?\.)
		 (setq h (1+ h)
		       widest (math-read-big-balance h v "[")
		       p (nconc p (list (math-read-big-rec
					 h math-rb-v1 (1- widest) math-rb-v2 v)))
		       h widest
		       p (cons 'intv (cons (if (= (math-read-big-char (1- h) v)
						  ?\])
					       3 2)
					   p)))
	       (setq p (cons 'vec (nreverse p)))))

	    ;; Date form.
	    ((= other-char ?\<)
	     (setq line (nth v math-read-big-lines))
	     (string-match ">" line math-rb-h1)
	     (setq h (match-end 0))
	     (math-read-big-emptyp math-rb-h1 math-rb-v1 h v nil t)
	     (math-read-big-emptyp math-rb-h1 (1+ v) h math-rb-v2 nil t)
	     (setq p (math-read-big-rec math-rb-h1 v h (1+ v) v)))

	    ;; Variable name or function call.
	    ((or (and (>= other-char ?a) (<= other-char ?z))
		 (and (>= other-char ?A) (<= other-char ?Z)))
	     (setq line (nth v math-read-big-lines))
	     (string-match "\\([a-zA-Z'_]+\\) *" line math-rb-h1)
	     (setq h (match-end 1)
		   widest (match-end 0)
		   p (math-match-substring line 1))
	     (math-read-big-emptyp math-rb-h1 math-rb-v1 h v nil t)
	     (math-read-big-emptyp math-rb-h1 (1+ v) h math-rb-v2 nil t)
	     (if (= (math-read-big-char widest v) ?\()
		 (progn
		   (setq line (if (string-match "-" p)
				  (intern p)
				(intern (concat "calcFunc-" p)))
			 h (1+ widest)
			 p nil)
		   (math-read-big-emptyp widest math-rb-v1 h v nil t)
		   (math-read-big-emptyp widest (1+ v) h math-rb-v2 nil t)
		   (while (progn
			    (setq widest (math-read-big-balance h v "(" t))
			    (math-read-big-emptyp (1- h) math-rb-v1 h v nil t)
			    (math-read-big-emptyp (1- h) (1+ v) h math-rb-v2 nil t)
			    (setq p (cons (math-read-big-rec
					   h math-rb-v1 (1- widest) math-rb-v2 v) p)
				  h widest)
			    (= (math-read-big-char (1- h) v) ?\,)))
		   (or (= (math-read-big-char (1- h) v) ?\))
		       (math-read-big-error (1- h) v "Expected `)'"))
		   (setq p (cons line (nreverse p))))
	       (setq p (list 'var
			     (intern (math-remove-dashes p))
			     (if (string-match "-" p)
				 (intern p)
			       (intern (concat "var-" p)))))))

	    ;; Number.
	    (t
	     (setq line (nth v math-read-big-lines))
	     (or (= (string-match "_?\\([0-9]+.?0*@ *\\)?\\([0-9]+.?0*' *\\)?\\([0-9]+\\(#\\|\\^\\^\\)[0-9a-zA-Z:]+\\|[0-9]+:[0-9:]+\\|[0-9.]+\\([eE][-+_]?[0-9]+\\)?\"?\\)?" line math-rb-h1) math-rb-h1)
		 (math-read-big-error h v "Expected a number"))
	     (setq h (match-end 0)
		   p (math-read-number (math-match-substring line 0)))
	     (math-read-big-emptyp math-rb-h1 math-rb-v1 h v nil t)
	     (math-read-big-emptyp math-rb-h1 (1+ v) h math-rb-v2 nil t)))

      ;; Now left term is bounded by math-rb-h1, math-rb-v1, h, math-rb-v2; 
      ;; baseline = v.
      (if baseline
	  (or (= v baseline)
	      (math-read-big-error math-rb-h1 v "Inconsistent baseline in formula"))
	(setq baseline v))

      ;; Look for superscripts or subscripts.
      (setq line (nth baseline math-read-big-lines)
	    len (min math-rb-h2 (length line))
	    widest h)
      (while (and (< widest len)
		  (= (aref line widest) ?\ ))
	(setq widest (1+ widest)))
      (and (>= widest len) (setq widest math-rb-h2))
      (if (math-read-big-emptyp h v widest math-rb-v2)
	  (if (math-read-big-emptyp h math-rb-v1 widest v)
	      (setq h widest)
	    (setq p (list '^ p (math-read-big-rec h math-rb-v1 widest v))
		  h widest))
	  (if (math-read-big-emptyp h math-rb-v1 widest v)
	      (setq p (list 'calcFunc-subscr p
			    (math-read-big-rec h v widest math-rb-v2))
		    h widest)))

      ;; Look for an operator name and grab additional terms.
      (while (and (< h len)
		  (if (setq widest (and (math-read-big-emptyp
					 h math-rb-v1 (1+ h) v)
					(math-read-big-emptyp
					 h (1+ v) (1+ h) math-rb-v2)
					(string-match "<=\\|>=\\|\\+/-\\|!=\\|&&\\|||\\|:=\\|=>\\|." line h)
					(assoc (math-match-substring line 0)
					       math-standard-opers)))
		      (and (>= (nth 2 widest) prec)
			   (setq h (match-end 0)))
		    (and (not (eq (string-match ",\\|;\\|\\.\\.\\|)\\|\\]\\|:" line h)
				  h))
			 (setq widest '("2x" * 196 195)))))
	(cond ((eq (nth 3 widest) -1)
	       (setq p (list (nth 1 widest) p)))
	      ((equal (car widest) "?")
	       (let ((y (math-read-big-rec h math-rb-v1 math-rb-h2 
                                           math-rb-v2 baseline nil t)))
		 (or (= (math-read-big-char math-read-big-h2 baseline) ?\:)
		     (math-read-big-error math-read-big-h2 baseline "Expected `:'"))
		 (setq p (list (nth 1 widest) p y
			       (math-read-big-rec 
                                (1+ math-read-big-h2) math-rb-v1 math-rb-h2 math-rb-v2
                                baseline (nth 3 widest) t))
		       h math-read-big-h2)))
	      (t
	       (setq p (list (nth 1 widest) p
			     (math-read-big-rec h math-rb-v1 math-rb-h2 math-rb-v2
						baseline (nth 3 widest) t))
		     h math-read-big-h2))))

      ;; Return all relevant information to caller.
      (setq math-read-big-baseline baseline
	    math-read-big-h2 h)
      (or short (= math-read-big-h2 math-rb-h2)
	  (math-read-big-error h baseline))
      p)))

(defun math-read-big-char (h v)
  (or (and (>= h math-rb-h1)
	   (< h math-rb-h2)
	   (>= v math-rb-v1)
	   (< v math-rb-v2)
	   (let ((line (nth v math-read-big-lines)))
	     (and line
		  (< h (length line))
		  (aref line h))))
      ?\ ))

(defun math-read-big-emptyp (eh1 ev1 eh2 ev2 &optional what error)
  (and (< ev1 math-rb-v1) (setq ev1 math-rb-v1))
  (and (< eh1 math-rb-h1) (setq eh1 math-rb-h1))
  (and (> ev2 math-rb-v2) (setq ev2 math-rb-v2))
  (and (> eh2 math-rb-h2) (setq eh2 math-rb-h2))
  (or what (setq what ?\ ))
  (let ((p (nthcdr ev1 math-read-big-lines))
	h)
    (while (and (< ev1 ev2)
		(progn
		  (setq h (min eh2 (length (car p))))
		  (while (and (>= (setq h (1- h)) eh1)
			      (= (aref (car p) h) what)))
		  (and error (>= h eh1)
		       (math-read-big-error h ev1 (if (stringp error)
						      error
						    "Whitespace expected")))
		  (< h eh1)))
      (setq ev1 (1+ ev1)
	    p (cdr p)))
    (>= ev1 ev2)))

;; math-read-big-err-msg is local to math-read-big-expr in calc-ext.el,
;; but is used by math-read-big-error which is called (indirectly) by
;; math-read-big-expr.
(defvar math-read-big-err-msg)

(defun math-read-big-error (h v &optional msg)
  (let ((pos 0)
	(p math-read-big-lines))
    (while (> v 0)
      (setq pos (+ pos 1 (length (car p)))
	    p (cdr p)
	    v (1- v)))
    (setq h (+ pos (min h (length (car p))))
	  math-read-big-err-msg (list 'error h (or msg "Syntax error")))
    (throw 'syntax nil)))

(defun math-read-big-balance (h v what &optional commas)
  (let* ((line (nth v math-read-big-lines))
	 (len (min math-rb-h2 (length line)))
	 (count 1))
    (while (> count 0)
      (if (>= h len)
	  (if what
	      (math-read-big-error nil v (format "Unmatched `%s'" what))
	    (setq count 0))
	(if (memq (aref line h) '(?\( ?\[))
	    (setq count (1+ count))
	  (if (if (and commas (= count 1))
		  (or (memq (aref line h) '(?\) ?\] ?\, ?\;))
		      (and (eq (aref line h) ?\.)
			   (< (1+ h) len)
			   (eq (aref line (1+ h)) ?\.)))
		(memq (aref line h) '(?\) ?\])))
	      (setq count (1- count))))
	(setq h (1+ h))))
    h))

(provide 'calc-lang)

;;; arch-tag: 483bfe15-f290-4fef-bb7d-ce65be687f2e
;;; calc-lang.el ends here
